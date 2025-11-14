package org.codehaus.mojo.versions;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;

/**
 * Attempts to resolve dependency version ranges to the specific version being used in the build. For example a version
 * range of "[1.0, 1.2)" would be resolved to the specific version currently in use "1.1".
 *
 * @author Paul Gier
 * @since 1.0-alpha-3
 */
@Mojo(name = "resolve-ranges", threadSafe = true)
public class ResolveRangesMojo extends AbstractVersionsDependencyUpdaterMojo {
    /**
     * Whether to process the properties section of the project.
     *
     * @since 1.3
     */
    @Parameter(property = "processProperties", defaultValue = "true")
    private boolean processProperties;

    /**
     * A comma separated list of properties to update if they contain version-ranges.
     *
     * @parameter property="includeProperties"
     * @since 1.3
     */
    @Parameter(property = "includeProperties")
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not update even if they contain version-ranges.
     *
     * @since 1.3
     */
    @Parameter(property = "excludeProperties")
    private String excludeProperties = null;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.5
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    private boolean allowMajorUpdates;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.5
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    private boolean allowMinorUpdates;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.5
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    private boolean allowIncrementalUpdates;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter(property = "processParent", defaultValue = "false")
    private boolean processParent = false;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a version range. For example 1.0-20090128.202731-1
     */
    private final Pattern matchRangeRegex = Pattern.compile(",");

    // ------------------------------ METHODS --------------------------

    /**
     * Creates a new instance.
     *
     * @param artifactFactory   the artifact factory
     * @param repositorySystem  the repository system
     * @param wagonMap          the map of wagon implementations
     * @param changeRecorderFactories   the change recorder factories
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public ResolveRangesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected boolean getProcessDependencies() {
        return processDependencies;
    }

    @Override
    protected boolean getProcessDependencyManagement() {
        return processDependencyManagement;
    }

    @Override
    public boolean getProcessParent() {
        return processParent;
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        // Note we have to getModel the dependencies from the model because the dependencies in the
        // project may have already had their range resolved [MNG-4138]
        if (hasDependencyManagement() && hasDependenciesInDependencyManagement() && getProcessDependencyManagement()) {
            getLog().debug("processing dependencyManagement of " + getProject().getId());
            resolveRanges(pom, getProject().getModel().getDependencyManagement().getDependencies());
        }
        if (getProject().getDependencies() != null && getProcessDependencies()) {
            getLog().debug("processing dependencies of " + getProject().getId());
            resolveRanges(pom, getProject().getModel().getDependencies());
        }
        if (hasParent() && getProcessParent()) {
            getLog().debug("processing parent " + getProject().getId());
            resolveRangesInParent(pom);
        }
        if (processProperties) {
            getLog().debug("processing properties of " + getProject().getId());
            resolvePropertyRanges(pom);
        }
    }

    private boolean hasParent() {
        return getProject().getModel().getParent() != null;
    }

    private boolean hasDependenciesInDependencyManagement() {
        return getProject().getModel().getDependencyManagement().getDependencies() != null;
    }

    private boolean hasDependencyManagement() {
        return getProject().getModel().getDependencyManagement() != null;
    }

    private void resolveRangesInParent(MutableXMLStreamReader pom)
            throws MojoExecutionException, VersionRetrievalException, XMLStreamException {
        Matcher versionMatcher =
                matchRangeRegex.matcher(getProject().getModel().getParent().getVersion());

        if (versionMatcher.find()) {
            Artifact artifact = this.toArtifact(getProject().getModel().getParent());

            if (artifact != null && isIncluded(artifact)) {
                getLog().debug("Resolving version range for parent: " + artifact);

                String artifactVersion = artifact.getVersion();
                if (artifactVersion == null) {
                    ArtifactVersion latestVersion =
                            findLatestVersion(artifact, artifact.getVersionRange(), allowSnapshots, false);

                    if (latestVersion != null) {
                        artifactVersion = latestVersion.toString();
                    } else {
                        getLog().warn("Not updating version " + artifact + " : could not resolve any versions");
                    }
                }

                if (artifactVersion != null) {
                    if (PomHelper.setProjectParentVersion(pom, artifactVersion)) {
                        getLog().debug("Version set to " + artifactVersion + " for parent: " + artifact);
                    } else {
                        getLog().warn("Could not find the version tag for parent " + artifact + " in project "
                                + getProject().getId() + " so unable to set version to " + artifactVersion);
                    }
                }
            }
        }
    }

    private void resolveRanges(MutableXMLStreamReader pom, Collection<Dependency> dependencies)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {

        for (Dependency dep : dependencies) {
            if (getExcludeReactor() && isProducedByReactor(dep)) {
                continue;
            }

            if (StringUtils.isBlank(dep.getVersion())) {
                getLog().info("Found managed dependency " + toString(dep) + " without a version");
                continue;
            }

            if (isHandledByProperty(dep)) {
                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                continue;
            }

            Matcher versionMatcher = matchRangeRegex.matcher(dep.getVersion());

            if (versionMatcher.find()) {
                Artifact artifact = this.toArtifact(dep);

                if (artifact != null && isIncluded(artifact)) {
                    getLog().debug("Resolving version range for dependency: " + artifact);

                    String artifactVersion = artifact.getVersion();
                    if (artifactVersion == null) {
                        ArtifactVersion latestVersion =
                                findLatestVersion(artifact, artifact.getVersionRange(), allowSnapshots, false);

                        if (latestVersion != null) {
                            artifactVersion = latestVersion.toString();
                        } else {
                            getLog().warn("Not updating version " + artifact + " : could not resolve any versions");
                        }
                    }

                    if (artifactVersion != null) {
                        if (PomHelper.setDependencyVersion(
                                pom,
                                artifact.getGroupId(),
                                artifact.getArtifactId(),
                                dep.getVersion(),
                                artifactVersion,
                                getProject().getModel(),
                                getLog())) {
                            getLog().debug("Version set to " + artifactVersion + " for dependency: " + artifact);
                        } else {
                            getLog().debug("Could not find the version tag for dependency " + artifact + " in project "
                                    + getProject().getId() + " so unable to set version to "
                                    + artifactVersion);
                        }
                    }
                }
            }
        }
    }

    private void resolvePropertyRanges(MutableXMLStreamReader pom) throws XMLStreamException, MojoExecutionException {

        if (includeProperties == null) {
            Properties originalProperties = getProject().getOriginalModel().getProperties();
            includeProperties = String.join(",", originalProperties.stringPropertyNames());
        }
        Map<Property, PropertyVersions> propertyVersions = this.getHelper()
                .getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                        .withMavenProject(getProject())
                        .withIncludeProperties(includeProperties)
                        .withExcludeProperties(excludeProperties)
                        .build());
        for (Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet()) {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            final String currentVersion = getProject().getProperties().getProperty(property.getName());
            if (currentVersion == null
                    || !matchRangeRegex.matcher(currentVersion).find()) {
                continue;
            }

            property.setVersion(currentVersion);

            Log log = getLog();
            if (log != null && !allowIncrementalUpdates) {
                log.info("Assuming allowMinorUpdates false because allowIncrementalUpdates is false.");
            }

            if (log != null && !allowMinorUpdates) {
                log.info("Assuming allowMajorUpdates false because allowMinorUpdates is false.");
            }

            Optional<Segment> unchangedSegment1 = allowMajorUpdates && allowMinorUpdates && allowIncrementalUpdates
                    ? empty()
                    : allowMinorUpdates && allowIncrementalUpdates
                            ? of(MAJOR)
                            : allowIncrementalUpdates ? of(MINOR) : of(INCREMENTAL);
            if (log != null && log.isDebugEnabled()) {
                log.debug(unchangedSegment1
                                .map(Segment::minorTo)
                                .map(Segment::toString)
                                .orElse("ALL") + " version changes allowed");
            }
            // TODO: Check if we could add allowDowngrade ?
            try {
                updatePropertyToNewestVersion(pom, property, version, currentVersion, false, unchangedSegment1);
            } catch (InvalidSegmentException | InvalidVersionSpecificationException e) {
                getLog().warn(String.format(
                        "Skipping the processing of %s:%s due to: %s",
                        property.getName(), property.getVersion(), e.getMessage()));
            }
        }
    }
}
