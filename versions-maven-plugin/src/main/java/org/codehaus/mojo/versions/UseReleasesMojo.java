package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.DEPENDENCY_MANAGEMENT_UPDATE;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.DEPENDENCY_UPDATE;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.PARENT_UPDATE;

/**
 * Replaces any -SNAPSHOT versions with the corresponding release version (if it has been released).
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo(name = "use-releases", threadSafe = true)
public class UseReleasesMojo extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * <p>When set to {@code true}, will use the highest found release version matching a string
     * starting with the current version string without {@code -SNAPSHOT}.</p>
     * <p>For example, if the current version is {@code 1.1-SNAPSHOT}, will match all versions
     * starting with {@code 1.1}, i.e. {@code 1.1}, {@code 1.1.1}, {@code 1.1.2}, {@code 1.1.3.1}, etc.,
     * and will select the highest found version, i.e. {@code 1.1.3.1}.</p>
     *
     * @since 2.3
     */
    @Parameter(property = "allowRangeMatching", defaultValue = "false")
    private boolean allowRangeMatching;

    /**
     * Whether to fail if a SNAPSHOT could not be replaced
     *
     * @since 2.3
     */
    @Parameter(property = "failIfNotReplaced", defaultValue = "false")
    protected boolean failIfNotReplaced;

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
        return false;
    }

    // ------------------------------ METHODS --------------------------

    /**
     * Creates a new instance.
     *
     * @param artifactFactory   the artifact factory
     * @param repositorySystem  the repository system
     * @param wagonMap          the map of wagon implementations
     * @param changeRecorders   the change recorders
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public UseReleasesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        List<DependencyVersionChange> versionChanges = new ArrayList<>();
        if (getProcessDependencyManagement()) {
            try {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    versionChanges.addAll(getReleaseVersionChanges(
                            dependencyManagement.getDependencies(), DEPENDENCY_MANAGEMENT_UPDATE));
                }
            } catch (IOException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            }
        }
        if (getProject().getDependencies() != null && getProcessDependencies()) {
            versionChanges.addAll(getReleaseVersionChanges(getProject().getDependencies(), DEPENDENCY_UPDATE));
        }
        if (getProject().getParent() != null && getProcessParent()) {
            versionChanges.addAll(getReleaseVersionChanges(singletonList(getParentDependency()), PARENT_UPDATE));
        }
        for (DependencyVersionChange change : versionChanges) {
            updateDependencyVersion(pom, change);
        }
    }

    private List<DependencyVersionChange> getReleaseVersionChanges(
            Collection<Dependency> dependencies, DependencyChangeKind dependencyChangeKind)
            throws MojoExecutionException, VersionRetrievalException {
        List<DependencyVersionChange> versionChanges = new ArrayList<>();
        for (Dependency dep : dependencies) {
            if (getExcludeReactor() && isProducedByReactor(dep)) {
                getLog().info("Ignoring reactor dependency: " + toString(dep));
                continue;
            }

            if (isHandledByProperty(dep)) {
                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                continue;
            }

            String version = dep.getVersion();
            if (version == null) {
                getLog().info("Ignoring dependency with no version: " + toString(dep));
                continue;
            }
            Matcher versionMatcher = SNAPSHOT_REGEX.matcher(version);
            if (versionMatcher.matches()) {
                String releaseVersion = versionMatcher.group(1);
                Artifact artifact = toArtifact(dep);
                if (!isIncluded(artifact)) {
                    continue;
                }

                getLog().debug("Looking for a release of " + toString(dep));
                // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
                // retrieve release version if provided snapshot version.
                artifact.setVersion(releaseVersion);
                Optional<String> targetVersion =
                        findReleaseVersion(releaseVersion, getHelper().lookupArtifactVersions(artifact, false));

                if (targetVersion.isPresent()) {
                    versionChanges.add(new DependencyVersionChange()
                            .withKind(dependencyChangeKind)
                            .withGroupId(artifact.getGroupId())
                            .withArtifactId(artifact.getArtifactId())
                            .withOldVersion(version)
                            .withNewVersion(targetVersion.get()));
                } else {
                    getLog().info("No matching release of " + toString(dep) + " to update.");
                    if (failIfNotReplaced) {
                        throw new MojoExecutionException(
                                "No matching release of " + toString(dep) + " found for update");
                    }
                }
            }
        }

        return versionChanges;
    }

    private Optional<String> findReleaseVersion(String releaseVersion, ArtifactVersions versions) {
        return !allowRangeMatching
                ? versions.containsVersion(releaseVersion) ? Optional.of(releaseVersion) : Optional.empty()
                : Arrays.stream(versions.getVersions(false))
                        .sorted((v1, v2) -> -(v1.compareTo(v2)))
                        .filter(v -> v.toString().startsWith(releaseVersion))
                        .findFirst()
                        .map(ArtifactVersion::toString);
    }
}
