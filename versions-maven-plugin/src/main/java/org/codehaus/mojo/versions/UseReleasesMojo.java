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
import java.util.Arrays;
import java.util.Collection;
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
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;

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

    @Inject
    public UseReleasesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
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
        try {
            if (getProcessDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    useReleases(
                            pom,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT);
                }
            }
            if (getProject().getDependencies() != null && getProcessDependencies()) {
                useReleases(pom, getProject().getDependencies(), DependencyChangeRecord.ChangeKind.DEPENDENCY);
            }
            if (getProject().getParent() != null && getProcessParent()) {
                useReleases(pom, singletonList(getParentDependency()), DependencyChangeRecord.ChangeKind.PARENT);
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void useReleases(
            MutableXMLStreamReader pom,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {
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
                Artifact artifact = this.toArtifact(dep);
                if (!isIncluded(artifact)) {
                    continue;
                }

                getLog().debug("Looking for a release of " + toString(dep));
                // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
                // retrieve release version if provided snapshot version.
                artifact.setVersion(releaseVersion);
                Optional<String> targetVersion = findReleaseVersion(
                        pom, dep, version, releaseVersion, getHelper().lookupArtifactVersions(artifact, false));
                if (targetVersion.isPresent()) {
                    updateDependencyVersion(pom, dep, targetVersion.get(), changeKind);
                } else {
                    getLog().info("No matching release of " + toString(dep) + " to update.");
                    if (failIfNotReplaced) {
                        throw new MojoExecutionException(
                                "No matching release of " + toString(dep) + " found for update");
                    }
                }
            }
        }
    }

    private Optional<String> findReleaseVersion(
            MutableXMLStreamReader pom,
            Dependency dep,
            String version,
            String releaseVersion,
            ArtifactVersions versions) {
        return !allowRangeMatching
                ? versions.containsVersion(releaseVersion) ? Optional.of(releaseVersion) : Optional.empty()
                : Arrays.stream(versions.getVersions(false))
                        .sorted((v1, v2) -> -(v1.compareTo(v2)))
                        .filter(v -> v.toString().startsWith(releaseVersion))
                        .findFirst()
                        .map(ArtifactVersion::toString);
    }
}
