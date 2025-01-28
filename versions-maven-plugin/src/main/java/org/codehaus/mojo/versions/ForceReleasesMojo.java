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
import java.util.Collection;
import java.util.Map;
import java.util.regex.Matcher;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
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
 * Replaces any -SNAPSHOT versions with a release version, older if necessary (if there has been a release).
 *
 * @author Stephen Connolly
 * @since 2.2
 */
@Mojo(name = "force-releases", threadSafe = true)
public class ForceReleasesMojo extends AbstractVersionsDependencyUpdaterMojo {
    /**
     * Whether to fail if a SNAPSHOT could not be replaced
     *
     * @since 2.14.0
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

    // ------------------------------ METHODS --------------------------

    @Inject
    public ForceReleasesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
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
        return false;
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
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

            Matcher versionMatcher = SNAPSHOT_REGEX.matcher(dep.getVersion());
            if (versionMatcher.matches()) {
                String releaseVersion = versionMatcher.group(1);
                Artifact artifact = this.toArtifact(dep);
                if (!isIncluded(artifact)) {
                    continue;
                }

                getLog().debug("Looking for a release of " + toString(dep));
                ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
                if (versions.containsVersion(releaseVersion)) {
                    updateDependencyVersion(pom, dep, releaseVersion, changeKind);
                } else {
                    ArtifactVersion newestRelease = versions.getNewestVersion((VersionRange) null, null, false, true);
                    if (newestRelease == null) {
                        getLog().info("No release of " + toString(dep) + " to force.");
                        if (failIfNotReplaced) {
                            throw new MojoExecutionException(
                                    "No matching release of " + toString(dep) + " found for update.");
                        }
                    } else {
                        updateDependencyVersion(pom, dep, newestRelease.toString(), changeKind);
                    }
                }
            }
        }
    }
}
