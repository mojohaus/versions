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

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.RepositoryUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.resolution.VersionRequest;
import org.eclipse.aether.resolution.VersionResolutionException;
import org.eclipse.aether.resolution.VersionResult;

/**
 * Attempts to resolve unlocked snapshot dependency versions to the locked timestamp versions used in the build. For
 * example, an unlocked snapshot version like "1.0-SNAPSHOT" could be resolved to "1.0-20090128.202731-1". If a
 * timestamped snapshot is not available, then the version will remained unchanged. This would be the case if the
 * dependency is only available in the local repository and not in a remote snapshot repository.
 *
 * @author Paul Gier
 * @since 1.0-alpha-3
 */
@Mojo(name = "lock-snapshots", threadSafe = true)
public class LockSnapshotsMojo extends AbstractVersionsDependencyUpdaterMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a timestamped snapshot version. For example 1.0-20090128.202731-1
     */
    private static final Pattern TIMESTAMPED_SNAPSHOT_REGEX = Pattern.compile("-" + Artifact.SNAPSHOT_VERSION);

    // ------------------------------ METHODS --------------------------

    @Inject
    public LockSnapshotsMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException when things go wrong in a very bad way
     * @throws XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        try {
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    lockSnapshots(pom, dependencyManagement.getDependencies());
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                lockSnapshots(pom, getProject().getDependencies());
            }
            if (getProject().getParent() != null && isProcessingParent()) {
                lockParentSnapshot(pom, getProject().getParent());
            }
        } catch (IOException | VersionResolutionException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    protected void lockSnapshots(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws XMLStreamException, MojoExecutionException, VersionResolutionException {
        for (Dependency dep : dependencies) {
            if (isExcludeReactor() && isProducedByReactor(dep)) {
                getLog().info("Ignoring reactor dependency: " + toString(dep));
                continue;
            }

            if (isHandledByProperty(dep)) {
                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                continue;
            }

            if (!isIncluded(this.toArtifact(dep))) {
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = TIMESTAMPED_SNAPSHOT_REGEX.matcher(version);
            if (versionMatcher.find() && versionMatcher.end() == version.length()) {
                Optional<String> lockedVersion = resolveSnapshotVersion(dep);
                if (lockedVersion.isPresent()) {
                    if (PomHelper.setDependencyVersion(
                            pom,
                            dep.getGroupId(),
                            dep.getArtifactId(),
                            version,
                            lockedVersion.get(),
                            getProject().getModel())) {
                        getLog().info("Locked " + toString(dep) + " to version " + lockedVersion.get());
                    }
                } else {
                    getLog().info("No timestamped version for " + toString(dep) + " found.");
                }
            }
        }
    }

    protected void lockParentSnapshot(ModifiedPomXMLEventReader pom, MavenProject parent)
            throws XMLStreamException, VersionResolutionException {
        if (parent == null) {
            getLog().info("Project does not have a parent");
            return;
        }

        if (reactorProjects.contains(parent)) {
            getLog().info("Project's parent is part of the reactor");
            return;
        }

        Artifact parentArtifact = parent.getArtifact();
        String parentVersion = parentArtifact.getVersion();

        Matcher versionMatcher = TIMESTAMPED_SNAPSHOT_REGEX.matcher(parentVersion);
        if (versionMatcher.find() && versionMatcher.end() == parentVersion.length()) {
            Optional<String> lockedParentVersion = resolveSnapshotVersion(parentArtifact);
            if (lockedParentVersion.isPresent()) {
                if (PomHelper.setProjectParentVersion(pom, lockedParentVersion.get())) {
                    getLog().info("Locked parent " + parentArtifact + " to version " + lockedParentVersion.get());
                }
            } else {
                getLog().info("No timestamped version for " + parentArtifact + " found.");
            }
        }
    }

    /**
     * Determine the timestamp version of the snapshot artifact used in the build.
     *
     * @param artifact artifact for which to retrieve the locked version
     * @return The timestamp version if exists, otherwise {@link Optional#empty()}
     * @throws VersionResolutionException thrown if version resolution fails
     */
    private Optional<String> resolveSnapshotVersion(Artifact artifact) throws VersionResolutionException {
        getLog().debug("Resolving snapshot version for artifact: " + artifact);
        VersionResult versionResult = repositorySystem.resolveVersion(
                session.getRepositorySession(),
                new VersionRequest(
                        RepositoryUtils.toArtifact(artifact),
                        getProject().getRemoteProjectRepositories(),
                        getClass().getSimpleName()));
        return Optional.ofNullable(versionResult.getVersion())
                .filter(v -> !String.valueOf(artifact.getVersion()).equals(v));
    }

    /**
     * Determine the timestamp version of the snapshot dependency used in the build.
     *
     * @param dep dependency for which to retrieve the locked version
     * @return The timestamp version if exists, otherwise {@link Optional#empty()}
     * @throws MojoExecutionException thrown if retrieval of {@link VersionsHelper} fails
     * @throws VersionResolutionException thrown if version resolution fails
     */
    private Optional<String> resolveSnapshotVersion(Dependency dep)
            throws MojoExecutionException, VersionResolutionException {
        return resolveSnapshotVersion(getHelper().createDependencyArtifact(dep));
    }
}
