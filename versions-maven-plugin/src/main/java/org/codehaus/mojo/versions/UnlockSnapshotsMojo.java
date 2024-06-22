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
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.recording.DefaultDependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.eclipse.aether.RepositorySystem;

/**
 * Attempts to resolve unlocked snapshot dependency versions to the locked timestamp versions used in the build. For
 * example, an unlocked snapshot version like "1.0-SNAPSHOT" could be resolved to "1.0-20090128.202731-1". If a
 * timestamped snapshot is not available, then the version will remained unchanged. This would be the case if the
 * dependency is only available in the local repository and not in a remote snapshot repository.
 *
 * @author Paul Gier
 * @since 1.0-alpha-3
 */
@Mojo(name = "unlock-snapshots", threadSafe = true)
public class UnlockSnapshotsMojo extends AbstractVersionsDependencyUpdaterMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a timestamped snapshot version. For example 1.0-20090128.202731-1
     */
    private static final Pattern TIMESTAMPED_SNAPSHOT_REGEX = Pattern.compile("-(\\d{8}\\.\\d{6})-(\\d+)$");

    // ------------------------------ METHODS --------------------------

    @Inject
    public UnlockSnapshotsMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        try {
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    unlockSnapshots(
                            pom,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT);
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                unlockSnapshots(pom, getProject().getDependencies(), DependencyChangeRecord.ChangeKind.DEPENDENCY);
            }
            if (getProject().getParent() != null && isProcessingParent()) {
                unlockParentSnapshot(pom, getProject().getParent());
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void unlockSnapshots(
            ModifiedPomXMLEventReader pom, List<Dependency> dependencies, DependencyChangeRecord.ChangeKind changeKind)
            throws XMLStreamException, MojoExecutionException {
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
                String unlockedVersion = versionMatcher.replaceFirst("-SNAPSHOT");
                if (PomHelper.setDependencyVersion(
                        pom,
                        dep.getGroupId(),
                        dep.getArtifactId(),
                        dep.getVersion(),
                        unlockedVersion,
                        getProject().getModel())) {

                    getChangeRecorder()
                            .recordChange(DefaultDependencyChangeRecord.builder()
                                    .withKind(changeKind)
                                    .withDependency(dep)
                                    .withNewVersion(unlockedVersion)
                                    .build());
                    getLog().info("Unlocked " + toString(dep) + " to version " + unlockedVersion);
                }
            }
        }
    }

    private void unlockParentSnapshot(ModifiedPomXMLEventReader pom, MavenProject parent)
            throws XMLStreamException, MojoExecutionException {
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
            String unlockedParentVersion = versionMatcher.replaceFirst("-SNAPSHOT");
            if (PomHelper.setProjectParentVersion(pom, unlockedParentVersion)) {
                getLog().info("Unlocked parent " + parentArtifact + " to version " + unlockedParentVersion);
                getChangeRecorder()
                        .recordChange(DefaultDependencyChangeRecord.builder()
                                .withKind(DependencyChangeRecord.ChangeKind.PARENT)
                                .withArtifact(parentArtifact)
                                .withNewVersion(unlockedParentVersion)
                                .build());
            }
        }
    }
}
