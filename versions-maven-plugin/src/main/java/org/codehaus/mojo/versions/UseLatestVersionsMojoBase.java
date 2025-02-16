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

import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;

/**
 * Common base class for {@link UseLatestVersionsMojo}
 * and {@link UseLatestReleasesMojo}
 */
public abstract class UseLatestVersionsMojoBase extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * Number of executor threads for update retrieval.
     * @since 2.19.0
     */
    @Parameter(property = "numThreads", defaultValue = "5")
    private int numThreads = 5;

    protected abstract boolean getAllowMajorUpdates();

    protected abstract boolean getAllowMinorUpdates();

    protected abstract boolean getAllowIncrementalUpdates();

    protected abstract boolean getAllowDowngrade();

    protected abstract boolean updateFilter(Dependency dep);

    protected abstract boolean artifactVersionsFilter(ArtifactVersion ver);

    protected abstract Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream);

    private final ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    public UseLatestVersionsMojoBase(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * Represents a version change of an artifact
     */
    private abstract static class ArtifactVersionChange {
        private final DependencyChangeRecord.ChangeKind changeKind;
        private final String newVersion;

        /**
         * Creates a new instance
         * @param changeKind change kind
         * @param newVersion new version
         */
        ArtifactVersionChange(DependencyChangeRecord.ChangeKind changeKind, String newVersion) {
            this.changeKind = changeKind;
            this.newVersion = newVersion;
        }

        /**
         * @return change kind
         */
        DependencyChangeRecord.ChangeKind getChangeKind() {
            return changeKind;
        }

        /**
         * @return new version
         */
        String getNewVersion() {
            return newVersion;
        }
    }

    /**
     * Represents a version change of a dependency
     */
    private static final class DependencyVersionChange extends ArtifactVersionChange {
        private final Dependency dependency;

        /**
         * Constructs a new instance
         * @param changeKind change kind
         * @param dependency {@code Dependency} instance
         * @param newVersion new version
         */
        DependencyVersionChange(
                DependencyChangeRecord.ChangeKind changeKind, Dependency dependency, String newVersion) {
            super(changeKind, newVersion);
            this.dependency = dependency;
        }

        /**
         * @return {@code Dependency} instance
         */
        Dependency getDependency() {
            return dependency;
        }
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
        if (getAllowDowngrade() && getAllowSnapshots()) {
            throw new MojoExecutionException("allowDowngrade is only valid with allowSnapshots equal to false");
        }

        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                getAllowMajorUpdates(), getAllowMinorUpdates(), getAllowIncrementalUpdates(), getLog());
        ConcurrentLinkedQueue<DependencyVersionChange> versionChanges = new ConcurrentLinkedQueue<>();
        Collection<CompletableFuture<Void>> versionChangeFutures = new ArrayList<>();
        try {
            if (getProcessDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    versionChangeFutures.add(getUpdates(
                            versionChanges,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT,
                            unchangedSegment));
                }
            }
            if (getProject().getDependencies() != null && getProcessDependencies()) {
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        getProject().getDependencies(),
                        DependencyChangeRecord.ChangeKind.DEPENDENCY,
                        unchangedSegment));
            }
            if (getProject().getParent() != null && getProcessParent()) {
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        singletonList(getParentDependency()),
                        DependencyChangeRecord.ChangeKind.PARENT,
                        unchangedSegment));
            }

            CompletableFuture.allOf(versionChangeFutures.toArray(new CompletableFuture[0]))
                    .join();
            for (DependencyVersionChange change : versionChanges) {
                updateDependencyVersion(pom, change.getDependency(), change.getNewVersion(), change.getChangeKind());
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        } catch (IllegalStateException e) {
            if (e.getCause() instanceof MojoExecutionException) {
                throw (MojoExecutionException) e.getCause();
            }
            if (e.getCause() instanceof VersionRetrievalException) {
                throw (VersionRetrievalException) e.getCause();
            }
            throw e;
        }
    }

    private CompletableFuture<Void> getUpdates(
            ConcurrentLinkedQueue<DependencyVersionChange> updates,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind,
            Optional<Segment> unchangedSegment) {
        return CompletableFuture.allOf(dependencies.stream()
                .map(dep -> CompletableFuture.runAsync(
                        () -> {
                            if (!updateFilter(dep)) {
                                return;
                            } else if (getExcludeReactor() && isProducedByReactor(dep)) {
                                getLog().info("Ignoring reactor dependency: " + toString(dep));
                            } else if (isHandledByProperty(dep)) {
                                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                            } else {
                                try {
                                    Artifact artifact = toArtifact(dep);
                                    if (!isIncluded(artifact)) {
                                        return;
                                    } else if (getLog().isDebugEnabled()) {
                                        ArtifactVersion selectedVersion =
                                                ArtifactVersionService.getArtifactVersion(dep.getVersion());
                                        getLog().debug("Selected version:" + selectedVersion);
                                        getLog().debug("Looking for newer versions of " + toString(dep));
                                    }

                                    ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
                                    versionProducer(Arrays.stream(versions.getNewerVersions(
                                                            dep.getVersion(),
                                                            unchangedSegment,
                                                            getAllowSnapshots(),
                                                            getAllowDowngrade()))
                                                    .filter(this::artifactVersionsFilter))
                                            .ifPresent(ver -> updates.add(
                                                    new DependencyVersionChange(changeKind, dep, ver.toString())));
                                } catch (VersionRetrievalException
                                        | InvalidSegmentException
                                        | MojoExecutionException e) {
                                    throw new IllegalStateException(e);
                                }
                            }
                        },
                        executor))
                .toArray(CompletableFuture[]::new));
    }
}
