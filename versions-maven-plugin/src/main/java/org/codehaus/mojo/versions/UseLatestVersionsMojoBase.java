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
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.DefaultArtifactVersionCache;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;

/**
 * Common base class for {@link UseLatestVersionsMojo}
 * and {@link UseLatestReleasesMojo}
 */
public abstract class UseLatestVersionsMojoBase extends AbstractVersionsDependencyUpdaterMojo {
    public UseLatestVersionsMojoBase(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    protected abstract boolean isAllowMajorUpdates();

    protected abstract boolean isAllowMinorUpdates();

    protected abstract boolean isAllowIncrementalUpdates();

    protected abstract boolean isAllowDowngrade();

    protected abstract boolean updateFilter(Dependency dep);

    protected abstract boolean artifactVersionsFilter(ArtifactVersion ver);

    protected abstract Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream);

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        if (isAllowDowngrade() && isAllowSnapshots()) {
            throw new MojoExecutionException("allowDowngrade is only valid with allowSnapshots equal to false");
        }

        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                isAllowMajorUpdates(), isAllowMinorUpdates(), isAllowIncrementalUpdates(), getLog());
        try {
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    update(
                            pom,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT,
                            unchangedSegment);
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                update(
                        pom,
                        getProject().getDependencies(),
                        DependencyChangeRecord.ChangeKind.DEPENDENCY,
                        unchangedSegment);
            }
            if (getProject().getParent() != null && isProcessingParent()) {
                update(
                        pom,
                        singletonList(getParentDependency()),
                        DependencyChangeRecord.ChangeKind.PARENT,
                        unchangedSegment);
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    protected final void update(
            MutableXMLStreamReader pom,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind,
            Optional<Segment> unchangedSegment)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {
        for (Dependency dep : dependencies) {
            if (!updateFilter(dep)) {
                continue;
            } else if (isExcludeReactor() && isProducedByReactor(dep)) {
                getLog().info("Ignoring reactor dependency: " + toString(dep));
                continue;
            } else if (isHandledByProperty(dep)) {
                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                continue;
            }
            Artifact artifact = toArtifact(dep);
            if (!isIncluded(artifact)) {
                continue;
            }
            if (getLog().isDebugEnabled()) {
                ArtifactVersion selectedVersion = DefaultArtifactVersionCache.of(dep.getVersion());
                getLog().debug("Selected version:" + selectedVersion);
                getLog().debug("Looking for newer versions of " + toString(dep));
            }

            ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
            try {
                Optional<ArtifactVersion> newestVer = versionProducer(Arrays.stream(versions.getNewerVersions(
                                dep.getVersion(), unchangedSegment, isAllowSnapshots(), isAllowDowngrade()))
                        .filter(this::artifactVersionsFilter));
                if (newestVer.isPresent()) {
                    updateDependencyVersion(pom, dep, newestVer.get().toString(), changeKind);
                }
            } catch (InvalidSegmentException e) {
                getLog().warn("Ignoring " + this.toString(dep) + " as the version number is too short");
            }
        }
    }
}
