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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;

/**
 * Replaces any <em>release</em> versions (i.e. versions that are not snapshots and do not
 * have a year-month-day suffix) with the latest <em>release</em> version. This goal
 * will <em>not</em> replace versions of dependencies which use snapshots
 * or versions with a year-month-day suffix.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo(name = "use-latest-releases", threadSafe = true)
public class UseLatestReleasesMojo extends UseLatestVersionsMojoBase {

    // ------------------------------ FIELDS ------------------------------

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.2
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 1.2
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 1.2
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a non-snapshot version within the range fulfilling the criteria.</p>
     * <p>Only valid if <code>allowSnapshots</code> is <code>false</code>.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseLatestReleasesMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        try {
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    useLatestReleases(
                            pom,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT);
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                useLatestReleases(pom, getProject().getDependencies(), DependencyChangeRecord.ChangeKind.DEPENDENCY);
            }
            if (getProject().getParent() != null && isProcessingParent()) {
                useLatestReleases(pom, singletonList(getParentDependency()), DependencyChangeRecord.ChangeKind.PARENT);
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void useLatestReleases(
            ModifiedPomXMLEventReader pom,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {
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
        Optional<Segment> unchangedSegment = unchangedSegment1;

        useLatestVersions(
                pom,
                dependencies,
                (dep, versions) -> {
                    try {
                        return getLastFiltered(
                                versions.getNewerVersions(dep.getVersion(), unchangedSegment, false, allowDowngrade),
                                dep);
                    } catch (InvalidSegmentException e) {
                        getLog().warn(String.format(
                                "Skipping the processing of %s:%s:%s due to: %s",
                                dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), e.getMessage()));
                    }
                    return empty();
                },
                changeKind,
                dep -> allowDowngrade
                        || !SNAPSHOT_REGEX.matcher(dep.getVersion()).matches());
    }

    /**
     * Returns the last element of the given {@link ArtifactVersion} array such as every version of the array
     * is included in the {@code includes} and excluded by the {@code excludes} filters
     *
     * @param newer array of {@link ArtifactVersion} with newer versions
     * @param dependency dependency prototype to create the artifacts from
     * @return the newest version fulfilling the criteria
     */
    private Optional<ArtifactVersion> getLastFiltered(ArtifactVersion[] newer, Dependency dependency) {
        return Arrays.stream(newer)
                .filter(version -> {
                    Artifact artefactWithNewVersion = new DefaultArtifact(
                            dependency.getGroupId(),
                            dependency.getArtifactId(),
                            VersionRange.createFromVersion(version.toString()),
                            dependency.getScope(),
                            dependency.getType(),
                            null,
                            new DefaultArtifactHandler(),
                            false);
                    return isIncluded(artefactWithNewVersion);
                })
                .reduce((v1, v2) -> v2);
    }
}
