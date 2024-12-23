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

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.recording.DefaultDependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.DefaultArtifactVersionCache;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

import static java.util.stream.StreamSupport.stream;
import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * Sets the parent version to the latest parent version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo(name = "update-parent", threadSafe = true)
public class UpdateParentMojo extends AbstractVersionsUpdaterMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * <p>If {@code skipResolution} is not set, specifies the <em>bottom</em> version considered
     * for target version resolution. If it is a version range, the resolved version will be
     * restricted by that range.</p>
     *
     * <p>If {@code skipResolution} is {@code true}, will specify the target version to which
     * the parent artifact will be updated.</p>
     * @since 1.0-alpha-1
     */
    @Parameter(property = "parentVersion")
    protected String parentVersion = null;

    /**
     * to update parent version by force when it is RELEASE or LATEST
     *
     * @since 2.9
     */
    @Parameter(property = "forceUpdate", defaultValue = "false")
    protected boolean forceUpdate = false;

    /**
     * Skips version resolution, only valid if {@code parentVersion} is set.
     * Will effectively set the new parent version to the one from {@code parentVersion}
     *
     * @since 2.13.0
     */
    @Parameter(property = "skipResolution", defaultValue = "false")
    protected boolean skipResolution = false;

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a version within the range fulfilling the criteria.</p>
     * <p>Default <code>false</code></p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    @Override
    protected boolean isAllowSnapshots() {
        return allowSnapshots;
    }

    // -------------------------- OTHER METHODS --------------------------

    @Inject
    public UpdateParentMojo(
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
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     * @since 1.0-alpha-1
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        if (getProject().getParent() == null) {
            getLog().info("Project does not have a parent");
            return;
        }

        if (reactorProjects.contains(getProject().getParent())) {
            getLog().info("Project's parent is part of the reactor");
            return;
        }

        if (skipResolution && isBlank(parentVersion)) {
            throw new MojoExecutionException("skipResolution is only valid if parentVersion is set");
        }

        try {
            ArtifactVersion artifactVersion =
                    skipResolution ? DefaultArtifactVersionCache.of(parentVersion) : resolveTargetVersion();
            if (artifactVersion != null) {
                getLog().info("Updating parent from " + getProject().getParent().getVersion() + " to "
                        + artifactVersion);

                if (PomHelper.setProjectParentVersion(pom, artifactVersion.toString())) {
                    if (getLog().isDebugEnabled()) {
                        getLog().debug("Made an update from "
                                + getProject().getParent().getVersion() + " to " + artifactVersion);
                    }
                    getChangeRecorder()
                            .recordChange(DefaultDependencyChangeRecord.builder()
                                    .withKind(DependencyChangeRecord.ChangeKind.PARENT)
                                    .withGroupId(getProject().getParent().getGroupId())
                                    .withArtifactId(getProject().getParent().getArtifactId())
                                    .withOldVersion(getProject().getParent().getVersion())
                                    .withNewVersion(artifactVersion.toString())
                                    .build());
                }
            }
        } catch (InvalidVersionSpecificationException e) {
            throw new MojoExecutionException("Invalid version range specification: " + parentVersion, e);
        } catch (InvalidSegmentException e) {
            throw new MojoExecutionException("Invalid segment specification for version " + parentVersion, e);
        }
    }

    protected ArtifactVersion resolveTargetVersion()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        Artifact artifact = getHelper()
                .createDependencyArtifact(DependencyBuilder.newBuilder()
                        .withGroupId(getProject().getParent().getGroupId())
                        .withArtifactId(getProject().getParent().getArtifactId())
                        .withVersion(getProject().getParent().getVersion())
                        .withType("pom")
                        .build());

        VersionRange targetVersionRange = VersionRange.createFromVersionSpec(parentVersion);
        if (targetVersionRange != null && targetVersionRange.getRecommendedVersion() != null) {
            targetVersionRange = targetVersionRange.restrict(
                    VersionRange.createFromVersionSpec("[" + targetVersionRange.getRecommendedVersion() + ",)"));
        }

        final ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog());

        // currentVersion (set to parentVersion here) is not included in the version range for searching upgrades
        // unless we set allowDowngrade to true

        VersionRange finalTargetVersionRange = targetVersionRange;
        return stream(
                        reverse(versions.getNewerVersions(
                                        getProject().getParent().getVersion(),
                                        unchangedSegment,
                                        allowSnapshots,
                                        allowDowngrade))
                                .spliterator(),
                        false)
                .filter(v -> finalTargetVersionRange == null
                        || ArtifactVersions.isVersionInRange(v, finalTargetVersionRange))
                .findFirst()
                .map(candidate -> {
                    if (shouldApplyUpdate(artifact, getProject().getParent().getVersion(), candidate, forceUpdate)) {
                        return candidate;
                    } else {
                        getLog().debug("Update not applied. Exiting.");
                        return null;
                    }
                })
                .orElseGet(() -> {
                    if (versions.isEmpty(allowSnapshots)) {
                        getLog().info("No versions found");
                    } else {
                        getLog().info("The parent project is the latest version");
                    }
                    return null;
                });
    }

    private static <T> Iterable<T> reverse(T[] array) {
        return Arrays.stream(array).sorted(Collections.reverseOrder()).collect(Collectors.toList());
    }
}
