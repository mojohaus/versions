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

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DefaultArtifactVersionCache;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.SegmentUtils;

import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * Displays any updates of the project's parent project
 *
 * @author Stephen Connolly
 * @since 2.2
 */
@Mojo(name = "display-parent-updates", threadSafe = true)
public class DisplayParentUpdatesMojo extends AbstractVersionsDisplayMojo {

    public static final int MESSAGE_LENGTH = 68;

    // ------------------------------ FIELDS ------------------------------

    /**
     * <p>If {@code skipResolution} is not set, specifies the <em>bottom</em> version considered
     * for target version resolution. If it is a version range, the resolved version will be
     * restricted by that range.</p>
     *
     * <p>If {@code skipResolution} is {@code true}, will specify the target version to which
     * the parent artifact will be updated.</p>
     * @since 2.16.2
     */
    @Parameter(property = "parentVersion")
    protected String parentVersion = null;

    /**
     * to update parent version by force when it is RELEASE or LATEST
     *
     * @since 2.16.2
     */
    @Parameter(property = "forceUpdate", defaultValue = "false")
    protected boolean forceUpdate = false;

    /**
     * Skips version resolution, only valid if {@code parentVersion} is set.
     * Will effectively set the new parent version to the one from {@code parentVersion}
     *
     * @since 2.16.2
     */
    @Parameter(property = "skipResolution", defaultValue = "false")
    protected boolean skipResolution = false;

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a version within the range fulfilling the criteria.</p>
     * <p>Default <code>false</code></p>
     *
     * @since 2.16.2
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.16.2
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.16.2
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.16.2
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    // -------------------------- OTHER METHODS --------------------------

    @Inject
    public DisplayParentUpdatesMojo(
            RepositorySystem repositorySystem,
            org.eclipse.aether.RepositorySystem aetherRepositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders);
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        logInit();
        if (getProject().getParent() == null) {
            logLine(false, "Project does not have a parent.");
            return;
        }

        if (reactorProjects.contains(getProject().getParent())) {
            logLine(false, "Parent project is part of the reactor.");
            return;
        }

        if (skipResolution && isBlank(parentVersion)) {
            throw new MojoExecutionException("skipResolution is only valid if parentVersion is set");
        }
        String initialVersion = Optional.ofNullable(parentVersion)
                .orElse(getProject().getParent().getVersion());
        ArtifactVersion artifactVersion;
        try {
            artifactVersion = skipResolution
                    ? DefaultArtifactVersionCache.of(parentVersion)
                    : resolveTargetVersion(initialVersion);
        } catch (VersionRetrievalException | InvalidVersionSpecificationException | InvalidSegmentException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }

        if (artifactVersion == null || initialVersion.equals(artifactVersion.toString())) {
            logLine(false, "The parent project is the latest version:");
            StringBuilder buf = new StringBuilder(MESSAGE_LENGTH);
            buf.append("  ");
            buf.append(getProject().getParent().getGroupId());
            buf.append(':');
            buf.append(getProject().getParent().getArtifactId());
            buf.append(' ');
            int padding = MESSAGE_LENGTH - initialVersion.length();
            while (buf.length() < padding) {
                buf.append('.');
            }
            buf.append(' ');
            buf.append(initialVersion);
            logLine(false, buf.toString());
        } else {
            logLine(false, "The parent project has a newer version:");
            StringBuilder buf = new StringBuilder(MESSAGE_LENGTH);
            buf.append("  ");
            buf.append(getProject().getParent().getGroupId());
            buf.append(':');
            buf.append(getProject().getParent().getArtifactId());
            buf.append(' ');
            int padding = MESSAGE_LENGTH
                    - initialVersion.length()
                    - artifactVersion.toString().length()
                    - " -> ".length();
            while (buf.length() < padding) {
                buf.append('.');
            }
            buf.append(' ');
            buf.append(initialVersion);
            buf.append(" -> ");
            buf.append(artifactVersion);
            logLine(false, buf.toString());
        }
    }

    protected ArtifactVersion resolveTargetVersion(String initialVersion)
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        Artifact artifact = getHelper()
                .createDependencyArtifact(DependencyBuilder.newBuilder()
                        .withGroupId(getProject().getParent().getGroupId())
                        .withArtifactId(getProject().getParent().getArtifactId())
                        .withVersion(initialVersion)
                        .withType("pom")
                        .build());

        VersionRange targetVersionRange = VersionRange.createFromVersionSpec(initialVersion);
        if (targetVersionRange.getRecommendedVersion() != null) {
            targetVersionRange = targetVersionRange.restrict(
                    VersionRange.createFromVersionSpec("[" + targetVersionRange.getRecommendedVersion() + ",)"));
        }

        final ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, false);
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog());

        // currentVersion (set to parentVersion here) is not included in the version range for searching upgrades
        // unless we set allowDowngrade to true
        for (ArtifactVersion candidate : reverse(versions.getNewerVersions(
                initialVersion, unchangedSegment, allowSnapshots, !isBlank(parentVersion) || allowDowngrade))) {
            if (allowDowngrade
                    || targetVersionRange == null
                    || ArtifactVersions.isVersionInRange(candidate, targetVersionRange)) {
                if (shouldApplyUpdate(artifact, getProject().getParent().getVersion(), candidate, forceUpdate)) {
                    return candidate;
                } else {
                    getLog().debug("Update not applied. Exiting.");
                    return null;
                }
            }
        }

        if (versions.isEmpty(allowSnapshots)) {
            getLog().info("No versions found");
        } else {
            getLog().info("The parent project is the latest version");
        }

        return null;
    }

    private static <T> Iterable<T> reverse(T[] array) {
        return Arrays.stream(array).sorted(Collections.reverseOrder()).collect(Collectors.toList());
    }

    @Override
    protected void update(ModifiedPomXMLEventReader pom) {}
}
