package org.codehaus.mojo.versions.api;

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

import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;

/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since 1.0-beta-1
 */
public interface VersionDetails {
    /**
     * Returns <code>true</code> if the specific version is in the list of versions.
     *
     * @param version the specific version.
     * @return <code>true</code> if the specific version is in the list of versions.
     * @since 1.0-beta-1
     */
    boolean containsVersion(String version);

    /**
     * Sets the current version.
     *
     * @param currentVersion The new current version.
     * @since 1.0-beta-1
     */
    void setCurrentVersion(ArtifactVersion currentVersion);

    /**
     * Sets the current version.
     *
     * @param currentVersion The new current version.
     * @since 1.0-beta-1
     */
    void setCurrentVersion(String currentVersion);

    /**
     * Returns the current version.
     *
     * @return The current version (may be {@code null}).
     * @since 1.0-beta-1
     */
    ArtifactVersion getCurrentVersion();

    /**
     * Returns the current version range (may be {@code null})
     * @return current version range (may be {@code null})
     * @since 2.16.0
     */
    VersionRange getCurrentVersionRange();

    /**
     * Sets the current version range (may be {@code null})
     * @param versionRange version range to set (may be {@code null})
     * @since 2.16.0
     */
    void setCurrentVersionRange(VersionRange versionRange);

    /**
     * Returns all available versions in increasing order.
     *
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions in increasing order.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions(boolean includeSnapshots);

    /**
     * Returns all available versions within the specified version range.
     *
     * @param versionRange The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions(VersionRange versionRange, boolean includeSnapshots);

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions(Restriction restriction, boolean includeSnapshots);

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param versionRange The version range within which the version must exist where <code>null</code> imples
     *            <code>[,)</code>.
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions(VersionRange versionRange, Restriction restriction, boolean includeSnapshots);

    /**
     * Returns the latest version given the version range, restricition, whether to include snapshots and/or
     * allow downgrades, or {@code null} if no such version exists.
     *
     * @param versionRange range to look for the versions
     * @param restriction restriction restricting the version lookup
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param allowDowngrade whether downgrades are allowed
     * @return the latest version satisfying the conditions or <code>null</code> if no version is available.
     */
    ArtifactVersion getNewestVersion(
            VersionRange versionRange, Restriction restriction, boolean includeSnapshots, boolean allowDowngrade);

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion(Restriction restriction, boolean includeSnapshots);

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param versionRange The version range within which the version must exist where <code>null</code> imples
     *            <code>[,)</code>.
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion(VersionRange versionRange, Restriction restriction, boolean includeSnapshots);

    /**
     * Returns the latest version within the specified version range or <code>null</code> if no such version exists.
     *
     * @param versionRange The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version within the version range or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion(VersionRange versionRange, boolean includeSnapshots);

    /**
     * Returns the latest version, newer than the given version, given the upper bound segment and whether snapshots
     * should be included.
     *
     * @param versionString current version
     * @param unchangedSegment segment that may not be changed; empty() means no upper bound
     * @param includeSnapshots whether snapshot versions should be included
     * @param allowDowngrade whether to allow downgrading if the current version is a snapshots and snapshots
     *                       are disallowed
     * @return newer version or {@link Optional#empty()} if none can be found
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     * the segment count)
     */
    Optional<ArtifactVersion> getNewestVersion(
            String versionString, Optional<Segment> unchangedSegment, boolean includeSnapshots, boolean allowDowngrade)
            throws InvalidSegmentException;

    /**
     * Returns the newest version newer than the specified current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdateWithinSegment(
            ArtifactVersion currentVersion, Optional<Segment> updateScope, boolean includeSnapshots)
            throws InvalidSegmentException;

    /**
     * Returns an array of newer versions than the given version, given the upper bound segment and whether snapshots
     * should be included.
     *
     * @param versionString current version
     * @param upperBoundSegment the upper bound segment; empty() means no upper bound
     * @param includeSnapshots whether snapshot versions should be included
     * @param allowDowngrade whether to allow downgrading if the current version is a snapshots and snapshots
     *                       are disallowed
     * @return array of newer versions fulfilling the criteria
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     * the segment count)
     */
    ArtifactVersion[] getNewerVersions(
            String versionString, Optional<Segment> upperBoundSegment, boolean includeSnapshots, boolean allowDowngrade)
            throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates(
            ArtifactVersion currentVersion, Optional<Segment> updateScope, boolean includeSnapshots)
            throws InvalidSegmentException;

    /**
     * <p>Returns the newest version newer than the specified current version, <u>only within the segment specified
     * by {@code updateScope}</u> or {@code null} if no such version exists.</p>
     * <p>If {@code updateScope} is {@link Optional#empty()}, will return all updates.</p>
     *
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdateWithinSegment(Optional<Segment> updateScope, boolean includeSnapshots)
            throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param updateScope the update scope to include.
     * @param includeSnapshots {@code true} if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates(Optional<Segment> updateScope, boolean includeSnapshots)
            throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version
     *
     * @param includeSnapshots {@code true} if snapshots are to be included.
     * @return the all versions after currentVersion
     * @since 2.13.0
     */
    ArtifactVersion[] getAllUpdates(boolean includeSnapshots);

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param versionRange the version range to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates(VersionRange versionRange, boolean includeSnapshots);

    /**
     * <p>Returns a {@linkplain Restriction} object for computing version <em>upgrades</em>
     * <u>within the given segment</u> allowing updates, with all more major segments locked in place,
     * but also <u>ignoring all version updates from lesser scopes</u>.</p>
     *
     * @param lowerBound artifact version, for which the unchanged segment is computed
     * @param selectedSegment segment, for which the restriction is to be built or {@link Optional#empty()} for no restriction
     * @return {@linkplain Restriction} object based on the arguments
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     * the segment count)
     */
    Restriction restrictionForSelectedSegment(ArtifactVersion lowerBound, Optional<Segment> selectedSegment)
            throws InvalidSegmentException;

    /**
     * <p>Returns a {@linkplain Restriction} object for computing version <em>upgrades</em>
     * <u>within the all segments</u> minor/lesser to the provided {@code unchangedSegment}.</p>
     * <p>If the provided segment is {@link Optional#empty()}, all possible updates are returned.</p>
     *
     * @param lowerBound artifact version, for which the unchanged segment is computed
     * @param unchangedSegment segment, which should not be changed or {@link Optional#empty()} for no restriction
     * @param allowDowngrade whether downgrades are allowed
     * @return {@linkplain Restriction} object based on the arguments
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     * the segment count)
     */
    Restriction restrictionForUnchangedSegment(
            ArtifactVersion lowerBound, Optional<Segment> unchangedSegment, boolean allowDowngrade)
            throws InvalidSegmentException;

    /**
     * Returns the {@link Restriction} objects for a segemnt scope which is to be <b>ignored</b>.
     *
     * @param lowerBound artifact version, for which the unchanged segment is computed
     * @param ignored most major segment where updates are to be ignored; Optional.empty() for no ignored segments
     * @return {@linkplain Restriction} object based on the arguments
     */
    Restriction restrictionForIgnoreScope(ArtifactVersion lowerBound, Optional<Segment> ignored);
}
