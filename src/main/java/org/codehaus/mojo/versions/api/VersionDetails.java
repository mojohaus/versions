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
import org.codehaus.mojo.versions.ordering.VersionComparator;

/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since 1.0-beta-1
 */
public interface VersionDetails
{
    /**
     * Returns <code>true</code> if the specific version is in the list of versions.
     *
     * @param version the specific version.
     * @return <code>true</code> if the specific version is in the list of versions.
     * @since 1.0-beta-1
     */
    boolean containsVersion( String version );

    /**
     * Returns <code>true</code> if and only if <code>getCurrentVersion() != null</code>.
     *
     * @return <code>true</code> if and only if <code>getCurrentVersion() != null</code>.
     * @since 1.0-beta-1
     */
    boolean isCurrentVersionDefined();

    /**
     * Sets the current version.
     *
     * @param currentVersion The new current version.
     * @since 1.0-beta-1
     */
    void setCurrentVersion( ArtifactVersion currentVersion );

    /**
     * Sets the current version.
     *
     * @param currentVersion The new current version.
     * @since 1.0-beta-1
     */
    void setCurrentVersion( String currentVersion );

    boolean isIncludeSnapshots();

    void setIncludeSnapshots( boolean includeSnapshots );

    /**
     * Retrieves the current version.
     *
     * @return The current version (may be <code>null</code>).
     * @since 1.0-beta-1
     */
    ArtifactVersion getCurrentVersion();

    /**
     * Gets the rule for version comparison of this artifact.
     *
     * @return the rule for version comparison of this artifact.
     * @since 1.0-beta-1
     */
    VersionComparator getVersionComparator();

    /**
     * Returns all the available versions in increasing order.
     *
     * @return all the available versions in increasing order.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions();

    /**
     * Returns all available versions in increasing order.
     *
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions in increasing order.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions( boolean includeSnapshots );

    /**
     * Returns all available versions within the specified version range.
     *
     * @param versionRange The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots );

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param lowerBound the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound the upper bound or <code>null</code> if the upper limit is unbounded.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound );

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param lowerBound the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots );

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( Restriction restriction, boolean includeSnapshots );

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
    ArtifactVersion[] getVersions( VersionRange versionRange, Restriction restriction, boolean includeSnapshots );

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
    ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction,
                                      boolean includeSnapshots, boolean allowDowngrade );

    /**
     * Returns the latest version newer than the specified lowerBound, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound the upper bound or <code>null</code> if the upper limit is unbounded.
     * @return the latest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound );

    /**
     * Returns the latest version newer than the specified lowerBound, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                      boolean includeSnapshots );

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( Restriction restriction, boolean includeSnapshots );

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
    ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction, boolean includeSnapshots );

    /**
     * Returns the latest version within the specified version range or <code>null</code> if no such version exists.
     *
     * @param versionRange The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version within the version range or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( VersionRange versionRange, boolean includeSnapshots );

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
    ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                     boolean includeSnapshots ) throws InvalidSegmentException;

    /**
     * Returns an array of newer versions than the given version, given whether snapshots
     * should be included.
     *
     * @param version           current version in String format
     * @param includeSnapshots  whether snapshot versions should be included
     * @return array of newer versions fulfilling the criteria
     */
    ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots );

    /**
     * Returns an array of newer versions than the given version, given the upper bound segment and whether snapshots
     * should be included.
     *
     * @param version           current version
     * @param upperBoundSegment the upper bound segment; empty() means no upper bound
     * @param includeSnapshots  whether snapshot versions should be included
     * @return array of newer versions fulfilling the criteria
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     *                                 the segment count)
     * @deprecated please use {@link AbstractVersionDetails#getNewerVersions(String, Optional, boolean, boolean)},
     * boolean, boolean)} instead
     */
    ArtifactVersion[] getNewerVersions( String version, Optional<Segment> upperBoundSegment,
                                                     boolean includeSnapshots ) throws InvalidSegmentException;

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
    ArtifactVersion[] getNewerVersions( String versionString, Optional<Segment> upperBoundSegment,
                                                     boolean includeSnapshots, boolean allowDowngrade )
            throws InvalidSegmentException;

    /**
     * Returns the oldest version within the specified version range or <code>null</code> if no such version exists.
     *
     * @param versionRange The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots );

    /**
     * Returns the oldest version within the specified bounds or <code>null</code> if no such version exists.
     *
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( Restriction restriction, boolean includeSnapshots );

    /**
     * Returns the oldest version within the specified bounds or <code>null</code> if no such version exists.
     *
     * @param versionRange The version range within which the version must exist where <code>null</code> imples
     *            <code>[,)</code>.
     * @param restriction version criteria.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( VersionRange versionRange, Restriction restriction, boolean includeSnapshots );

    /**
     * Returns the oldest version newer than the specified current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                     boolean includeSnapshots ) throws InvalidSegmentException;

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
    ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                     boolean includeSnapshots ) throws InvalidSegmentException;

    /**
     * Returns the oldest version newer than the current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( Optional<Segment> updateScope ) throws InvalidSegmentException;

    /**
     * Returns the newest version newer than the specified current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( Optional<Segment> updateScope ) throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param updateScope the update scope to include.
     * @return the all versions after currentVersion within the specified update scope.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope ) throws InvalidSegmentException;

    /**
     * Returns the oldest version newer than the specified current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( Optional<Segment> updateScope, boolean includeSnapshots )
            throws InvalidSegmentException;

    /**
     * Returns the newest version newer than the specified current version, but within the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( Optional<Segment> updateScope, boolean includeSnapshots )
            throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param updateScope the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @throws InvalidSegmentException thrown if the updateScope is greater than the number of segments
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope, boolean includeSnapshots )
            throws InvalidSegmentException;

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param versionRange the version range to include.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( VersionRange versionRange );

    /**
     * Returns the all versions newer than the specified current version, but within the specified update scope.
     *
     * @param versionRange the version range to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( VersionRange versionRange, boolean includeSnapshots );

}
