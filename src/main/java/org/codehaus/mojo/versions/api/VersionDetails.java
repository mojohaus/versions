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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
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
     * @param versionRange     The version range within which the version must exist.
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
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots );

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots,
                                   boolean includeLower, boolean includeUpper );

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
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is
     *         available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                      boolean includeSnapshots );

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return the latest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots,
                                      boolean includeLower, boolean includeUpper );

    /**
     * Returns the latest version within the specified version range or <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version within the version range or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getNewestVersion( VersionRange versionRange, boolean includeSnapshots );

    /**
     * Returns the oldest version after the specified lowerBound, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound the upper bound or <code>null</code> if the upper limit is unbounded.
     * @return the next version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound );

    /**
     * Returns the oldest version within the specified version range or <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between currentVersion and upperBound or <code>null</code> if no version is
     *         available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots );

    /**
     * Returns the oldest version newer than the specified lower bound, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is
     *         available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                      boolean includeSnapshots );

    /**
     * Returns the oldest version within the specified bounds or <code>null</code> if no such version exists.
     *
     * @param lowerBound       the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return the oldest version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots,
                                      boolean includeLower, boolean includeUpper );

    /**
     * Returns the oldest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope    the update scope to include.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope );

    /**
     * Returns the newest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope    the update scope to include.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope );

    /**
     * Returns the all versions newer than the specified current version, but within the the specified update scope.
     *
     * @param currentVersion the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope    the update scope to include.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, UpdateScope updateScope );

    /**
     * Returns the oldest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion   the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope,
                                     boolean includeSnapshots );

    /**
     * Returns the newest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion   the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope,
                                     boolean includeSnapshots );

    /**
     * Returns the all versions newer than the specified current version, but within the the specified update scope.
     *
     * @param currentVersion   the lower bound or <code>null</code> if the lower limit is unbounded.
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, UpdateScope updateScope,
                                     boolean includeSnapshots );

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
     * Retrieves the current version.
     *
     * @return The current version (may be <code>null</code>).
     * @since 1.0-beta-1
     */
    ArtifactVersion getCurrentVersion();

    /**
     * Returns the oldest version newer than the current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( UpdateScope updateScope );

    /**
     * Returns the newest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope the update scope to include.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( UpdateScope updateScope );

    /**
     * Returns the all versions newer than the specified current version, but within the the specified update scope.
     *
     * @param updateScope the update scope to include.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( UpdateScope updateScope );

    /**
     * Returns the oldest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestUpdate( UpdateScope updateScope, boolean includeSnapshots );

    /**
     * Returns the newest version newer than the specified current version, but within the the specified update scope or
     * <code>null</code> if no such version exists.
     *
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the newest version after currentVersion within the specified update scope or <code>null</code> if no
     *         version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getNewestUpdate( UpdateScope updateScope, boolean includeSnapshots );

    /**
     * Returns the all versions newer than the specified current version, but within the the specified update scope.
     *
     * @param updateScope      the update scope to include.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the all versions after currentVersion within the specified update scope.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getAllUpdates( UpdateScope updateScope, boolean includeSnapshots );


}
