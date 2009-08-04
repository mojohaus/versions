package org.codehaus.mojo.versions.api;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.VersionComparator;

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
/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since Aug 4, 2009 10:28:14 AM
 */
public interface VersionDetails
{
    /**
     * Returns all the available versions in increasing order.
     *
     * @return all the available versions in increasing order.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions()
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all available versions in increasing order.
     *
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions in increasing order.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions( boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all available versions within the specified version range.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all the available versions newer than the provided version.
     *
     * @param version The lower (exclusive) bound.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getNewerVersions( ArtifactVersion version )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all available versions within the specified version range.
     *
     * @param version          The lower (exclusive) bound.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the current version.
     * @param upperBound     the exclusive upper bound.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion   the current version.
     * @param upperBound       the exclusive upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound,
                                      boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound       the lower bound.
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getLatestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots,
                                      boolean includeLower, boolean includeUpper )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the latest version within the specified version range or
     * <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    ArtifactVersion getLatestVersion( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns <code>true</code> if the specific version is in the list of versions.
     *
     * @param version the specific version.
     * @return <code>true</code> if the specific version is in the list of versions.
     */
    boolean containsVersion( String version )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all the available versions newer than the provided version.
     *
     * @param version The lower (exclusive) bound.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getNewerVersions( String version )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all available versions within the specified version range.
     *
     * @param version          The lower (exclusive) bound.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Gets the rule for version comparison of this artifact.
     *
     * @return the rule for version comparison of this artifact.
     * @since 1.0-beta-1
     */
    VersionComparator getVersionComparator();

    /**
     * Returns the next version after the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound the inclusive lower bound.
     * @param upperBound the exclusive upper bound.
     * @return the next version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the oldest version within the specified version range or
     * <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound       the current version.
     * @param upperBound       the exclusive upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns the oldest version within the specified bounds or <code>null</code> if no such version exists.
     *
     * @param lowerBound       the current version.
     * @param upperBound       the exclusive upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return the oldest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots,
                                      boolean includeLower, boolean includeUpper )
        throws ArtifactMetadataRetrievalException;

    /**
     * Returns all available versions within the specified bounds.
     *
     * @param lowerBound       the current version.
     * @param upperBound       the exclusive upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return all available versions within the specified version range.
     * @since 1.0-beta-1
     */
    ArtifactVersion[] getVersions( DefaultArtifactVersion lowerBound, ArtifactVersion upperBound,
                                   boolean includeSnapshots, boolean includeLower, boolean includeUpper )
        throws ArtifactMetadataRetrievalException;
}
