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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Holds the results of a search for versions of an artifact.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class ArtifactVersions
{
    /**
     * The artifact that who's versions we hold details of.
     *
     * @since 1.0-alpha-3
     */
    private final Artifact artifact;

    /**
     * The available versions.
     *
     * @since 1.0-alpha-3
     */
    private final SortedSet/*<ArtifactVersion>*/ versions;

    /**
     * The cversion comparison rule that is used for this artifact.
     *
     * @since 1.0-alpha-3
     */
    private final VersionComparator versionComparator;

    /**
     * Creates a new {@link ArtifactVersions} instance.
     *
     * @param artifact          The artifact.
     * @param versions          The versions.
     * @param versionComparator The version comparison rule.
     * @since 1.0-alpha-3
     */
    public ArtifactVersions( Artifact artifact, ArtifactVersion[] versions, VersionComparator versionComparator )
    {
        this( artifact, Arrays.asList( versions ), versionComparator );
    }

    /**
     * Creates a new {@link ArtifactVersions} instance.
     *
     * @param artifact          The artifact.
     * @param versions          The versions.
     * @param versionComparator The version comparison rule.
     * @since 1.0-alpha-3
     */
    public ArtifactVersions( Artifact artifact, List versions, VersionComparator versionComparator )
    {
        this.artifact = artifact;
        this.versionComparator = versionComparator;
        this.versions = new TreeSet( versionComparator );
        this.versions.addAll( versions );
    }

    /**
     * Returns the artifact who's version information we are holding.
     *
     * @return the artifact who's version information we are holding.
     * @since 1.0-alpha-3
     */
    public Artifact getArtifact()
    {
        return artifact;
    }

    /**
     * Returns the groupId of the artifact who's versions we are holding.
     *
     * @return the groupId.
     * @since 1.0-alpha-3
     */
    public String getGroupId()
    {
        return getArtifact().getGroupId();
    }

    /**
     * Returns the artifactId of the artifact who's versions we are holding.
     *
     * @return the artifactId.
     * @since 1.0-alpha-3
     */
    public String getArtifactId()
    {
        return getArtifact().getArtifactId();
    }

    /**
     * Returns all the available versions in increasing order.
     *
     * @return all the available versions in increasing order.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions()
    {
        return getVersions( true );
    }

    /**
     * Returns all available versions in increasing order.
     *
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions in increasing order.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions( boolean includeSnapshots )
    {
        Set/*<ArtifactVersion>*/ result;
        if ( includeSnapshots )
        {
            result = versions;
        }
        else
        {
            result = new TreeSet( versionComparator );
            Iterator i = versions.iterator();
            while ( i.hasNext() )
            {
                ArtifactVersion candidate = (ArtifactVersion) i.next();
                if ( ArtifactUtils.isSnapshot( candidate.toString() ) )
                {
                    continue;
                }
                result.add( candidate );
            }
        }
        return (ArtifactVersion[]) result.toArray( new ArtifactVersion[result.size()] );
    }

    /**
     * Returns all available versions within the specified version range.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all available versions within the specified version range.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots )
    {
        Set/*<ArtifactVersion>*/ result;
        result = new TreeSet( versionComparator );
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( !versionRange.containsVersion( candidate ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            result.add( candidate );
        }
        return (ArtifactVersion[]) result.toArray( new ArtifactVersion[result.size()] );
    }

    /**
     * Returns all the available versions newer than the provided version.
     *
     * @param version The lower (exclusive) bound.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getNewerVersions( ArtifactVersion version )
    {
        return getNewerVersions( version, true );
    }

    /**
     * Returns all available versions within the specified version range.
     *
     * @param version          The lower (exclusive) bound.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
    {
        Set/*<ArtifactVersion>*/ result;
        result = new TreeSet( versionComparator );
        Iterator i = versions.tailSet( version ).iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( versionComparator.compare( version, candidate ) >= 0 )
            {
                // the tailSet will skip most older versions except the pivot element
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            result.add( candidate );
        }
        return (ArtifactVersion[]) result.toArray( new ArtifactVersion[result.size()] );
    }

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param currentVersion the current version.
     * @param upperBound     the exclusive upper bound.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound )
    {
        return getLatestVersion( currentVersion, upperBound, true );
    }

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
    public ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound,
                                             boolean includeSnapshots )
    {
        return getLatestVersion( currentVersion, upperBound, includeSnapshots, false, false );
    }

    /**
     * Returns the latest version newer than the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound       the lower bound..
     * @param upperBound       the upper bound or <code>null</code> if the upper limit is unbounded.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @param includeLower     <code>true</code> if the lower bound is inclusive.
     * @param includeUpper     <code>true> if the upper bound is inclusive.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion getLatestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                             boolean includeSnapshots, boolean includeLower, boolean includeUpper )
    {
        ArtifactVersion latest = null;
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            int lower = lowerBound == null ? -1 : versionComparator.compare( lowerBound, candidate );
            int upper = upperBound == null ? +1 : versionComparator.compare( upperBound, candidate );
            if ( lower > 0 || upper < 0 )
            {
                continue;
            }
            if ( ( !includeLower && lower == 0 ) || ( !includeUpper && upper == 0 ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            if ( latest == null )
            {
                latest = candidate;
            }
            else if ( versionComparator.compare( latest, candidate ) < 0 )
            {
                latest = candidate;
            }
        }
        return latest;
    }

    /**
     * Returns the latest version within the specified version range or
     * <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion getLatestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        ArtifactVersion latest = null;
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( !versionRange.containsVersion( candidate ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            if ( latest == null )
            {
                latest = candidate;
            }
            else if ( versionComparator.compare( latest, candidate ) < 0 )
            {
                latest = candidate;
            }
        }
        return latest;
    }

    /**
     * Returns <code>true</code> if the specific version is in the list of versions.
     *
     * @param version the specific version.
     * @return <code>true</code> if the specific version is in the list of versions.
     */
    public boolean containsVersion( String version )
    {
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( version.equals( candidate.toString() ) )
            {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns all the available versions newer than the provided version.
     *
     * @param version The lower (exclusive) bound.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getNewerVersions( String version )
    {
        return getNewerVersions( version, true );
    }

    /**
     * Returns all available versions within the specified version range.
     *
     * @param version          The lower (exclusive) bound.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return all the available versions newer than the provided version.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots )
    {
        return getNewerVersions( new DefaultArtifactVersion( version ), includeSnapshots );
    }

    /**
     * Gets the rule for version comparison of this artifact.
     *
     * @return the rule for version comparison of this artifact.
     * @since 1.0-beta-1
     */
    public VersionComparator getVersionComparator()
    {
        return versionComparator;
    }

    /**
     * Returns the next version after the specified current version, but less than the specified upper bound or
     * <code>null</code> if no such version exists.
     *
     * @param lowerBound the inclusive lower bound.
     * @param upperBound the exclusive upper bound.
     * @return the next version between lowerBound and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    public ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getOldestVersion( lowerBound, upperBound, true );
    }

    /**
     * Returns the oldest version within the specified version range or
     * <code>null</code> if no such version exists.
     *
     * @param versionRange     The version range within which the version must exist.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the oldest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-beta-1
     */
    public ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        ArtifactVersion oldest = null;
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( !versionRange.containsVersion( candidate ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            if ( oldest == null )
            {
                oldest = candidate;
            }
            else if ( versionComparator.compare( oldest, candidate ) > 0 )
            {
                oldest = candidate;
            }
        }
        return oldest;
    }

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
    public ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                             boolean includeSnapshots )
    {
        return getOldestVersion( lowerBound, upperBound, includeSnapshots, false, false );
    }

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
    public ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                             boolean includeSnapshots, boolean includeLower, boolean includeUpper )
    {
        ArtifactVersion oldest = null;
        Iterator i = versions.iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            int lower = lowerBound == null ? -1 : versionComparator.compare( lowerBound, candidate );
            int upper = upperBound == null ? +1 : versionComparator.compare( upperBound, candidate );
            if ( lower > 0 || upper < 0 )
            {
                continue;
            }
            if ( ( !includeLower && lower == 0 ) || ( !includeUpper && upper == 0 ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            if ( oldest == null )
            {
                oldest = candidate;
            }
            else if ( versionComparator.compare( oldest, candidate ) > 0 )
            {
                oldest = candidate;
            }
        }
        return oldest;
    }

}
