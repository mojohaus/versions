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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

import java.util.Arrays;
import java.util.Comparator;
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
    private final Comparator/*<ArtifactVersion>*/ rule;

    /**
     * Creates a new {@link ArtifactVersions} instance.
     *
     * @param artifact The artifact.
     * @param versions The versions.
     * @param rule     The version comparison rule.
     * @since 1.0-alpha-3
     */
    public ArtifactVersions( Artifact artifact, ArtifactVersion[] versions, Comparator rule )
    {
        this( artifact, Arrays.asList( versions ), rule );
    }

    /**
     * Creates a new {@link ArtifactVersions} instance.
     *
     * @param artifact The artifact.
     * @param versions The versions.
     * @param rule     The version comparison rule.
     * @since 1.0-alpha-3
     */
    public ArtifactVersions( Artifact artifact, List versions, Comparator rule )
    {
        this.artifact = artifact;
        this.rule = rule;
        this.versions = new TreeSet( rule );
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
            result = new TreeSet( rule );
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
        result = new TreeSet( rule );
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
        result = new TreeSet( rule );
        Iterator i = versions.tailSet( version ).iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( rule.compare( version, candidate ) >= 0 )
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
     * @param upperBound       the exclusive upper bound.
     * @param includeSnapshots <code>true</code> if snapshots are to be included.
     * @return the latest version between currentVersion and upperBound or <code>null</code> if no version is available.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound,
                                             boolean includeSnapshots )
    {
        try
        {
            final String versionRangeStr = "(" + currentVersion.toString() + "," + upperBound.toString() + ")";
            return getLatestVersion( VersionRange.createFromVersionSpec( versionRangeStr ), includeSnapshots );
        }
        catch ( InvalidVersionSpecificationException e )
        {
            return null; // no valid version
        }
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
            else if ( rule.compare( latest, candidate ) < 0 )
            {
                latest = candidate;
            }
        }
        return latest;
    }


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
        return getNewerVersions( new DefaultArtifactVersion( version), includeSnapshots );
    }
}
