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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Base class for {@link org.codehaus.mojo.versions.api.VersionDetails}.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionDetails
    implements VersionDetails
{

    /**
     * The current version.
     * Guarded by {@link #currentVersionLock}.
     *
     * @since 1.0-beta-1
     */
    private ArtifactVersion currentVersion = null;

    /**
     * Do we want to include snapshots when snapshot inclusion is not specified.
     * Guarded by {@link #currentVersionLock}.
     *
     * @since 1.0-beta-1
     */
    private boolean includeSnapshots = false;

    /**
     * Not sure if we need to be thread safe, but there's no harm being careful, after all we could be invoked from an
     * IDE.
     *
     * @since 1.0-beta-1
     */
    private final Object currentVersionLock = new Object();

    public final boolean isCurrentVersionDefined()
    {
        return getCurrentVersion() != null;
    }

    public final ArtifactVersion getCurrentVersion()
    {
        synchronized ( currentVersionLock )
        {
            return currentVersion;
        }
    }

    public final void setCurrentVersion( ArtifactVersion currentVersion )
    {
        synchronized ( currentVersionLock )
        {
            this.currentVersion = currentVersion;
        }
    }

    public final void setCurrentVersion( String currentVersion )
    {
        setCurrentVersion( new DefaultArtifactVersion( currentVersion ) );
    }

    public final boolean isIncludeSnapshots()
    {
        synchronized ( currentVersionLock )
        {
            return includeSnapshots;
        }
    }

    public final void setIncludeSnapshots( boolean includeSnapshots )
    {
        synchronized ( currentVersionLock )
        {
            this.includeSnapshots = includeSnapshots;
        }
    }

    public final ArtifactVersion[] getVersions()
    {
        return getVersions( isIncludeSnapshots() );
    }

    public abstract ArtifactVersion[] getVersions( boolean includeSnapshots );

    public final ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots )
    {
        Set/*<ArtifactVersion>*/ result;
        result = new TreeSet( getVersionComparator() );
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( versionRange != null && !versionRange.containsVersion( candidate ) )
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

    public final ArtifactVersion[] getVersions( ArtifactVersion currentVersion, ArtifactVersion upperBound )
    {
        return getVersions( currentVersion, upperBound, isIncludeSnapshots() );
    }

    public final ArtifactVersion[] getVersions( ArtifactVersion currentVersion, ArtifactVersion upperBound,
                                                boolean includeSnapshots )
    {
        return getVersions( currentVersion, upperBound, includeSnapshots, false, false );
    }

    private final ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
    {
        return getVersions( version, null, includeSnapshots, false, true );
    }

    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getNewestVersion( lowerBound, upperBound, isIncludeSnapshots() );
    }

    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
    {
        return getNewestVersion( lowerBound, upperBound, includeSnapshots, false, false );
    }

    public final ArtifactVersion getNewestVersion( VersionRange versionRange, ArtifactVersion lowerBound, 
                                                   ArtifactVersion upperBound, boolean includeSnapshots, 
                                                   boolean includeLower, boolean includeUpper )
    {
        ArtifactVersion latest = null;
        final VersionComparator versionComparator = getVersionComparator();
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( versionRange != null && !versionRange.containsVersion( candidate ) )
            {
                continue;
            }
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

    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots, boolean includeLower,
                                                   boolean includeUpper )
    {
        return getNewestVersion( null, lowerBound, upperBound, includeSnapshots, includeLower, includeUpper );
    }

    public final ArtifactVersion getNewestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, null, null, includeSnapshots, true, true );
    }

    public final boolean containsVersion( String version )
    {
        Iterator i = Arrays.asList( getVersions( true ) ).iterator();
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

    public final ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots )
    {
        return getNewerVersions( new DefaultArtifactVersion( version ), includeSnapshots );
    }
                                                          
    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getOldestVersion( lowerBound, upperBound, true );
    }

    public final ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        return getOldestVersion( versionRange, null, null, includeSnapshots, true, true );
    }

    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
    {
        return getOldestVersion( lowerBound, upperBound, includeSnapshots, false, false );
    }

    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots, boolean includeLower,
                                                   boolean includeUpper )
    {
        return getOldestVersion( null, lowerBound, upperBound, includeSnapshots, includeLower, includeUpper);
    }

    public final ArtifactVersion getOldestVersion( VersionRange versionRange, ArtifactVersion lowerBound, 
                                                   ArtifactVersion upperBound, boolean includeSnapshots, 
                                                   boolean includeLower, boolean includeUpper )
    {
        ArtifactVersion oldest = null;
        final VersionComparator versionComparator = getVersionComparator();
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
        while ( i.hasNext() )
        {
            ArtifactVersion candidate = (ArtifactVersion) i.next();
            if ( versionRange != null && !versionRange.containsVersion( candidate ) )
            {
                continue;
            }
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

    public final ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                boolean includeSnapshots, boolean includeLower, boolean includeUpper )
    {
        Set/*<ArtifactVersion>*/ result;
        final VersionComparator versionComparator = getVersionComparator();
        result = new TreeSet( versionComparator );
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
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
            result.add( candidate );
        }
        return (ArtifactVersion[]) result.toArray( new ArtifactVersion[result.size()] );
    }

    public final ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope )
    {
        return getOldestUpdate( currentVersion, updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope )
    {
        return getNewestUpdate( currentVersion, updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, UpdateScope updateScope )
    {
        return getAllUpdates( currentVersion, updateScope, isIncludeSnapshots() );
    }

    public ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, VersionRange versionRange )
    {
        return getOldestUpdate( currentVersion,  versionRange, isIncludeSnapshots());
    }

    public ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, VersionRange versionRange )
    {
        return getNewestUpdate( currentVersion,  versionRange, isIncludeSnapshots());
    }

    public ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, VersionRange versionRange )
    {
        return getAllUpdates( currentVersion,  versionRange, isIncludeSnapshots());
    }

    public final ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope,
                                                  boolean includeSnapshots )
    {
        return updateScope.getOldestUpdate( this, currentVersion, includeSnapshots );
    }

    public final ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, UpdateScope updateScope,
                                                  boolean includeSnapshots )
    {
        return updateScope.getNewestUpdate( this, currentVersion, includeSnapshots );
    }

    public final ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, UpdateScope updateScope,
                                                  boolean includeSnapshots )
    {
        return updateScope.getAllUpdates( this, currentVersion, includeSnapshots );
    }

    public ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, VersionRange versionRange,
                                            boolean includeSnapshots )
    {
        return getOldestVersion( versionRange, includeSnapshots );
    }

    public ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, VersionRange versionRange,
                                            boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, includeSnapshots );
    }

    public ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, VersionRange versionRange,
                                            boolean includeSnapshots )
    {
        return new ArtifactVersion[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    public final ArtifactVersion getOldestUpdate( UpdateScope updateScope )
    {
        return getOldestUpdate( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion getNewestUpdate( UpdateScope updateScope )
    {
        return getNewestUpdate( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion[] getAllUpdates( UpdateScope updateScope )
    {
        return getAllUpdates( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion getOldestUpdate( UpdateScope updateScope, boolean includeSnapshots )
    {
        return getOldestUpdate( getCurrentVersion(), updateScope, includeSnapshots );
    }

    public final ArtifactVersion getNewestUpdate( UpdateScope updateScope, boolean includeSnapshots )
    {
        return getNewestUpdate( getCurrentVersion(), updateScope, includeSnapshots );
    }

    public final ArtifactVersion[] getAllUpdates( UpdateScope updateScope, boolean includeSnapshots )
    {
        return getAllUpdates( getCurrentVersion(), updateScope, includeSnapshots );
    }
}
