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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since Aug 4, 2009 11:04:36 AM
 */
public abstract class AbstractVersionDetails
    implements VersionDetails
{

    public final ArtifactVersion[] getVersions()
        throws ArtifactMetadataRetrievalException
    {
        return getVersions( true );
    }

    public abstract ArtifactVersion[] getVersions( boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException;

    public final ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        Set/*<ArtifactVersion>*/ result;
        result = new TreeSet( getVersionComparator() );
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
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

    public final ArtifactVersion[] getNewerVersions( ArtifactVersion version )
        throws ArtifactMetadataRetrievalException
    {
        return getNewerVersions( version, true );
    }

    public final ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        Set/*<ArtifactVersion>*/ result;
        final VersionComparator versionComparator = getVersionComparator();
        result = new TreeSet( versionComparator );
        SortedSet/*<ArtifactVersion>*/ versions = new TreeSet( versionComparator );
        versions.addAll( Arrays.asList( getVersions( includeSnapshots ) ) );
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

    public final ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound )
        throws ArtifactMetadataRetrievalException
    {
        return getLatestVersion( currentVersion, upperBound, true );
    }

    public final ArtifactVersion getLatestVersion( ArtifactVersion currentVersion, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        return getLatestVersion( currentVersion, upperBound, includeSnapshots, false, false );
    }

    public final ArtifactVersion getLatestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots, boolean includeLower,
                                                   boolean includeUpper )
        throws ArtifactMetadataRetrievalException
    {
        ArtifactVersion latest = null;
        final VersionComparator versionComparator = getVersionComparator();
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

    public final ArtifactVersion getLatestVersion( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        ArtifactVersion latest = null;
        final VersionComparator versionComparator = getVersionComparator();
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
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

    public final boolean containsVersion( String version )
        throws ArtifactMetadataRetrievalException
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

    public final ArtifactVersion[] getNewerVersions( String version )
        throws ArtifactMetadataRetrievalException
    {
        return getNewerVersions( version, true );
    }

    public final ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        return getNewerVersions( new DefaultArtifactVersion( version ), includeSnapshots );
    }

    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
        throws ArtifactMetadataRetrievalException
    {
        return getOldestVersion( lowerBound, upperBound, true );
    }

    public final ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        ArtifactVersion oldest = null;
        final VersionComparator versionComparator = getVersionComparator();
        Iterator i = Arrays.asList( getVersions( includeSnapshots ) ).iterator();
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

    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        return getOldestVersion( lowerBound, upperBound, includeSnapshots, false, false );
    }

    public final ArtifactVersion getOldestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots, boolean includeLower,
                                                   boolean includeUpper )
        throws ArtifactMetadataRetrievalException
    {
        ArtifactVersion oldest = null;
        final VersionComparator versionComparator = getVersionComparator();
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

    public final ArtifactVersion[] getVersions( DefaultArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                boolean includeSnapshots, boolean includeLower, boolean includeUpper )
        throws ArtifactMetadataRetrievalException
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
}
