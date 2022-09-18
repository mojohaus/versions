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

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import static java.util.Optional.empty;
import static java.util.Optional.of;

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
     * The current version. Guarded by {@link #currentVersionLock}.
     *
     * @since 1.0-beta-1
     */
    private ArtifactVersion currentVersion = null;

    /**
     * Do we want to include snapshots when snapshot inclusion is not specified. Guarded by {@link #currentVersionLock}.
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

    protected AbstractVersionDetails()
    {
    }

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
        setCurrentVersion( currentVersion == null ? null : new DefaultArtifactVersion( currentVersion ) );
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
        return getVersions( versionRange, null, includeSnapshots );
    }

    public final ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getVersions( lowerBound, upperBound, isIncludeSnapshots() );
    }

    public final ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( lowerBound, false, upperBound, false );
        return getVersions( restriction, includeSnapshots );
    }

    private ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( version, false, null, false );
        return getVersions( restriction, includeSnapshots );
    }

    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getNewestVersion( lowerBound, upperBound, isIncludeSnapshots() );
    }

    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( lowerBound, false, upperBound, false );
        return getNewestVersion( restriction, includeSnapshots );
    }

    public final ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction,
                                                   boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, restriction, includeSnapshots, false );
    }

    private static <T> Iterable<T> reverse( T[] array )
    {
        return Arrays.stream( array ).sorted( Collections.reverseOrder() ).collect( Collectors.toList() );
    }

    public final ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction,
                                                   boolean includeSnapshots, boolean allowDowngrade )
    {
        final VersionComparator versionComparator = getVersionComparator();
        // reverse( getVersions( ... ) ) will contain versions sorted from latest to oldest,
        // so we only need to find the first candidate fulfilling the criteria
        for ( ArtifactVersion candidate : reverse( getVersions( includeSnapshots ) ) )
        {
            if ( !allowDowngrade && versionRange != null
                    && !ArtifactVersions.isVersionInRange( candidate, versionRange ) )
            {
                continue;
            }
            if ( restriction != null && !isVersionInRestriction( restriction, candidate ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            return candidate;
        }
        return null;
    }

    public final ArtifactVersion getNewestVersion( Restriction restriction, boolean includeSnapshots )
    {
        return getNewestVersion( null, restriction, includeSnapshots );
    }

    public final ArtifactVersion getNewestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, null, includeSnapshots );
    }

    public final boolean containsVersion( String version )
    {
        for ( ArtifactVersion candidate : getVersions( true ) )
        {
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
    @Deprecated
    public final ArtifactVersion[] getNewerVersions( String version, Optional<Segment> upperBoundSegment,
                                                     boolean includeSnapshots )
            throws InvalidSegmentException
    {
        return getNewerVersions( version, upperBoundSegment, includeSnapshots, false );
    }

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
    public final ArtifactVersion[] getNewerVersions( String versionString, Optional<Segment> upperBoundSegment,
                                                     boolean includeSnapshots, boolean allowDowngrade )
            throws InvalidSegmentException
    {
        ArtifactVersion currentVersion = new DefaultArtifactVersion( versionString );
        ArtifactVersion lowerBound =
                allowDowngrade ? getLowerBoundArtifactVersion( currentVersion, upperBoundSegment ) : currentVersion;
        ArtifactVersion upperBound = !upperBoundSegment.isPresent() ? null
                : getVersionComparator().incrementSegment( lowerBound, upperBoundSegment.get() );

        Restriction restriction = new Restriction( lowerBound, allowDowngrade, upperBound, allowDowngrade );
        return getVersions( restriction, includeSnapshots );
    }

    public final ArtifactVersion getOldestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        return getOldestVersion( versionRange, null, includeSnapshots );
    }

    public final ArtifactVersion getOldestVersion( Restriction restriction,
                                                   boolean includeSnapshots )
    {
        return getOldestVersion( null, restriction, includeSnapshots );
    }

    public final ArtifactVersion getOldestVersion( VersionRange versionRange, Restriction restriction,
                                                   boolean includeSnapshots )
    {
        ArtifactVersion oldest = null;
        final VersionComparator versionComparator = getVersionComparator();
        for ( ArtifactVersion candidate : getVersions( includeSnapshots ) )
        {
            if ( versionRange != null && !ArtifactVersions.isVersionInRange( candidate, versionRange ) )
            {
                continue;
            }
            if ( restriction != null && !isVersionInRestriction( restriction, candidate ) )
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

    public final ArtifactVersion[] getVersions( Restriction restriction, boolean includeSnapshots )
    {
        return getVersions( null, restriction, includeSnapshots );
    }

    public final ArtifactVersion[] getVersions( VersionRange versionRange, Restriction restriction,
                                                boolean includeSnapshots )
    {
        final VersionComparator versionComparator = getVersionComparator();
        Set<ArtifactVersion> result = new TreeSet<>( versionComparator );
        for ( ArtifactVersion candidate : getVersions( includeSnapshots ) )
        {
            if ( versionRange != null && !ArtifactVersions.isVersionInRange( candidate, versionRange ) )
            {
                continue;
            }
            if ( restriction != null && !isVersionInRestriction( restriction, candidate ) )
            {
                continue;
            }
            if ( !includeSnapshots && ArtifactUtils.isSnapshot( candidate.toString() ) )
            {
                continue;
            }
            result.add( candidate );
        }
        return result.toArray( new ArtifactVersion[0] );
    }

    public final ArtifactVersion getOldestUpdate( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                                  boolean includeSnapshots )
    {
        try
        {
            return getOldestVersion( restrictionFor( currentVersion, updateScope ), includeSnapshots );
        }
        catch ( InvalidSegmentException e )
        {
            return null;
        }
    }

    public final ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                                  boolean includeSnapshots )
    {
        try
        {
            return getNewestVersion( restrictionFor( currentVersion, updateScope ), includeSnapshots );
        }
        catch ( InvalidSegmentException e )
        {
            return null;
        }
    }

    public final ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                                  boolean includeSnapshots )
    {
        try
        {
            return getVersions( restrictionFor( currentVersion, updateScope ), includeSnapshots );
        }
        catch ( InvalidSegmentException e )
        {
            return null;
        }
    }

    public final ArtifactVersion getOldestUpdate( Optional<Segment> updateScope )
    {
        return getOldestUpdate( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion getNewestUpdate( Optional<Segment> updateScope )
    {
        return getNewestUpdate( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope )
    {
        return getAllUpdates( updateScope, isIncludeSnapshots() );
    }

    public final ArtifactVersion getOldestUpdate( Optional<Segment> updateScope, boolean includeSnapshots )
    {
        if ( isCurrentVersionDefined() )
        {
            return getOldestUpdate( getCurrentVersion(), updateScope, includeSnapshots );
        }
        return null;
    }

    public final ArtifactVersion getNewestUpdate( Optional<Segment> updateScope, boolean includeSnapshots )
    {
        if ( isCurrentVersionDefined() )
        {
            return getNewestUpdate( getCurrentVersion(), updateScope, includeSnapshots );
        }
        return null;
    }

    public final ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope, boolean includeSnapshots )
    {
        if ( isCurrentVersionDefined() )
        {
            return getAllUpdates( getCurrentVersion(), updateScope, includeSnapshots );
        }
        return null;
    }

    public final ArtifactVersion[] getAllUpdates( VersionRange versionRange )
    {
        return getAllUpdates( versionRange, isIncludeSnapshots() );
    }

    public ArtifactVersion[] getAllUpdates( VersionRange versionRange, boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( getCurrentVersion(), false, null, false );
        return getVersions( versionRange, restriction, includeSnapshots );
    }

    /**
     * Returns the lower bound version based on the given artifact version
     * and the lowest unchanged segment index (0-based); -1 means that the whole version string can be changed,
     * implying that there is also no string designation of the lower bound version.
     *
     * @param version {@link ArtifactVersion} object specyfing the verion for which the lower bound is being computed
     * @param unchangedSegment first segment not to be changed; empty() means any segment can change
     * @return {@link ArtifactVersion} the lowest artifact version with the given segment held or null if no such
     * version can be found
     * @throws InvalidSegmentException if the requested segment is outside the bounds (less than 1 or greater than
     * the segment count)
     */
    protected ArtifactVersion getLowerBoundArtifactVersion( ArtifactVersion version,
                                                            Optional<Segment> unchangedSegment )
            throws InvalidSegmentException
    {
        Optional<String> lowerBound = getLowerBound( version, unchangedSegment );
        return lowerBound.map( DefaultArtifactVersion::new ).orElse( null );
    }

    /**
     * Returns the string designation of the lower bound version based on the given artifact version
     * and the lowest unchanged segment index (0-based); -1 means that the whole version string can be changed,
     * implying that there is also no string designation of the lower bound version.
     *
     * @param version {@link ArtifactVersion} object specyfing the verion for which the lower bound is being computed
     * @param unchangedSegment first segment not to be changed; empty() means anything can change
     * @return {@link Optional} string containing the lowest artifact version with the given segment held
     * @throws InvalidSegmentException if the requested segment is outside of the bounds (less than 1 or greater than
     * the segment count)
     */
    protected Optional<String> getLowerBound( ArtifactVersion version, Optional<Segment> unchangedSegment )
            throws InvalidSegmentException
    {
        if ( !unchangedSegment.isPresent() )
        {
            return empty();
        }

        int segmentCount = getVersionComparator().getSegmentCount( version );
        if ( unchangedSegment.get().value() > segmentCount )
        {
            throw new InvalidSegmentException( unchangedSegment.get(), segmentCount, version );
        }

        StringBuilder newVersion = new StringBuilder();
        newVersion.append( version.getMajorVersion() );

        if ( segmentCount > 0 )
        {
            newVersion.append( "." ).append( unchangedSegment.get().value() >= 1 ? version.getMinorVersion() : 0 );
        }
        if ( segmentCount > 1 )
        {
            newVersion.append( "." )
                    .append( unchangedSegment.get().value() >= 2 ? version.getIncrementalVersion() : 0 );
        }
        if ( segmentCount > 2 )
        {
            if ( version.getQualifier() != null )
            {
                newVersion.append( "-" ).append( unchangedSegment.get().value() >= 3 ? version.getQualifier() : "0" );
            }
            else
            {
                newVersion.append( "-" ).append( unchangedSegment.get().value() >= 3 ? version.getBuildNumber() : "0" );
            }
        }
        return of( newVersion.toString() );
    }

    /**
     * Checks if the candidate version is in the range of the restriction.
     * a custom comparator is/can be used to have milestones and rcs before final releases,
     * which is not yet possible with {@link Restriction#containsVersion(ArtifactVersion)}.
     * @PARAM RESTRICTION THE RANGE TO CHECK AGAINST.
     * @param candidate the version to check.
     * @return true if the candidate version is within the range of the restriction parameter.
     */
    private boolean isVersionInRestriction( Restriction restriction, ArtifactVersion candidate )
    {
        ArtifactVersion lowerBound = restriction.getLowerBound();
        ArtifactVersion upperBound = restriction.getUpperBound();
        boolean includeLower = restriction.isLowerBoundInclusive();
        boolean includeUpper = restriction.isUpperBoundInclusive();
        final VersionComparator versionComparator = getVersionComparator();
        int lower = lowerBound == null ? -1 : versionComparator.compare( lowerBound, candidate );
        int upper = upperBound == null ? +1 : versionComparator.compare( upperBound, candidate );
        if ( lower > 0 || upper < 0 )
        {
            return false;
        }
        return ( includeLower || lower != 0 ) && ( includeUpper || upper != 0 );
    }

    /**
     * Helper method to get the artifact boundaries for computing updates
     *
     * @param currentVersion The current version.
     * @param scope scope of the restriction or Optional.empty() for no restriction
     * @return {@linkplain Restriction} object based on the arguments
     * @throws InvalidSegmentException if {@code segment} ∉ [0, segmentCount)
     */
    protected Restriction restrictionFor( ArtifactVersion currentVersion, Optional<Segment> scope )
            throws InvalidSegmentException
    {
        VersionComparator versionComparator = getVersionComparator();
        ArtifactVersion lowerBound = scope.isPresent() && scope.get().value() < Segment.SUBINCREMENTAL.value()
                ? versionComparator.incrementSegment( currentVersion, scope.get() )
                : currentVersion;
        ArtifactVersion upperBound = scope.isPresent() && scope.get().value() > Segment.MAJOR.value()
                ? versionComparator.incrementSegment( currentVersion, Segment.of( scope.get().value() - 1 ) )
                : null;
        return new Restriction( lowerBound, lowerBound != currentVersion,
                upperBound, false );
    }
}
