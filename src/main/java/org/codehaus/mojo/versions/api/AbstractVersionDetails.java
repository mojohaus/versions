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
import java.util.Optional;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.BoundArtifactVersion;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import static java.util.Collections.reverseOrder;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

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

    @Override
    public final boolean isCurrentVersionDefined()
    {
        return getCurrentVersion() != null;
    }

    @Override
    public final ArtifactVersion getCurrentVersion()
    {
        synchronized ( currentVersionLock )
        {
            return currentVersion;
        }
    }

    @Override
    public final void setCurrentVersion( ArtifactVersion currentVersion )
    {
        synchronized ( currentVersionLock )
        {
            this.currentVersion = currentVersion;
        }
    }

    @Override
    public final void setCurrentVersion( String currentVersion )
    {
        setCurrentVersion( currentVersion == null ? null : new DefaultArtifactVersion( currentVersion ) );
    }

    @Override
    public final boolean isIncludeSnapshots()
    {
        synchronized ( currentVersionLock )
        {
            return includeSnapshots;
        }
    }

    @Override
    public final void setIncludeSnapshots( boolean includeSnapshots )
    {
        synchronized ( currentVersionLock )
        {
            this.includeSnapshots = includeSnapshots;
        }
    }

    @Override
    public final ArtifactVersion[] getVersions()
    {
        return getVersions( isIncludeSnapshots() );
    }

    @Override
    public abstract ArtifactVersion[] getVersions( boolean includeSnapshots );

    @Override
    public final ArtifactVersion[] getVersions( VersionRange versionRange, boolean includeSnapshots )
    {
        return getVersions( versionRange, null, includeSnapshots );
    }

    @Override
    public final ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getVersions( lowerBound, upperBound, isIncludeSnapshots() );
    }

    @Override
    public final ArtifactVersion[] getVersions( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( lowerBound, false, upperBound, false );
        return getVersions( restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound )
    {
        return getNewestVersion( lowerBound, upperBound, isIncludeSnapshots() );
    }

    @Override
    public final ArtifactVersion getNewestVersion( ArtifactVersion lowerBound, ArtifactVersion upperBound,
                                                   boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( lowerBound, false, upperBound, false );
        return getNewestVersion( restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction,
                                                   boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, restriction, includeSnapshots, false );
    }

    @Override
    public final ArtifactVersion getNewestVersion( VersionRange versionRange, Restriction restriction,
                                                   boolean includeSnapshots, boolean allowDowngrade )
    {
        // reverseOrder( getVersions( ... ) ) will contain versions sorted from latest to oldest,
        // so we only need to find the first candidate fulfilling the criteria
        return Arrays.stream( getVersions( includeSnapshots ) )
                .sorted( reverseOrder() )
                .filter( candidate -> allowDowngrade || versionRange == null
                        || ArtifactVersions.isVersionInRange( candidate, versionRange ) )
                .filter( candidate -> restriction == null || isVersionInRestriction( restriction, candidate ) )
                .filter( candidate -> includeSnapshots || !ArtifactUtils.isSnapshot( candidate.toString() ) )
                .findAny()
                .orElse( null );
    }

    @Override
    public final ArtifactVersion getNewestVersion( Restriction restriction, boolean includeSnapshots )
    {
        return getNewestVersion( null, restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion getNewestVersion( VersionRange versionRange, boolean includeSnapshots )
    {
        return getNewestVersion( versionRange, null, includeSnapshots );
    }

    @Override
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

    private ArtifactVersion[] getNewerVersions( ArtifactVersion version, boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( version, false, null, false );
        return getVersions( restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion[] getNewerVersions( String version, boolean includeSnapshots )
    {
        return getNewerVersions( new DefaultArtifactVersion( version ), includeSnapshots );
    }

    @Deprecated
    @Override
    public final ArtifactVersion[] getNewerVersions( String version, Optional<Segment> upperBoundSegment,
                                                     boolean includeSnapshots )
            throws InvalidSegmentException
    {
        return getNewerVersions( version, upperBoundSegment, includeSnapshots, false );
    }

    @Override
    public final ArtifactVersion[] getNewerVersions( String versionString, Optional<Segment> unchangedSegment,
                                                     boolean includeSnapshots, boolean allowDowngrade )
            throws InvalidSegmentException
    {
        ArtifactVersion currentVersion = new DefaultArtifactVersion( versionString );
        ArtifactVersion lowerBound = allowDowngrade
                ? getLowerBound( currentVersion, unchangedSegment )
                    .map( DefaultArtifactVersion::new )
                    .orElse( null )
                : currentVersion;
        ArtifactVersion upperBound =
                unchangedSegment
                        .map( s -> (ArtifactVersion) new BoundArtifactVersion( currentVersion,
                                        s.isMajorTo( SUBINCREMENTAL )
                                                ? Segment.of( s.value() + 1 )
                                                : s ) )
                        .orElse( null );

        Restriction restriction = new Restriction( lowerBound, allowDowngrade, upperBound, allowDowngrade );
        return getVersions( restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion[] getVersions( Restriction restriction, boolean includeSnapshots )
    {
        return getVersions( null, restriction, includeSnapshots );
    }

    @Override
    public final ArtifactVersion[] getVersions( VersionRange versionRange, Restriction restriction,
                                                boolean includeSnapshots )
    {
        return Arrays.stream( getVersions( includeSnapshots ) )
                .filter( candidate ->
                        versionRange == null || ArtifactVersions.isVersionInRange( candidate, versionRange ) )
                .filter( candidate -> restriction == null || isVersionInRestriction( restriction, candidate ) )
                .filter( candidate -> includeSnapshots || !ArtifactUtils.isSnapshot( candidate.toString() ) )
                .sorted( getVersionComparator() )
                .distinct()
                .toArray( ArtifactVersion[]::new );
    }

    @Override
    public final ArtifactVersion getNewestUpdate( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                                  boolean includeSnapshots )
    {
        try
        {
            return getNewestVersion( getVersionComparator().restrictionFor( currentVersion, updateScope ),
                    includeSnapshots );
        }
        catch ( InvalidSegmentException e )
        {
            return null;
        }
    }

    @Override
    public final ArtifactVersion[] getAllUpdates( ArtifactVersion currentVersion, Optional<Segment> updateScope,
                                                  boolean includeSnapshots )
    {
        try
        {
            return getVersions( getVersionComparator().restrictionFor( currentVersion, updateScope ),
                    includeSnapshots );
        }
        catch ( InvalidSegmentException e )
        {
            return null;
        }
    }

    @Override
    public final ArtifactVersion getNewestUpdate( Optional<Segment> updateScope )
    {
        return getNewestUpdate( updateScope, isIncludeSnapshots() );
    }

    @Override
    public final ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope )
    {
        return getAllUpdates( updateScope, isIncludeSnapshots() );
    }

    @Override
    public final ArtifactVersion getNewestUpdate( Optional<Segment> updateScope, boolean includeSnapshots )
    {
        if ( isCurrentVersionDefined() )
        {
            return getNewestUpdate( getCurrentVersion(), updateScope, includeSnapshots );
        }
        return null;
    }

    @Override
    public final ArtifactVersion[] getAllUpdates( Optional<Segment> updateScope, boolean includeSnapshots )
    {
        if ( isCurrentVersionDefined() )
        {
            return getAllUpdates( getCurrentVersion(), updateScope, includeSnapshots );
        }
        return null;
    }

    @Override
    public final ArtifactVersion[] getAllUpdates()
    {
        return getAllUpdates( (VersionRange) null, isIncludeSnapshots() );
    }

    @Override
    public final ArtifactVersion[] getAllUpdates( VersionRange versionRange )
    {
        return getAllUpdates( versionRange, isIncludeSnapshots() );
    }

    @Override
    public ArtifactVersion[] getAllUpdates( VersionRange versionRange, boolean includeSnapshots )
    {
        Restriction restriction = new Restriction( getCurrentVersion(), false, null, false );
        return getVersions( versionRange, restriction, includeSnapshots );
    }

    /**
     * Returns the string designation of the lower bound version based on the given artifact version
     * and the lowest unchanged segment index (0-based); -1 means that the whole version string can be changed,
     * implying that there is also no string designation of the lower bound version.
     *
     * @param version {@link ArtifactVersion} object specifying the version for which the lower bound is being computed
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
     * @param restriction the range to check against.
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
}
