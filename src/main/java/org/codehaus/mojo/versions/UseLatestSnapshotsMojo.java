package org.codehaus.mojo.versions;

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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.ordering.MajorMinorIncrementalFilter;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any release versions with the latest snapshot version (if it has been deployed).
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "use-latest-snapshots", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseLatestSnapshotsMojo
    extends ParentUpdatingDependencyUpdateMojo
{

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "false" )
    protected boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     * 
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "false" )
    protected boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    protected boolean allowIncrementalUpdates;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    @Override
    Collection<VersionChange> getVersionChanges(Collection<ArtifactIdentifier> artifacts) throws MojoExecutionException, ArtifactMetadataRetrievalException
    {
        final Collection<VersionChange> versionsToChange = new ArrayList<>();

        int segment = determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );
        MajorMinorIncrementalFilter majorMinorIncfilter =
                new MajorMinorIncrementalFilter( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );

        for ( ArtifactIdentifier dep : artifacts )
        {
            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + dep );
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( !versionMatcher.matches() )
            {
                getLog().debug( "Looking for latest snapshot of " + dep );
                Artifact artifact = dep.getArtifact( getProject(), getHelper() );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                ArtifactVersion selectedVersion = new DefaultArtifactVersion( version );

                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                final VersionComparator versionComparator = versions.getVersionComparator();
                final DefaultArtifactVersion lowerBound = new DefaultArtifactVersion( version );
                if ( segment + 1 > versionComparator.getSegmentCount( lowerBound ) )
                {
                    getLog().info( "Ignoring " + dep + " as the version number is too short" );
                    continue;
                }
                ArtifactVersion upperBound =
                    segment >= 0 ? versionComparator.incrementSegment( lowerBound, segment ) : null;
                getLog().info( "Upper bound: " + (upperBound == null ? "none" : upperBound.toString()) );
                ArtifactVersion[] newer = versions.getVersions( lowerBound, upperBound, true, false, false );
                getLog().debug( "Candidate versions " + Arrays.asList( newer ) );

                ArrayList snapshotsOnly = new ArrayList();

                for ( int j = 0; j < newer.length; j++ )
                {
                    String newVersion = newer[j].toString();
                    if ( matchSnapshotRegex.matcher( newVersion ).matches() )
                    {
                        snapshotsOnly.add( newer[j] );
                    }
                }
                getLog().debug( "Snapshot Only versions " + snapshotsOnly.toString() );

                ArtifactVersion[] filteredVersions = majorMinorIncfilter.filter( selectedVersion, (ArtifactVersion[]) snapshotsOnly.toArray( new ArtifactVersion[snapshotsOnly.size()] ) );
                getLog().debug( "Filtered versions " + Arrays.asList( filteredVersions ) );


                if ( filteredVersions.length > 0 )
                {
                    final VersionChange versionChange = new VersionChange( artifact.getGroupId(), artifact.getArtifactId(), version, filteredVersions[filteredVersions.length - 1].toString() );
                    versionsToChange.add( versionChange );
                }
            }
        }
        return versionsToChange;
    }

}
