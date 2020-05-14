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
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.ordering.MajorMinorIncrementalFilter;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
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
    extends AbstractVersionsDependencyUpdaterMojo
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

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useLatestSnapshots( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useLatestSnapshots( pom, getProject().getDependencies() );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                Dependency dependency = new Dependency();
                dependency.setArtifactId( getProject().getParent().getArtifactId() );
                dependency.setGroupId( getProject().getParent().getGroupId() );
                dependency.setVersion( getProject().getParent().getVersion() );
                dependency.setType("pom");
                List list = new ArrayList();
                list.add(dependency);
                useLatestSnapshots( pom, list );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useLatestSnapshots( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        int segment = determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );
        MajorMinorIncrementalFilter majorMinorIncfilter =
                new MajorMinorIncrementalFilter( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );

        for ( Dependency dep : dependencies )
        {
            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            if ( isHandledByProperty( dep ) )
            {
                getLog().debug( "Ignoring dependency with property as version: " + toString( dep ) );
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher(version);
            if ( !versionMatcher.matches() )
            {
                getLog().debug( "Looking for latest snapshot of " + toString( dep ) );
                Artifact artifact = this.toArtifact( dep );
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
                    getLog().info( "Ignoring " + toString( dep ) + " as the version number is too short" );
                    continue;
                }
                ArtifactVersion upperBound =
                    segment >= 0 ? versionComparator.incrementSegment( lowerBound, segment ) : null;
                getLog().info( "Upper bound: " + (upperBound == null ? "none" : upperBound.toString() ) );
                ArtifactVersion[] newer = versions.getVersions( lowerBound, upperBound, true, false, false );
                getLog().debug( "Candidate versions " + Arrays.asList( newer ) );

                String latestVersion = null;
                ArrayList snapshotsOnly = new ArrayList();

                for ( int j = 0; j < newer.length; j++ )
                {
                    String newVersion = newer[j].toString();
                    if ( matchSnapshotRegex.matcher( newVersion ).matches() )
                    {
                        snapshotsOnly.add( newer[j] );
                    }
                }
                getLog().debug("Snapshot Only versions " + snapshotsOnly.toString());

                ArtifactVersion[] filteredVersions = majorMinorIncfilter.filter( selectedVersion,
                                                                                (ArtifactVersion[]) snapshotsOnly.toArray(
                                                                                    new ArtifactVersion[snapshotsOnly.size()] ) );
                getLog().debug( "Filtered versions " + Arrays.asList( filteredVersions ) );


                if ( filteredVersions.length > 0 )
                {
                    latestVersion = filteredVersions[filteredVersions.length - 1].toString();
                    if ( getProject().getParent() != null )
                    {
                        if ( artifact.getId().equals(getProject().getParentArtifact().getId()) && isProcessingParent() )
                        {
                            if ( PomHelper.setProjectParentVersion( pom, latestVersion.toString() ) )
                            {
                                getLog().debug( "Made parent update from " + version + " to " + latestVersion.toString() );
                            }
                        }
                    }

                    if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                         latestVersion, getProject().getModel() ) )
                    {
                        getLog().info( "Updated " + toString( dep ) + " to version " + latestVersion );
                    }
                }
            }
        }
    }

}
