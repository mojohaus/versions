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
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any release versions with the latest snapshot version (if it has been deployed).
 *
 * @author Stephen Connolly
 * @goal use-latest-snapshots
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-beta-1
 */
public class UseLatestSnapshotsMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @parameter expression="${allowMajorUpdates}" default-value="false"
     * @since 1.0-beta-1
     */
    protected Boolean allowMajorUpdates;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @parameter expression="${allowMajorUpdates}" default-value="false"
     * @since 1.0-beta-1
     */
    protected Boolean allowMinorUpdates;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @parameter expression="${allowMajorUpdates}" default-value="true"
     * @since 1.0-beta-1
     */
    protected Boolean allowIncrementalUpdates;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException
     *          when things go wrong with XML streaming
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
            if ( isProcessingDependencies() )
            {
                useLatestSnapshots( pom, getProject().getDependencies() );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useLatestSnapshots( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        int segment;
        if ( Boolean.TRUE.equals( allowMajorUpdates ) )
        {
            segment = -1;
            getLog().info( "Major version changes allowed" );
        }
        else if ( Boolean.TRUE.equals( allowMinorUpdates ) )
        {
            segment = 0;
            getLog().info( "Minor version changes allowed" );
        }
        else if ( Boolean.TRUE.equals( allowIncrementalUpdates ) )
        {
            segment = 1;
            getLog().info( "Incremental version changes allowed" );
        }
        else
        {
            segment = 2;
            getLog().info( "Subincremental version changes allowed" );
        }

        Iterator i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = (Dependency) i.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( !versionMatcher.matches() )
            {
                getLog().debug( "Looking for latest snapshot of " + toString( dep ) );
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                final VersionComparator versionComparator = versions.getVersionComparator();
                final DefaultArtifactVersion lowerBound = new DefaultArtifactVersion( version );
                if ( segment + 1 > versionComparator.getSegmentCount( lowerBound ) )
                {
                    getLog().info( "Ignoring " + toString( dep ) + " as the version number is too short" );
                    continue;
                }
                ArtifactVersion upperBound = segment >= 0 ? versionComparator.incrementSegment( lowerBound, segment ) : null;
                upperBound = segment >= 0 ? versionComparator.incrementSegment( upperBound, segment ) : null;
                ArtifactVersion[] newer = versions.getVersions( lowerBound, upperBound, true, false, false );
                getLog().debug( "Candidate versions " + Arrays.asList(newer));
                String latestVersion = null;
                for ( int j = 0; j < newer.length; j++ )
                {
                    String newVersion = newer[j].toString();
                    if ( matchSnapshotRegex.matcher( newVersion ).matches() )
                    {
                        latestVersion = newVersion;
                    }
                }
                if ( latestVersion != null )
                {
                    if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                         latestVersion ) )
                    {
                        getLog().info( "Updated " + toString( dep ) + " to version " + latestVersion );
                    }
                }
            }
        }
    }

}