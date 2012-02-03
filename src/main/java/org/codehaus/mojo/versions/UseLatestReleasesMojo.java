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
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any release versions with the latest release version.
 *
 * @author Stephen Connolly
 * @goal use-latest-releases
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UseLatestReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    /**
     * Whether to allow the major version number to be changed.
     *
     * @parameter expression="${allowMajorUpdates}" default-value="true"
     * @since 1.2
     */
    protected Boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * @parameter expression="${allowMinorUpdates}" default-value="true"
     * @since 1.2
     */
    protected Boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * @parameter expression="${allowIncrementalUpdates}" default-value="true"
     * @since 1.2
     */
    protected Boolean allowIncrementalUpdates;

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException
     *          when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useLatestReleases( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( isProcessingDependencies() )
            {
                useLatestReleases( pom, getProject().getDependencies() );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useLatestReleases( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        int segment = determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );

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
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                getLog().debug( "Looking for newer versions of " + toString( dep ) );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                ArtifactVersion[] newer = versions.getNewerVersions( version, segment, false );
                newer = filterVersionsWithIncludes( newer, artifact );
                if ( newer.length > 0 )
                {
                    String newVersion = newer[newer.length - 1].toString();
                    if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                         newVersion ) )
                    {
                        getLog().info( "Updated " + toString( dep ) + " to version " + newVersion );
                    }
                }
            }
        }
    }

    private ArtifactVersion[] filterVersionsWithIncludes( ArtifactVersion[] newer, Artifact artifact )
    {
        List filteredNewer = new ArrayList( newer.length );
        for ( int j = 0; j < newer.length; j++ )
        {
            ArtifactVersion artifactVersion = newer[j];
            Artifact artefactWithNewVersion = new DefaultArtifact( artifact.getGroupId(), artifact.getArtifactId(),
                                                                   VersionRange.createFromVersion(
                                                                       artifactVersion.toString() ),
                                                                   artifact.getScope(), artifact.getType(), null,
                                                                   new DefaultArtifactHandler(), false );
            if ( isIncluded( artefactWithNewVersion ) )
            {
                filteredNewer.add( artifactVersion );
            }
        }
        return (ArtifactVersion[]) filteredNewer.toArray( new ArtifactVersion[filteredNewer.size()] );
    }

}