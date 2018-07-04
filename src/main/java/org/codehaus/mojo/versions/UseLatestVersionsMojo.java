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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.ordering.MajorMinorIncrementalFilter;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-latest-versions", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseLatestVersionsMojo
    extends ParentUpdatingDependencyUpdateMojo
{
    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "true" )
    private boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "true" )
    private boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    private boolean allowIncrementalUpdates;

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    @Override
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                final DependencyManagement dependencyManagement =
                    PomHelper.getRawModel( getProject() ).getDependencyManagement();
                if ( dependencyManagement != null )
                {
                    setDependencyVersions( pom, dependencyManagement.getDependencies() );
                }
            }
            else
            {
                super.update( pom );
            }
        }
        catch ( ArtifactMetadataRetrievalException | IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

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
                getLog().info( "Ignoring reactor dependency: " + dep.toString() );
                continue;
            }

            String version = dep.getVersion();
            Artifact artifact = dep.getArtifact( getProject(), getHelper() );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            ArtifactVersion selectedVersion = new DefaultArtifactVersion( version );
            getLog().debug( "Selected version:" + selectedVersion.toString() );

            getLog().debug( "Looking for newer versions of " + dep );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );

            ArtifactVersion[] newerVersions = versions.getNewerVersions( version, segment, allowSnapshots );

            ArtifactVersion[] filteredVersions = majorMinorIncfilter.filter( selectedVersion, newerVersions );
            if ( filteredVersions.length > 0 )
            {
                final VersionChange versionChange = new VersionChange( artifact.getGroupId(), artifact.getArtifactId(), version, filteredVersions[filteredVersions.length - 1].toString() );
                versionsToChange.add( versionChange );
            }
        }
        return versionsToChange;
    }

}
