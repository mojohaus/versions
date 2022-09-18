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

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.ordering.MajorMinorIncrementalFilter;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

import static java.util.Collections.singletonList;

/**
 * Replaces any release versions with the latest release version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-latest-releases", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseLatestReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    private final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "true" )
    protected boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "true" )
    protected boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    protected boolean allowIncrementalUpdates;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseLatestReleasesMojo( RepositorySystem repositorySystem,
                                     MavenProjectBuilder projectBuilder,
                                     ArtifactMetadataSource artifactMetadataSource,
                                     WagonManager wagonManager,
                                     ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
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
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useLatestReleases( pom, getProject().getDependencies() );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useLatestReleases( pom, singletonList( DependencyBuilder.newBuilder()
                        .withGroupId( getProject().getParent().getGroupId() )
                        .withArtifactId( getProject().getParent().getArtifactId() )
                        .withVersion( getProject().getParent().getVersion() )
                        .withType( "pom" )
                        .build() ) );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useLatestReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        Optional<Segment> unchangedSegment = determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates,
                allowIncrementalUpdates );
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
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( !versionMatcher.matches() )
            {
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                ArtifactVersion selectedVersion = new DefaultArtifactVersion( version );
                getLog().debug( "Selected version: " + selectedVersion );

                getLog().debug( "Looking for newer versions of " + toString( dep ) );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                try
                {
                    ArtifactVersion[] newer = versions.getNewerVersions( version, unchangedSegment, false );
                    newer = filterVersionsWithIncludes( newer, artifact );

                    ArtifactVersion[] filteredVersions = majorMinorIncfilter.filter( selectedVersion, newer );
                    if ( filteredVersions.length > 0 )
                    {
                        String newVersion = filteredVersions[filteredVersions.length - 1].toString();
                        if ( getProject().getParent() != null )
                        {
                            if ( artifact.getId().equals( getProject().getParentArtifact().getId() )
                                    && isProcessingParent() )
                            {
                                if ( PomHelper.setProjectParentVersion( pom, newVersion ) )
                                {
                                    getLog().debug( "Made parent update from " + version + " to " + newVersion );
                                }
                            }
                        }
                        if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                newVersion, getProject().getModel() ) )
                        {
                            getLog().info( "Updated " + toString( dep ) + " to version " + newVersion );

                            this.getChangeRecorder().recordUpdate( "useLatestReleases", dep.getGroupId(),
                                    dep.getArtifactId(), version, newVersion );
                        }
                    }
                }
                catch ( InvalidSegmentException e )
                {
                    getLog().warn( String.format( "Skipping the processing of %s:%s:%s due to: %s", dep.getGroupId(),
                            dep.getArtifactId(), dep.getVersion(), e.getMessage() ) );
                }
            }
        }
    }

    private ArtifactVersion[] filterVersionsWithIncludes( ArtifactVersion[] newer, Artifact artifact )
    {
        List<ArtifactVersion> filteredNewer = new ArrayList<>( newer.length );
        for ( int j = 0; j < newer.length; j++ )
        {
            ArtifactVersion artifactVersion = newer[j];
            Artifact artefactWithNewVersion =
                new DefaultArtifact( artifact.getGroupId(), artifact.getArtifactId(),
                                     VersionRange.createFromVersion( artifactVersion.toString() ), artifact.getScope(),
                                     artifact.getType(), null, new DefaultArtifactHandler(), false );
            if ( isIncluded( artefactWithNewVersion ) )
            {
                filteredNewer.add( artifactVersion );
            }
        }
        return filteredNewer.toArray( new ArtifactVersion[filteredNewer.size()] );
    }

}
