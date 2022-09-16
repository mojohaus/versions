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

import java.util.Collection;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

/**
 * Replaces any -SNAPSHOT versions with the corresponding release version (if it has been released).
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-releases", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    /**
     * Whether to check for releases within the range.
     *
     * @since 2.3
     */
    @Parameter( property = "allowRangeMatching", defaultValue = "false" )
    private boolean allowRangeMatching;

    /**
     * Whether to fail if a SNAPSHOT could not be replaced
     *
     * @since 2.3
     */
    @Parameter( property = "failIfNotReplaced", defaultValue = "false" )
    private boolean failIfNotReplaced;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    private final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseReleasesMojo( RepositorySystem repositorySystem,
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
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useReleases( pom, getProject().getParent() );
            }

            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useReleases( pom, PomHelper.readImportedPOMsFromDependencyManagementSection( pom ) );
                useReleases( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useReleases( pom, getProject().getDependencies() );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useReleases( ModifiedPomXMLEventReader pom, MavenProject project )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        String version = project.getVersion();
        Matcher versionMatcher = matchSnapshotRegex.matcher( version );
        if ( versionMatcher.matches() )
        {
            String releaseVersion = versionMatcher.group( 1 );

            final MavenProject parent = getProject().getParent();

            Artifact artifact = getHelper().createDependencyArtifact( DependencyBuilder.newBuilder()
                    .withGroupId( parent.getGroupId() )
                    .withArtifactId( parent.getArtifactId() )
                    .withVersion( releaseVersion )
                    .withType( "pom" )
                    .build() );
            if ( !isIncluded( artifact ) )
            {
                return;
            }

            getLog().debug( "Looking for a release of " + toString( project ) );
            // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
            // retrieve release version if provided snapshot version.
            artifact.setVersion( releaseVersion );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            if ( !allowRangeMatching ) // standard behaviour
            {
                if ( versions.containsVersion( releaseVersion ) )
                {
                    if ( PomHelper.setProjectParentVersion( pom, releaseVersion ) )
                    {
                        getLog().info( "Updated " + toString( project ) + " to version " + releaseVersion );
                    }
                }
                else if ( failIfNotReplaced )
                {
                    throw new NoSuchElementException( "No matching release of " + toString( project )
                                                          + " found for update." );
                }
            }
            else
            {
                ArtifactVersion finalVersion = null;
                for ( ArtifactVersion proposedVersion : versions.getVersions( false ) )
                {
                    if ( proposedVersion.toString().startsWith( releaseVersion ) )
                    {
                        getLog().debug( "Found matching version for " + toString( project ) + " to version "
                                            + releaseVersion );
                        finalVersion = proposedVersion;
                    }
                }

                if ( finalVersion != null )
                {
                    if ( PomHelper.setProjectParentVersion( pom, finalVersion.toString() ) )
                    {
                        getLog().info( "Updated " + toString( project ) + " to version " + finalVersion );

                        this.getChangeRecorder().recordUpdate( "useReleases", parent.getGroupId(),
                                                               parent.getArtifactId(), version,
                                                               finalVersion.toString() );
                    }
                }
                else
                {
                    getLog().info( "No matching release of " + toString( project ) + " to update via rangeMatching." );
                    if ( failIfNotReplaced )
                    {
                        throw new NoSuchElementException( "No matching release of " + toString( project )
                                                              + " found for update via rangeMatching." );
                    }
                }

            }
        }
    }

    private void useReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
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
            if ( version == null )
            {
                getLog().info( "Ignoring dependency with no version: " + toString( dep ) );
                continue;
            }
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.matches() )
            {
                String releaseVersion = versionMatcher.group( 1 );
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                getLog().debug( "Looking for a release of " + toString( dep ) );
                // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
                // retrieve release version if provided snapshot version.
                artifact.setVersion( releaseVersion );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                if ( !allowRangeMatching ) // standard behaviour
                {
                    noRangeMatching( pom, dep, version, releaseVersion, versions );
                }
                else
                {
                    rangeMatching( pom, dep, version, releaseVersion, versions );
                }
            }
        }
    }

    private void rangeMatching( ModifiedPomXMLEventReader pom, Dependency dep, String version, String releaseVersion,
                                ArtifactVersions versions ) throws XMLStreamException, MojoExecutionException
    {
        ArtifactVersion finalVersion = null;
        for ( ArtifactVersion proposedVersion : versions.getVersions( false ) )
        {
            if ( proposedVersion.toString().startsWith( releaseVersion ) )
            {
                getLog().debug( "Found matching version for " + toString( dep ) + " to version " + releaseVersion );
                finalVersion = proposedVersion;
            }
        }

        if ( finalVersion != null )
        {
            if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                 finalVersion.toString(), getProject().getModel() ) )
            {
                getLog().info( "Updated " + toString( dep ) + " to version " + finalVersion );

                this.getChangeRecorder().recordUpdate( "useReleases", dep.getGroupId(),
                                                       dep.getArtifactId(), version, finalVersion.toString() );
            }
        }
        else
        {
            getLog().info( "No matching release of " + toString( dep ) + " to update via rangeMatching." );
            if ( failIfNotReplaced )
            {
                throw new NoSuchElementException( "No matching release of " + toString( dep )
                                                      + " found for update via rangeMatching." );
            }
        }
    }

    private void noRangeMatching( ModifiedPomXMLEventReader pom, Dependency dep, String version, String releaseVersion,
                                  ArtifactVersions versions ) throws XMLStreamException, MojoExecutionException
    {
        if ( versions.containsVersion( releaseVersion ) )
        {
            if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version, releaseVersion,
                                                 getProject().getModel() ) )
            {
                getLog().info( "Updated " + toString( dep ) + " to version " + releaseVersion );

                this.getChangeRecorder().recordUpdate( "useReleases", dep.getGroupId(),
                                                       dep.getArtifactId(), version, releaseVersion );
            }
        }
        else if ( failIfNotReplaced )
        {
            throw new NoSuchElementException( "No matching release of " + toString( dep ) + " found for update." );
        }
    }

}
