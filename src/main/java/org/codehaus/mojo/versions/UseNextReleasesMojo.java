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
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Replaces any release versions with the next release version (if it has been released).
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-next-releases", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseNextReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    private static final Pattern MATCH_SNAPSHOT_REGEX = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseNextReleasesMojo( RepositorySystem repositorySystem,
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
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
        if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
        {
            useNextReleases( pom, getProject().getDependencyManagement().getDependencies() );
        }
        if ( getProject().getDependencies() != null && isProcessingDependencies() )
        {
            useNextReleases( pom, getProject().getDependencies() );
        }
    }

    private void useNextReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
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
            Matcher versionMatcher = MATCH_SNAPSHOT_REGEX.matcher( version );
            if ( !versionMatcher.matches() )
            {
                getLog().debug( "Looking for newer versions of " + toString( dep ) );
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                ArtifactVersion[] newer = versions.getNewerVersions( version, false );
                if ( newer.length > 0 )
                {
                    String newVersion = newer[0].toString();
                    if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                         newVersion, getProject().getModel() ) )

                    {
                        getLog().info( "Updated " + toString( dep ) + " to version " + newVersion );

                        this.getChangeRecorder().recordUpdate( "useNextReleases", dep.getGroupId(),
                                dep.getArtifactId(), version, newVersion );
                    }
                }
            }
        }
    }

}
