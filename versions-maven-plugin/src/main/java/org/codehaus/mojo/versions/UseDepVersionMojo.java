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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import static java.util.Collections.singletonList;

/**
 * Updates a dependency to a specific version.
 * This can be useful if you have to manage versions for a very large (100+ module) projects where you can’t always use
 * the most up-to-date version of a particular third party component.
 *
 * @author Dan Arcari
 * @since 2.3
 */
@Mojo( name = "use-dep-version", threadSafe = true )
public class UseDepVersionMojo extends AbstractVersionsDependencyUpdaterMojo
{

    /**
     * The exact version to be applied for the included dependencies
     */
    @Parameter( property = "depVersion",
                required = true )
    protected String depVersion;

    /**
     * If set to true, will use whatever version is supplied without attempting to validate that such a version is
     * obtainable from the repository chain.
     */
    @Parameter( property = "forceVersion",
                defaultValue = "false" )
    protected boolean forceVersion;

    @Inject
    public UseDepVersionMojo( RepositorySystem repositorySystem,
                                           MavenProjectBuilder projectBuilder,
                                           ArtifactMetadataSource artifactMetadataSource,
                                           WagonManager wagonManager,
                                           ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    @Override
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {

        if ( depVersion == null || depVersion.equals( "" ) )
        {
            throw new IllegalArgumentException(
                "depVersion must be supplied with use-specific-version, and cannot be blank." );
        }

        if ( !forceVersion && !hasIncludes() )
        {
            throw new IllegalArgumentException(
                "The use-specific-version goal is intended to be used with a single artifact. "
                    + "Please specify a value for the 'includes' parameter, "
                    + "or use -DforceVersion=true to override this check." );
        }

        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useDepVersion( pom, getProject().getDependencyManagement().getDependencies() );
            }

            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useDepVersion( pom, getProject().getDependencies() );
            }

            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useDepVersion( pom, singletonList( getParentDependency() ) );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useDepVersion( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws MojoExecutionException, XMLStreamException, ArtifactMetadataRetrievalException
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

            Artifact artifact = this.toArtifact( dep );

            if ( isIncluded( artifact ) )
            {
                if ( !forceVersion )
                {
                    ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );

                    if ( !versions.containsVersion( depVersion ) )
                    {
                        throw new MojoExecutionException(
                            String.format( "Version %s is not available for artifact %s:%s",
                                           depVersion, artifact.getGroupId(), artifact.getArtifactId() ) );
                    }
                }
                updateDependencyVersion( pom, dep, depVersion, "useDependencyVersion" );
            }
        }
    }
}
