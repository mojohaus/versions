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

import java.io.IOException;
import java.util.Collection;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import static java.util.Collections.singletonList;
import static java.util.Optional.of;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-next-versions", threadSafe = true )
public class UseNextVersionsMojo
    extends UseLatestVersionsMojoBase
{

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseNextVersionsMojo( RepositorySystem repositorySystem,
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
            if ( isProcessingDependencyManagement() )
            {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel( getProject() ).getDependencyManagement();
                if ( dependencyManagement != null )
                {
                    useNextVersions( pom, dependencyManagement.getDependencies() );
                }
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useNextVersions( pom, getProject().getDependencies() );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useNextVersions( pom, singletonList( getParentDependency() ) );
            }
        }
        catch ( ArtifactMetadataRetrievalException | IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useNextVersions( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
            throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        useLatestVersions( pom, dependencies,
                ( dep, versions ) -> of( versions.getNewerVersions( dep.getVersion(), allowSnapshots )[0] ),
                "useNextVersions" );
    }
}
