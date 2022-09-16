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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

/**
 * Displays any updates of the project's parent project
 *
 * @author Stephen Connolly
 * @since 2.2
 */
@Mojo( name = "display-parent-updates", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class DisplayParentUpdatesMojo
    extends AbstractVersionsDisplayMojo
{

    public static final int MESSAGE_LENGTH = 68;

    @Inject
    public DisplayParentUpdatesMojo( RepositorySystem repositorySystem,
                                           MavenProjectBuilder projectBuilder,
                                           ArtifactMetadataSource artifactMetadataSource,
                                           WagonManager wagonManager,
                                           ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    @Override
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();
        if ( getProject().getParent() == null )
        {
            logLine( false, "Project does not have a parent." );
            return;
        }

        if ( reactorProjects.contains( getProject().getParent() ) )
        {
            logLine( false, "Parent project is part of the reactor." );
            return;
        }

        String currentVersion = getProject().getParent().getVersion();
        String version = currentVersion;

        VersionRange versionRange;
        try
        {
            versionRange = VersionRange.createFromVersionSpec( version );
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( "Invalid version range specification: " + version, e );
        }

        Artifact artifact = getHelper().createDependencyArtifact( DependencyBuilder.newBuilder()
                .withGroupId( getProject().getParent().getGroupId() )
                .withArtifactId( getProject().getParent().getArtifactId() )
                .withVersion( version )
                .withType( "pom" )
                .build() );

        ArtifactVersion artifactVersion;
        try
        {
            artifactVersion = findLatestVersion( artifact, versionRange, null, false );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

        if ( artifactVersion == null || currentVersion.equals( artifactVersion.toString() ) )
        {
            logLine( false, "The parent project is the latest version:" );
            StringBuilder buf = new StringBuilder( MESSAGE_LENGTH );
            buf.append( "  " );
            buf.append( getProject().getParent().getGroupId() );
            buf.append( ':' );
            buf.append( getProject().getParent().getArtifactId() );
            buf.append( ' ' );
            int padding = MESSAGE_LENGTH - currentVersion.length();
            while ( buf.length() < padding )
            {
                buf.append( '.' );
            }
            buf.append( ' ' );
            buf.append( currentVersion );
            logLine( false, buf.toString() );
        }
        else
        {
            logLine( false, "The parent project has a newer version:" );
            StringBuilder buf = new StringBuilder( MESSAGE_LENGTH );
            buf.append( "  " );
            buf.append( getProject().getParent().getGroupId() );
            buf.append( ':' );
            buf.append( getProject().getParent().getArtifactId() );
            buf.append( ' ' );
            int padding = MESSAGE_LENGTH - currentVersion.length()
                - artifactVersion.toString().length() - " -> ".length();
            while ( buf.length() < padding )
            {
                buf.append( '.' );
            }
            buf.append( ' ' );
            buf.append( currentVersion );
            buf.append( " -> " );
            buf.append( artifactVersion );
            logLine( false, buf.toString() );
        }
    }

    @Override
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
    }
}
