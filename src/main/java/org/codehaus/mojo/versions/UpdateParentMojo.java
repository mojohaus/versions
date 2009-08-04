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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;

/**
 * Sets the parent version to the latest parent version.
 *
 * @author Stephen Connolly
 * @goal update-parent
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-1
 */
public class UpdateParentMojo
    extends AbstractVersionsUpdaterMojo
{

// ------------------------------ FIELDS ------------------------------

    /**
     * Version specification to control artifact resolution.
     *
     * @parameter expression="${parentVersion}"
     * @since 1.0-alpha-1
     */
    protected String parentVersion = null;

// -------------------------- OTHER METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( getProject().getParent() == null )
        {
            getLog().info( "Project does not have a parent" );
            return;
        }

        if ( reactorProjects.contains( getProject().getParent() ) )
        {
            getLog().info( "Project's parent is part of the reactor" );
            return;
        }

        String currentVersion = getProject().getParent().getVersion();
        String version = currentVersion;

        if ( parentVersion != null )
        {
            version = parentVersion;
        }

        VersionRange versionRange;
        try
        {
            versionRange = VersionRange.createFromVersionSpec( version );
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( "Invalid version range specification: " + version, e );
        }

        Artifact artifact = artifactFactory.createDependencyArtifact( getProject().getParent().getGroupId(),
                                                                      getProject().getParent().getArtifactId(),
                                                                      versionRange, "pom", null, null );

        ArtifactVersion artifactVersion;
        try
        {
            artifactVersion = findLatestVersion( artifact, versionRange, null, false );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

        if ( !shouldApplyUpdate( artifact, currentVersion, artifactVersion ) )
        {
            return;
        }

        getLog().info( "Updating parent from " + currentVersion + " to " + artifactVersion.toString() );

        if ( PomHelper.setProjectParentVersion( pom, artifactVersion.toString() ) )
        {
            getLog().debug( "Made an update from " + currentVersion + " to " + artifactVersion.toString() );
        }
    }

}