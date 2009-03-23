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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.api.PomHelper;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

/**
 * Scans the current projects child modules, updating the versions of any which use the current project to
 * the version of the current project.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-child-modules
 * @aggregator
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-2
 */
public class UpdateChildModulesMojo
    extends AbstractVersionsUpdaterMojo
{

    /**
     * Called when this mojo is executed.
     *
     * @throws MojoExecutionException when things go wrong.
     * @throws MojoFailureException   when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        Set childModules = PomHelper.getAllChildModules( getProject(), getLog() );

        PomHelper.removeMissingChildModules( getLog(), getProject(), childModules );

        Iterator i = childModules.iterator();

        MojoExecutionException pbe = null;

        while ( i.hasNext() )
        {
            String modulePath = (String) i.next();

            File moduleDir = new File( getProject().getBasedir(), modulePath );

            File moduleProjectFile;

            if ( moduleDir.isDirectory() )
            {
                moduleProjectFile = new File( moduleDir, "pom.xml" );
            }
            else
            {
                // i don't think this should ever happen... but just in case
                // the module references the file-name
                moduleProjectFile = moduleDir;
            }

            try
            {
                // the aim of this goal is to fix problems when the project cannot be parsed by Maven
                // so we have to parse the file by hand!
                StringBuffer childPom = readFile( moduleProjectFile );
                ModifiedPomXMLEventReader pom = newModifiedPomXER( childPom );
                Artifact parent = PomHelper.getProjectParent( pom, getHelper() );
                if ( parent == null )
                {
                    getLog().info( "Module: " + modulePath + " does not have a parent" );
                }
                else if ( !getProject().getGroupId().equals( parent.getGroupId() )
                    || !getProject().getArtifactId().equals( parent.getArtifactId() ) )
                {
                    getLog().info( "Module: " + modulePath + " does not use "
                        + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + " as its parent" );
                }
                else if ( getProject().getVersion().equals( parent.getVersion() ) )
                {
                    getLog().info( "Module: " + modulePath + " parent is "
                        + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":"
                        + getProject().getVersion() );
                }
                else
                {
                    getLog().info( "Module: " + modulePath + " parent was "
                        + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":" + parent.getVersion()
                        + ", now " + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":"
                        + getProject().getVersion() );
                    process( moduleProjectFile );
                }
            }
            catch ( XMLStreamException e )
            {
                getLog().debug( "Could not parse " + moduleProjectFile.getPath(), e );
                if ( pbe == null )
                {
                    // save this until we get to the end.
                    pbe = new MojoExecutionException( "Could not parse " + moduleProjectFile.getPath(), e );
                }
            }
            catch ( IOException e )
            {
                getLog().debug( "Could not parse " + moduleProjectFile.getPath(), e );
                if ( pbe == null )
                {
                    // save this until we get to the end.
                    pbe = new MojoExecutionException( "Could not parse " + moduleProjectFile.getPath(), e );
                }
            }
        }

        if ( pbe != null )
        {
            // ok, now throw the first one to blow up.
            throw pbe;
        }

    }

    /**
     * Updates the pom file.
     *
     * @param pom The pom file to update.
     * @throws MojoExecutionException when things go wrong.
     * @throws MojoFailureException   when things go wrong.
     * @throws XMLStreamException     when things go wrong.
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        getLog().debug( "Updating parent to " + getProject().getVersion() );

        if ( PomHelper.setProjectParentVersion( pom, getProject().getVersion() ) )
        {
            getLog().debug( "Made an update to " + getProject().getVersion() );
        }
    }

}
