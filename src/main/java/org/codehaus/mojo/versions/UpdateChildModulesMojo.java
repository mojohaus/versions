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
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Profile;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.Collection;
import java.util.regex.Pattern;

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
     * @throws MojoFailureException when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        Set childModules = getAllChildModules( getProject() );

        removeMissingChildModules( getProject(), childModules );

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
                Stack stack = new Stack();
                String path = "";
                String groupId = null;
                String artifactId = null;
                String version = null;
                Pattern pattern = Pattern.compile( "/project/parent/(groupId|artifactId|version)" );
                while ( pom.hasNext() )
                {
                    XMLEvent event = pom.nextEvent();
                    if ( event.isStartDocument() )
                    {
                        path = "";
                        stack.clear();
                    }
                    else if ( event.isStartElement() )
                    {
                        stack.push( path );

                        path = path + "/" + event.asStartElement().getName().getLocalPart();

                        if ( pattern.matcher( path ).matches() )
                        {
                            String text = pom.getElementText().trim();
                            if ( path.endsWith( "groupId" ) )
                            {
                                groupId = text;
                            }
                            else if ( path.endsWith( "artifactId" ) )
                            {
                                artifactId = text;
                            }
                            else if ( path.endsWith( "version" ) )
                            {
                                version = text;
                            }
                            path = (String) stack.pop();
                        }
                    }
                    else if ( event.isEndElement() )
                    {
                        if ( "/project/parent".equals( path ) )
                        {
                            getLog().info( "Module: " + modulePath );

                            if ( getProject().getGroupId().equals( groupId ) && getProject().getArtifactId().equals(
                                artifactId ) )
                            {

                                if ( getProject().getVersion().equals( version ) )
                                {
                                    getLog().info( "  Parent is "
                                        + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":"
                                        + getProject().getVersion() );
                                }
                                else
                                {
                                    getLog().info( "  Parent was "
                                        + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":" + version
                                        + ", now " + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + ":"
                                        + getProject().getVersion() );
                                    process( moduleProjectFile );
                                }
                            }
                            else
                            {
                                getLog().info( "  does not use "
                                    + ArtifactUtils.versionlessKey( getProject().getArtifact() ) + " as its parent" );
                            }
                        }
                        path = (String) stack.pop();
                    }
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
     * @throws MojoFailureException when things go wrong.
     * @throws XMLStreamException when things go wrong.
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        getLog().debug( "Updating parent to " + getProject().getVersion() );

        Stack stack = new Stack();
        String path = "";

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer()
                    .append( path )
                    .append( "/" )
                    .append( event.asStartElement().getName().getLocalPart() )
                    .toString();

                if ( "/project/parent/version".equals( path ) )
                {
                    pom.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( "/project/parent/version".equals( path ) )
                {
                    pom.mark( 1 );
                    if ( pom.hasMark( 0 ) )
                    {
                        pom.replaceBetween( 0, 1, getProject().getVersion() );
                        getLog().debug( "Made an update to " + getProject().getVersion() );
                        return;
                    }
                }
                path = (String) stack.pop();
            }
        }
    }

    /**
     * Returns a set of all child modules for a project, including any defined in profiles (ignoring profile
     * activation).
     *
     * @param project The project.
     * @return the set of all child modules of the project.
     */
    private Set getAllChildModules( MavenProject project )
    {
        getLog().debug( "Finding child modules..." );
        Set childModules = new TreeSet();
        childModules.addAll( project.getOriginalModel().getModules() );
        Iterator i = project.getOriginalModel().getProfiles().iterator();
        while ( i.hasNext() )
        {
            Profile profile = (Profile) i.next();
            childModules.addAll( profile.getModules() );
        }
        debugModules( "Child modules:", childModules );
        return childModules;
    }

    /**
     * Outputs a debug message with a list of modules.
     *
     * @param message The message to display.
     * @param modules The modules to append to the message.
     */
    private void debugModules( String message, Collection modules )
    {
        Iterator i;
        if ( getLog().isDebugEnabled() )
        {
            getLog().debug( message );
            if ( modules.isEmpty() )
            {
                getLog().debug( "None." );
            }
            else
            {
                i = modules.iterator();
                while ( i.hasNext() )
                {
                    getLog().debug( "  " + i.next() );
                }
            }

        }
    }

    /**
     * Modifies the collection of child modules removing those which cannot be found relative to the parent.
     *
     * @param project the project.
     * @param childModules the child modules.
     */
    private void removeMissingChildModules( MavenProject project, Collection childModules )
    {
        getLog().debug( "Removing child modules which are missing..." );
        Iterator i = childModules.iterator();
        while ( i.hasNext() )
        {
            String modulePath = (String) i.next();
            File moduleFile = new File( project.getBasedir(), modulePath );

            if ( moduleFile.isDirectory() && new File(moduleFile, "pom.xml").isFile())
            {
                // it's a directory that exists
                continue;
            }

            if ( moduleFile.isFile())
            {
                // it's the pom.xml file directly referenced and it exists.
                continue;
            }

            getLog().debug( "Removing missing child module " + modulePath );
            i.remove();
        }
        debugModules( "After removing missing", childModules );
    }
}
