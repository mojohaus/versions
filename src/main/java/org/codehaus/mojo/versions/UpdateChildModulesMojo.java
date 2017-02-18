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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Scans the current projects child modules, updating the versions of any which use the current project to the version
 * of the current project.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-2
 */
@Mojo( name = "update-child-modules", requiresProject = true, requiresDirectInvocation = true, aggregator = true )
public class UpdateChildModulesMojo
    extends AbstractVersionsUpdaterMojo
{

    /**
     * The groupId that we are updating. Guarded by this.
     */
    private transient String sourceGroupId = null;

    /**
     * The artifactId that we are updating. Guarded by this.
     */
    private transient String sourceArtifactId = null;

    /**
     * The version that we are updating to. Guarded by this.
     */
    private transient String sourceVersion = null;

    /**
     * Called when this mojo is executed.
     *
     * @throws MojoExecutionException when things go wrong.
     * @throws MojoFailureException when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {

        boolean didSomething = false;

        try
        {
            final Map<String, Model> reactor = PomHelper.getReactorModels( getProject(), getLog() );
            List<String> order = new ArrayList<String>( reactor.keySet() );
            Collections.sort( order, new Comparator<String>()
            {
                public int compare( String o1, String o2 )
                {
                    Model m1 = (Model) reactor.get( o1 );
                    Model m2 = (Model) reactor.get( o2 );
                    int d1 = PomHelper.getReactorParentCount( reactor, m1 );
                    int d2 = PomHelper.getReactorParentCount( reactor, m2 );
                    if ( d1 < d2 )
                    {
                        return -1;
                    }
                    else if ( d1 > d2 )
                    {
                        return 1;
                    }
                    return 0;
                }
            } );

            for ( String item : order )
            {
                String sourcePath = item;
                Model sourceModel = (Model) reactor.get( sourcePath );

                getLog().debug( sourcePath.length() == 0 ? "Processing root module as parent"
                                : "Processing " + sourcePath + " as a parent." );

                synchronized ( this )
                {
                    sourceGroupId = PomHelper.getGroupId( sourceModel );
                    if ( sourceGroupId == null )
                    {
                        getLog().warn( "Module " + sourcePath + " is missing a groupId." );
                        continue;
                    }
                    sourceArtifactId = PomHelper.getArtifactId( sourceModel );
                    if ( sourceArtifactId == null )
                    {
                        getLog().warn( "Module " + sourcePath + " is missing an artifactId." );
                        continue;
                    }
                    sourceVersion = PomHelper.getVersion( sourceModel );
                    if ( sourceVersion == null )
                    {
                        getLog().warn( "Module " + sourcePath + " is missing a version." );
                        continue;
                    }

                    getLog().debug( "Looking for modules which use "
                        + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + " as their parent" );

                    Map<String, Model> childModels =
                        PomHelper.getChildModels( reactor, sourceGroupId, sourceArtifactId );
                    for ( Entry<String, Model> childModelEntry : childModels.entrySet() )
                    {
                        File moduleDir = new File( getProject().getBasedir(), childModelEntry.getKey() );

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

                        Model targetModel = childModelEntry.getValue();
                        final Parent parent = targetModel.getParent();
                        if ( sourceVersion.equals( parent.getVersion() ) )
                        {
                            getLog().debug( "Module: " + childModelEntry.getKey() + " parent is "
                                + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":"
                                + sourceVersion );
                        }
                        else
                        {
                            getLog().info( "Module: " + childModelEntry.getKey() );
                            getLog().info( "    parent was "
                                + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":"
                                + parent.getVersion() );
                            getLog().info( "    updated to "
                                + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":"
                                + sourceVersion );
                            process( moduleProjectFile );
                            // don't forget to update the cached model
                            targetModel.setVersion( sourceVersion );
                            didSomething = true;
                        }
                    }
                }
            }

        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        if ( !didSomething )
        {
            getLog().info( "All child modules are up to date." );
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
    protected synchronized void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        getLog().debug( "Updating parent to " + sourceVersion );

        if ( PomHelper.setProjectParentVersion( pom, sourceVersion ) )
        {
            getLog().debug( "Made an update to " + sourceVersion );
        }
    }

}
