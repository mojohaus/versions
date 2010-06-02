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
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.codehaus.plexus.util.StringUtils;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Sets the current projects version, updating the details of any child modules as necessary.
 *
 * @author Stephen Connolly
 * @goal set
 * @aggregator
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-beta-1
 */
public class SetMojo
    extends AbstractVersionsUpdaterMojo
{

    /**
     * The new version number to set.
     *
     * @parameter expression="${newVersion}"
     * @since 1.0-beta-1
     */
    private String newVersion;

    /**
     * The groupId of the dependency/module to update.
     *
     * @parameter expression="${groupId}" default-value="${project.groupId}"
     * @since 1.2
     */
    private String groupId;

    /**
     * The artifactId of the dependecy/module to update.
     *
     * @parameter expression="${artifactId}" default-value="${project.artifactId}"
     * @since 1.2
     */
    private String artifactId;

    /**
     * The version of the dependency/module to update.
     *
     * @parameter expression="${oldVersion}" default-value="${project.version}"
     * @since 1.2
     */
    private String oldVersion;

    /**
     * Component used to prompt for input
     *
     * @component
     */
    private Prompter prompter;

    /**
     * The changes to module coordinates. Guarded by this.
     */
    private final transient List sourceChanges = new ArrayList();

    private synchronized void addChange( String groupId, String artifactId, String oldVersion, String newVersion )
    {
        if ( !newVersion.equals( oldVersion ) )
        {
            sourceChanges.add( new Change( groupId, artifactId, oldVersion, newVersion ) );
        }
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        if ( getProject().getOriginalModel().getVersion() == null )
        {
            throw new MojoExecutionException( "Project version is inherited from parent." );
        }

        if ( StringUtils.isEmpty(newVersion) )
        {
            if ( settings.isInteractiveMode() )
            {
                try
                {
                    newVersion = prompter.prompt( "Enter the new version to set",
                            getProject().getOriginalModel().getVersion() );
                }
                catch (PrompterException e)
                {
                    throw new MojoExecutionException(e.getMessage(), e);
                }
            }
            else
            {
                throw new MojoExecutionException( "You must specify the new version, either by using the newVersion " +
                        "property (that is -DnewVersion=... on the command line) or run in interactive mode" );
            }
        }
        if ( StringUtils.isEmpty(newVersion) )
        {
            throw new MojoExecutionException("You must specify the new version, either by using the newVersion " +
                    "property (that is -DnewVersion=... on the command line) or run in interactive mode");
        }

        // this is the triggering change
        addChange( groupId, artifactId, oldVersion, newVersion );

        try
        {
            final MavenProject project =
                PomHelper.getLocalRoot( projectBuilder, getProject(), localRepository, null, getLog() );

            getLog().info( "Local aggregation root: " + project.getBasedir() );
            final Map reactor = PomHelper.getReactorModels( project, getLog() );

            // now fake out the triggering change
            final Model current = PomHelper.getModel( reactor, getProject().getGroupId(), getProject().getArtifactId() )
                ;
            current.setVersion( newVersion );

            final Set files = new LinkedHashSet();
            files.add( getProject().getFile() );

            final List order = new ArrayList( reactor.keySet() );
            Collections.sort( order, new ReactorDepthComparator( reactor ) );

            final Iterator i = order.iterator();
            while ( i.hasNext() )
            {
                final String sourcePath = (String) i.next();
                final Model sourceModel = (Model) reactor.get( sourcePath );

                getLog().debug( sourcePath.length() == 0
                    ? "Processing root module as parent"
                    : "Processing " + sourcePath + " as a parent." );

                final String sourceGroupId = PomHelper.getGroupId( sourceModel );
                if ( sourceGroupId == null )
                {
                    getLog().warn( "Module " + sourcePath + " is missing a groupId." );
                    continue;
                }
                final String sourceArtifactId = PomHelper.getArtifactId( sourceModel );
                if ( sourceArtifactId == null )
                {
                    getLog().warn( "Module " + sourcePath + " is missing an artifactId." );
                    continue;
                }
                final String sourceVersion = PomHelper.getVersion( sourceModel );
                if ( sourceVersion == null )
                {
                    getLog().warn( "Module " + sourcePath + " is missing a version." );
                    continue;
                }

                final File moduleDir = new File( project.getBasedir(), sourcePath );

                final File moduleProjectFile;

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

                files.add( moduleProjectFile );

                getLog().debug(
                    "Looking for modules which use " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) +
                        " as their parent" );

                final Iterator j =
                    PomHelper.getChildModels( reactor, sourceGroupId, sourceArtifactId ).entrySet().iterator();

                while ( j.hasNext() )
                {
                    final Map.Entry target = (Map.Entry) j.next();
                    final String targetPath = (String) target.getKey();
                    final Model targetModel = (Model) target.getValue();
                    final Parent parent = targetModel.getParent();
                    getLog().debug( "Module: " + targetPath );
                    if ( sourceVersion.equals( parent.getVersion() ) )
                    {
                        getLog().debug( "    parent already is " +
                            ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":" + sourceVersion );
                    }
                    else
                    {
                        getLog().debug(
                            "    parent is " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":" +
                                parent.getVersion() );
                        getLog().debug(
                            "    will become " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":" +
                                sourceVersion );
                    }
                    if ( StringUtils.equals( parent.getVersion(), PomHelper.getVersion( targetModel ) ) )
                    {
                        getLog().debug( "    module is " + ArtifactUtils.versionlessKey(
                            PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ) ) + ":"
                            + PomHelper.getVersion( targetModel ) );
                        getLog().debug( "    will become " + ArtifactUtils.versionlessKey(
                            PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ) ) + ":"
                            + sourceVersion );
                        addChange( PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ),
                                   PomHelper.getVersion( targetModel ), sourceVersion );
                        targetModel.setVersion( sourceVersion );
                    }
                    else
                    {
                        getLog().debug( "    module is " + ArtifactUtils.versionlessKey(
                            PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ) ) + ":"
                            + PomHelper.getVersion( targetModel ) );
                    }
                }
            }

            // now process all the updates
            final Iterator k = files.iterator();
            while ( k.hasNext() )
            {
                process( (File) k.next() );
            }

        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    /**
     * Updates the pom file.
     *
     * @param pom The pom file to update.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong.
     * @throws javax.xml.stream.XMLStreamException
     *          when things go wrong.
     */
    protected synchronized void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            Model model = PomHelper.getRawModel( pom );
            boolean gaveInfo = false;
            Iterator i = sourceChanges.iterator();
            while ( i.hasNext() )
            {
                Change change = (Change) i.next();
                if ( change.getGroupId().equals( PomHelper.getGroupId( model ) ) &&
                    change.getArtifactId().equals( PomHelper.getArtifactId( model ) ) )
                {
                    if ( PomHelper.setProjectVersion( pom, change.getNewVersion() ) )
                    {
                        if ( !gaveInfo )
                        {
                            getLog().info( "Processing " + PomHelper.getGroupId( model ) + ":" +
                                PomHelper.getArtifactId( model ) );
                            gaveInfo = true;
                        }
                        getLog().info( "    Updating project " + change.getGroupId() + ":" + change.getArtifactId() );
                        getLog().info(
                            "        from version " + change.getOldVersion() + " to " + change.getNewVersion() );
                    }
                }
                else if ( model.getParent() != null && change.getGroupId().equals( model.getParent().getGroupId() ) &&
                    change.getArtifactId().equals( model.getParent().getArtifactId() ) )
                {
                    if ( PomHelper.setProjectParentVersion( pom, change.getNewVersion() ) )
                    {
                        if ( !gaveInfo )
                        {
                            getLog().info( "Processing " + PomHelper.getGroupId( model ) + ":" +
                                PomHelper.getArtifactId( model ) );
                            gaveInfo = true;
                        }
                        getLog().info( "    Updating parent " + change.getGroupId() + ":" + change.getArtifactId() );
                        getLog().info(
                            "        from version " + change.getOldVersion() + " to " + change.getNewVersion() );
                    }
                }

                if ( PomHelper.setDependencyVersion( pom, change.getGroupId(), change.getArtifactId(),
                                                     change.getOldVersion(), change.getNewVersion() ) )
                {
                    if ( !gaveInfo )
                    {
                        getLog().info(
                            "Processing " + PomHelper.getGroupId( model ) + ":" + PomHelper.getArtifactId( model ) );
                        gaveInfo = true;
                    }
                    getLog().info( "    Updating dependency " + change.getGroupId() + ":" + change.getArtifactId() );
                    getLog().info( "        from version " + change.getOldVersion() + " to " + change.getNewVersion() );
                }
                if ( PomHelper.setPluginVersion( pom, change.getGroupId(), change.getArtifactId(),
                                                 change.getOldVersion(), change.getNewVersion() ) )
                {
                    if ( !gaveInfo )
                    {
                        getLog().info(
                            "Processing " + PomHelper.getGroupId( model ) + ":" + PomHelper.getArtifactId( model ) );
                        gaveInfo = true;
                    }
                    getLog().info( "    Updating plugin " + change.getGroupId() + ":" + change.getArtifactId() );
                    getLog().info( "        from version " + change.getOldVersion() + " to " + change.getNewVersion() );
                }
            }
            if ( gaveInfo )
            {
                getLog().info( "" );
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private static final class Change
    {
        private final String groupId;

        private final String artifactId;

        private final String oldVersion;

        private final String newVersion;

        private Change( String groupId, String artifactId, String oldVersion, String newVersion )
        {
            this.groupId = groupId;
            this.artifactId = artifactId;
            this.oldVersion = oldVersion;
            this.newVersion = newVersion;
        }

        public String getGroupId()
        {
            return groupId;
        }

        public String getArtifactId()
        {
            return artifactId;
        }

        public String getOldVersion()
        {
            return oldVersion;
        }

        public String getNewVersion()
        {
            return newVersion;
        }

        public boolean equals( Object o )
        {
            if ( this == o )
            {
                return true;
            }
            if ( o == null || getClass() != o.getClass() )
            {
                return false;
            }

            Change change = (Change) o;

            if ( artifactId != null ? !artifactId.equals( change.artifactId ) : change.artifactId != null )
            {
                return false;
            }
            if ( groupId != null ? !groupId.equals( change.groupId ) : change.groupId != null )
            {
                return false;
            }
            if ( newVersion != null ? !newVersion.equals( change.newVersion ) : change.newVersion != null )
            {
                return false;
            }
            if ( oldVersion != null ? !oldVersion.equals( change.oldVersion ) : change.oldVersion != null )
            {
                return false;
            }

            return true;
        }

        public int hashCode()
        {
            int result = groupId != null ? groupId.hashCode() : 0;
            result = 31 * result + ( artifactId != null ? artifactId.hashCode() : 0 );
            result = 31 * result + ( oldVersion != null ? oldVersion.hashCode() : 0 );
            result = 31 * result + ( newVersion != null ? newVersion.hashCode() : 0 );
            return result;
        }
    }

    private static class ReactorDepthComparator
        implements Comparator
    {
        private final Map reactor;

        public ReactorDepthComparator( Map reactor )
        {
            this.reactor = reactor;
        }

        public int compare( Object o1, Object o2 )
        {
            final Model m1 = (Model) reactor.get( o1 );
            final Model m2 = (Model) reactor.get( o2 );
            final int d1 = PomHelper.getReactorParentCount( reactor, m1 );
            final int d2 = PomHelper.getReactorParentCount( reactor, m2 );
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
    }
}

