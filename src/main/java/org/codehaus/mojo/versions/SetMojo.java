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
import org.codehaus.mojo.versions.change.VersionChange;
import org.codehaus.mojo.versions.change.VersionChanger;
import org.codehaus.mojo.versions.change.VersionChangerFactory;
import org.codehaus.mojo.versions.ordering.ReactorDepthComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.ContextualLog;
import org.codehaus.mojo.versions.utils.DelegatingContextualLog;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.codehaus.plexus.util.StringUtils;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Pattern;

/**
 * Sets the current project's version and based on that change propagates that change onto any child modules as
 * necessary.
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
     * @parameter property="newVersion"
     * @since 1.0-beta-1
     */
    private String newVersion;

    /**
     * The groupId of the dependency/module to update.
     *
     * @parameter property="groupId" default-value="${project.groupId}"
     * @since 1.2
     */
    private String groupId;

    /**
     * The artifactId of the dependency/module to update.
     *
     * @parameter property="artifactId" default-value="${project.artifactId}"
     * @since 1.2
     */
    private String artifactId;

    /**
     * The version of the dependency/module to update.
     *
     * @parameter property="oldVersion" default-value="${project.version}"
     * @since 1.2
     */
    private String oldVersion;

    /**
     * Whether matching versions explicitly specified (as /project/version) in child modules should be updated.
     *
     * @parameter property="updateMatchingVersions" default-value="true"
     * @since 1.3
     */
    private Boolean updateMatchingVersions;

    /**
     * Whether to process the parent of the project. If not set will default to true.
     *
     * @parameter property="processParent" default-value="true"
     * @since 1.3
     */
    private boolean processParent;

    /**
     * Whether to process the project version. If not set will default to true.
     *
     * @parameter property="processProject" default-value="true"
     * @since 1.3
     */
    private boolean processProject;

    /**
     * Whether to process the dependencies section of the project. If not set will default to true.
     *
     * @parameter property="processDependencies" default-value="true"
     * @since 1.3
     */
    private boolean processDependencies;

    /**
     * Whether to process the plugins section of the project. If not set will default to true.
     *
     * @parameter property="processPlugins" default-value="true"
     * @since 1.3
     */
    private boolean processPlugins;

    /**
     * Component used to prompt for input
     *
     * @component
     */
    private Prompter prompter;

    /**
     * Whether to remove -SNAPSHOT from the existing version. If not set will default to false.
     *
     * @parameter property="removeSnapshot" default-value="false"
     * @since 2.10
     */
    private boolean removeSnapshot;

    /**
     * Whether to add next version number and -SNAPSHOT to the existing version. If not set will default to false.
     *
     * @parameter property="nextSnapshot" default-value="false"
     * @since 2.10
     */
    private boolean nextSnapshot;

    /**
     * The changes to module coordinates. Guarded by this.
     */
    private final transient List<VersionChange> sourceChanges = new ArrayList<VersionChange>();

    private synchronized void addChange( String groupId, String artifactId, String oldVersion, String newVersion )
    {
        if ( !newVersion.equals( oldVersion ) )
        {
            sourceChanges.add( new VersionChange( groupId, artifactId, oldVersion, newVersion ) );
        }
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong.
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        if ( updateMatchingVersions == null )
        {
            updateMatchingVersions = Boolean.TRUE;
        }

        if ( getProject().getOriginalModel().getVersion() == null )
        {
            throw new MojoExecutionException( "Project version is inherited from parent." );
        }

        if ( removeSnapshot == true && nextSnapshot == false ) {
            String version = getVersion();
            String release = version;
            if (version.indexOf("-SNAPSHOT") > -1) {
                release = version.substring(0, version.indexOf(
                        "-SNAPSHOT"));
                newVersion = release;
                getLog().info("SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion);
            }
        }

        if ( removeSnapshot == false && nextSnapshot == true ) {
            String version = getVersion();
            String versionWithoutSnapshot = version;
            if (version.indexOf("-SNAPSHOT") > -1) {
                versionWithoutSnapshot = version.substring(0, version.indexOf("-SNAPSHOT"));
            }
            String subversion = versionWithoutSnapshot.substring(0, versionWithoutSnapshot.length() - 3);

            // 1.0-b1-002
            String currentVersionStr = versionWithoutSnapshot.substring(versionWithoutSnapshot.length() - 3);
            getLog().info("VER: " + currentVersionStr);
            int currentVersion = Integer.parseInt(currentVersionStr);
            int nextVersion = currentVersion + 1;
            String nextVersionStr = ("000" + nextVersion);
            nextVersionStr = nextVersionStr.substring(nextVersionStr.length() - 3);
            newVersion = subversion + nextVersionStr + "-SNAPSHOT";
            getLog().info("SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion);
        }

        if ( StringUtils.isEmpty( newVersion ) )
        {
            if ( settings.isInteractiveMode() )
            {
                try
                {
                    newVersion =
                        prompter.prompt( "Enter the new version to set", getProject().getOriginalModel().getVersion() );
                }
                catch ( PrompterException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
            else
            {
                throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                    + "property (that is -DnewVersion=... on the command line) or run in interactive mode" );
            }
        }
        if ( StringUtils.isEmpty( newVersion ) )
        {
            throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                + "property (that is -DnewVersion=... on the command line) or run in interactive mode" );
        }

        try
        {
            final MavenProject project =
                PomHelper.getLocalRoot( projectBuilder, getProject(), localRepository, null, getLog() );

            getLog().info( "Local aggregation root: " + project.getBasedir() );
            Map<String, Model> reactorModels = PomHelper.getReactorModels( project, getLog() );
            final SortedMap<String, Model> reactor =
                new TreeMap<String, Model>( new ReactorDepthComparator( reactorModels ) );
            reactor.putAll( reactorModels );

            // set of files to update
            final Set<File> files = new LinkedHashSet<File>();

            getLog().info( "Processing change of " + groupId + ":" + artifactId + ":" + oldVersion + " -> "
                + newVersion );
            Pattern groupIdRegex =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( fixNullOrEmpty( groupId, "*" ), true ) );
            Pattern artifactIdRegex =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( fixNullOrEmpty( artifactId, "*" ), true ) );
            Pattern oldVersionIdRegex =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( fixNullOrEmpty( oldVersion, "*" ), true ) );
            boolean found = false;
            for ( Model m : reactor.values() )
            {
                final String mGroupId = PomHelper.getGroupId( m );
                final String mArtifactId = PomHelper.getArtifactId( m );
                final String mVersion = PomHelper.getVersion( m );
                if ( groupIdRegex.matcher( mGroupId ).matches() && artifactIdRegex.matcher( mArtifactId ).matches()
                    && oldVersionIdRegex.matcher( mVersion ).matches() && !newVersion.equals( mVersion ) )
                {
                    found = true;
                    // if the change is not one we have swept up already
                    applyChange( project, reactor, files, mGroupId, m.getArtifactId(),
                                 StringUtils.isBlank( oldVersion ) || "*".equals( oldVersion ) ? "" : m.getVersion() );
                }
            }
            if ( !found && RegexUtils.getWildcardScore(groupId) == 0
            		&& RegexUtils.getWildcardScore(artifactId) == 0
            		&& RegexUtils.getWildcardScore(oldVersion) == 0 )
            {
                applyChange( project, reactor, files, groupId, artifactId, oldVersion );
            }

            // now process all the updates
            for ( File file : files )
            {
                process( file );
            }

        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private static String fixNullOrEmpty( String value, String defaultValue )
    {
        return StringUtils.isBlank( value ) ? defaultValue : value;
    }

    private void applyChange( MavenProject project, SortedMap<String, Model> reactor, Set<File> files, String groupId,
                              String artifactId, String oldVersion )
    {

        getLog().debug( "Applying change " + groupId + ":" + artifactId + ":" + oldVersion + " -> " + newVersion );
        // this is a triggering change
        addChange( groupId, artifactId, oldVersion, newVersion );
        // now fake out the triggering change

        final Map.Entry<String, Model> current = PomHelper.getModelEntry( reactor, groupId, artifactId );
        current.getValue().setVersion( newVersion );

        addFile( files, getProject(), current.getKey() );

        for ( Map.Entry<String, Model> sourceEntry : reactor.entrySet() )
        {
            final String sourcePath = sourceEntry.getKey();
            final Model sourceModel = sourceEntry.getValue();

            getLog().debug( sourcePath.length() == 0 ? "Processing root module as parent"
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

            addFile( files, project, sourcePath );

            getLog().debug( "Looking for modules which use "
                + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + " as their parent" );

            for ( Map.Entry<String, Model> stringModelEntry : PomHelper.getChildModels( reactor, sourceGroupId,
                                                                                        sourceArtifactId ).entrySet() )
            {
                final Map.Entry target = (Map.Entry) stringModelEntry;
                final String targetPath = (String) target.getKey();
                final Model targetModel = (Model) target.getValue();
                final Parent parent = targetModel.getParent();
                getLog().debug( "Module: " + targetPath );
                if ( sourceVersion.equals( parent.getVersion() ) )
                {
                    getLog().debug( "    parent already is "
                        + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":" + sourceVersion );
                }
                else
                {
                    getLog().debug( "    parent is " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId )
                        + ":" + parent.getVersion() );
                    getLog().debug( "    will become " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId )
                        + ":" + sourceVersion );
                }
                final boolean targetExplicit = PomHelper.isExplicitVersion( targetModel );
                if ( ( updateMatchingVersions || !targetExplicit )
                    && StringUtils.equals( parent.getVersion(), PomHelper.getVersion( targetModel ) ) )
                {
                    getLog().debug( "    module is "
                        + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                        PomHelper.getArtifactId( targetModel ) )
                        + ":" + PomHelper.getVersion( targetModel ) );
                    getLog().debug( "    will become "
                        + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                        PomHelper.getArtifactId( targetModel ) )
                        + ":" + sourceVersion );
                    addChange( PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ),
                               PomHelper.getVersion( targetModel ), sourceVersion );
                    targetModel.setVersion( sourceVersion );
                }
                else
                {
                    getLog().debug( "    module is "
                        + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                        PomHelper.getArtifactId( targetModel ) )
                        + ":" + PomHelper.getVersion( targetModel ) );
                }
            }
        }
    }

    private void addFile( Set<File> files, MavenProject project, String relativePath )
    {
        final File moduleDir = new File( project.getBasedir(), relativePath );
        final File projectBaseDir = project.getBasedir();

        final File moduleProjectFile;

        if ( projectBaseDir.equals(moduleDir) )
        {
            moduleProjectFile = project.getFile();
        }
        else if ( moduleDir.isDirectory() )
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
    }

    /**
     * Updates the pom file.
     *
     * @param pom The pom file to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong.
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong.
     * @throws javax.xml.stream.XMLStreamException when things go wrong.
     */
    protected synchronized void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        ContextualLog log = new DelegatingContextualLog( getLog() );
        try
        {
            Model model = PomHelper.getRawModel( pom );
            log.setContext( "Processing " + PomHelper.getGroupId( model ) + ":" + PomHelper.getArtifactId( model ) );

            VersionChangerFactory versionChangerFactory = new VersionChangerFactory();
            versionChangerFactory.setPom( pom );
            versionChangerFactory.setLog( log );
            versionChangerFactory.setModel( model );

            VersionChanger changer = versionChangerFactory.newVersionChanger( processParent, processProject,
                                                                              processDependencies, processPlugins );

            for ( VersionChange versionChange : sourceChanges )
            {
                changer.apply( versionChange );
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        log.clearContext();
    }

}
