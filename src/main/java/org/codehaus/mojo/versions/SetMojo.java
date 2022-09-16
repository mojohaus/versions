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

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.regex.Pattern;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
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

/**
 * Sets the current project's version and based on that change propagates that change onto any child modules as
 * necessary.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "set", requiresProject = true, requiresDirectInvocation = true, aggregator = true, threadSafe = true )
public class SetMojo extends AbstractVersionsUpdaterMojo
{

    private static final String SNAPSHOT = "-SNAPSHOT";

    /**
     * The new version number to set.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "newVersion" )
    private String newVersion;

    /**
     * The groupId of the dependency/module to update.
     * If you like to update modules of a aggregator you
     * should set <code>-DgroupId='*'</code> to ignore the
     * group of the current project. On Windows you can omit
     * the single quotes on Linux they are necessary to prevent
     * expansion through the shell.
     *
     * @since 1.2
     */
    @Parameter( property = "groupId", defaultValue = "${project.groupId}" )
    private String groupId;

    /**
     * The artifactId of the dependency/module to update.
     * If you like to update modules of a aggregator you
     * should set <code>-DartifactId='*'</code> to ignore the
     * artifactId of the current project. On Windows you can omit
     * the single quotes on Linux they are necessary to prevent
     * expansion through the shell.
     *
     * @since 1.2
     */
    @Parameter( property = "artifactId", defaultValue = "${project.artifactId}" )
    private String artifactId;

    /**
     * The version of the dependency/module to update.
     * If you are changing an aggregator you should give
     * <code>-DoldVersion='*'</code> to suppress the check against the
     * version of the current project. On Windows you can omit
     * the single quotes on Linux they are necessary to prevent
     * expansion through the shell.
     *
     * @since 1.2
     */
    @Parameter( property = "oldVersion", defaultValue = "${project.version}" )
    private String oldVersion;

    /**
     * Whether matching versions explicitly specified (as /project/version) in child modules should be updated.
     *
     * @since 1.3
     */
    @Parameter( property = "updateMatchingVersions", defaultValue = "true" )
    private boolean updateMatchingVersions;

    /**
     * Whether to process the parent of the project.
     *
     * @since 1.3
     */
    @Parameter( property = "processParent", defaultValue = "true" )
    private boolean processParent;

    /**
     * Whether to process the project version.
     *
     * @since 1.3
     */
    @Parameter( property = "processProject", defaultValue = "true" )
    private boolean processProject;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.3
     */
    @Parameter( property = "processDependencies", defaultValue = "true" )
    private boolean processDependencies;

    /**
     * Whether to process the plugins section of the project.
     *
     * @since 1.3
     */
    @Parameter( property = "processPlugins", defaultValue = "true" )
    private boolean processPlugins;

    /**
     * Component used to prompt for input
     */
    private Prompter prompter;

    /**
     * Whether to remove <code>-SNAPSHOT</code> from the existing version.
     *
     * @since 2.10
     */
    @Parameter( property = "removeSnapshot", defaultValue = "false" )
    private boolean removeSnapshot;

    /**
     * Whether to add next version number and <code>-SNAPSHOT</code> to the existing version.
     * Unless specified by <code>nextSnapshotIndexToIncrement</code>, will increment
     * the last minor index of the snapshot version, e.g. the <code>z</code> in <code>x.y.z-SNAPSHOT</code>
     *
     * @since 2.10
     */
    @Parameter( property = "nextSnapshot", defaultValue = "false" )
    protected boolean nextSnapshot;

    /**
     * <p>Specifies the version index to increment when using <code>nextSnapshot</code>.
     * Will increment the (1-based, counting from the left, or the most major component) index
     * of the snapshot version, e.g. for <code>-DnextSnapshotIndexToIncrement=1</code>
     * and the version being <code>1.2.3-SNAPSHOT</code>, the new version will become <code>2.2.3-SNAPSHOT.</code></p>
     * <p>Only valid with <code>nextSnapshot</code>.</p>
     *
     * @since 2.12
     */
    @Parameter( property = "nextSnapshotIndexToIncrement" )
    protected Integer nextSnapshotIndexToIncrement;

    /**
     * Whether to process all modules whereas they have parent/child or not.
     *
     * @since 2.5
     */
    @Parameter( property = "processAllModules", defaultValue = "false" )
    private boolean processAllModules;

    /**
     * Whether to start processing at the local aggregation root (which might be a parent module
     * of that module where Maven is executed in, and the version change may affect parent and sibling modules).
     * Setting to false makes sure only the module (and it's submodules) where Maven is executed for is affected.
     *
     * @since 2.9
     */
    @Parameter( property = "processFromLocalAggregationRoot", defaultValue = "true" )
    private boolean processFromLocalAggregationRoot;

    /**
     * Whether to update the <code>project.build.outputTimestamp<code> property in the POM when setting version.
     *
     * @since 2.10
     * @deprecated please use {@link #updateBuildOutputTimestampPolicy} instead
     */
    @Parameter( property = "updateBuildOutputTimestamp", defaultValue = "true" )
    private boolean updateBuildOutputTimestamp;

    /**
     * Whether to update the <code>project.build.outputTimestamp<code> property in the POM when setting version.
     * Valid values are: <code>onchange</code>, which will only change <code>outputTimestamp</code> for changed POMs,
     * <code>always</code>, <code>never</code>.
     *
     * @since 2.12
     */
    @Parameter( property = "updateBuildOutputTimestampPolicy", defaultValue = "onchange" )
    private String updateBuildOutputTimestampPolicy;

    /**
     * The changes to module coordinates. Guarded by this.
     */
    private final transient List<VersionChange> sourceChanges = new ArrayList<>();

    @Inject
    public SetMojo( RepositorySystem repositorySystem,
                       MavenProjectBuilder projectBuilder,
                       ArtifactMetadataSource artifactMetadataSource,
                       WagonManager wagonManager,
                       ArtifactResolver artifactResolver,
                       Prompter prompter )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
        this.prompter = prompter;
    }

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
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     */
    public void execute() throws MojoExecutionException, MojoFailureException
    {
        if ( getProject().getOriginalModel().getVersion() == null )
        {
            throw new MojoExecutionException( "Project version is inherited from parent." );
        }

        if ( removeSnapshot && !nextSnapshot )
        {
            String version = getVersion();
            if ( version.endsWith( SNAPSHOT ) )
            {
                String release = version.substring( 0, version.indexOf( SNAPSHOT ) );
                newVersion = release;
                getLog().info( "SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion );
            }
        }

        if ( !nextSnapshot && nextSnapshotIndexToIncrement != null )
        {
            throw new MojoExecutionException( "nextSnapshotIndexToIncrement is not valid when nextSnapshot is false" );
        }

        if ( !removeSnapshot && nextSnapshot )
        {
            String version = getVersion();
            newVersion = getIncrementedVersion( version, nextSnapshotIndexToIncrement );
            getLog().info( "SNAPSHOT found.  BEFORE " + version + "  --> AFTER: " + newVersion );
        }

        if ( StringUtils.isEmpty( newVersion ) )
        {
            if ( settings.isInteractiveMode() )
            {
                try
                {
                    newVersion = prompter.prompt( "Enter the new version to set",
                                                  getProject().getOriginalModel().getVersion() );
                }
                catch ( PrompterException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
            else
            {
                throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                                                      + "property (that is -DnewVersion=... on the command line) "
                                                      + "or run in interactive mode" );
            }
        }
        if ( StringUtils.isEmpty( newVersion ) )
        {
            throw new MojoExecutionException( "You must specify the new version, either by using the newVersion "
                                                  + "property (that is -DnewVersion=... on the command line) "
                                                  + "or run in interactive mode" );
        }

        if ( !"onchange".equals( updateBuildOutputTimestampPolicy ) && !"always".equals(
            updateBuildOutputTimestampPolicy ) && !"never".equals( updateBuildOutputTimestampPolicy ) )
        {
            throw new MojoExecutionException(
                "updateBuildOutputTimestampPolicy should be one of: " + "\"onchange\", \"always\", \"never\"." );
        }

        try
        {
            final MavenProject project;
            if ( processFromLocalAggregationRoot )
            {
                project = PomHelper.getLocalRoot( projectBuilder, getProject(), localRepository, null, getLog() );
            }
            else
            {
                project = getProject();
            }

            getLog().info( "Local aggregation root: " + project.getBasedir() );
            Map<String, Model> reactorModels = PomHelper.getReactorModels( project, getLog() );
            final SortedMap<String, Model> reactor =
                new TreeMap<String, Model>( new ReactorDepthComparator( reactorModels ) );
            reactor.putAll( reactorModels );

            // set of files to update
            final Set<File> files = new LinkedHashSet<File>();

            getLog().info(
                "Processing change of " + groupId + ":" + artifactId + ":" + oldVersion + " -> " + newVersion );
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
                if ( ( processAllModules
                        || groupIdRegex.matcher( mGroupId ).matches()
                        && artifactIdRegex.matcher( mArtifactId ).matches() )
                        && oldVersionIdRegex.matcher( mVersion ).matches()
                        && !newVersion.equals( mVersion ) )
                {
                    found = true;
                    // if the change is not one we have swept up already
                    applyChange( project, reactor, files, mGroupId, m.getArtifactId(),
                            StringUtils.isBlank( oldVersion ) || "*".equals( oldVersion ) ? "" : mVersion );
                }
            }
            if ( !found && RegexUtils.getWildcardScore( groupId ) == 0 && RegexUtils.getWildcardScore( artifactId ) == 0
                && RegexUtils.getWildcardScore( oldVersion ) == 0 )
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

    /**
     * Returns the incremented version, with the nextSnapshotIndexToIncrement indicating the 1-based index,
     * conunting from the left, or the most major version component, of the version string.
     *
     * @param version input version
     * @return version with the incremented index specified by nextSnapshotIndexToIncrement or last index
     * @throws MojoExecutionException thrown if the input parameters are invalid
     */
    protected String getIncrementedVersion( String version, Integer nextSnapshotIndexToIncrement )
        throws MojoExecutionException
    {
        String versionWithoutSnapshot =
            version.endsWith( SNAPSHOT ) ? version.substring( 0, version.indexOf( SNAPSHOT ) ) : version;
        List<String> numbers = new LinkedList<>( Arrays.asList( versionWithoutSnapshot.split( "\\." ) ) );

        if ( nextSnapshotIndexToIncrement == null )
        {
            nextSnapshotIndexToIncrement = numbers.size();
        }
        else if ( nextSnapshotIndexToIncrement < 1 )
        {
            throw new MojoExecutionException( "nextSnapshotIndexToIncrement cannot be less than 1" );
        }
        else if ( nextSnapshotIndexToIncrement > numbers.size() )
        {
            throw new MojoExecutionException(
                "nextSnapshotIndexToIncrement cannot be greater than the last version index" );
        }
        int snapshotVersionToIncrement = Integer.parseInt( numbers.remove( nextSnapshotIndexToIncrement - 1 ) );
        numbers.add( nextSnapshotIndexToIncrement - 1, String.valueOf( snapshotVersionToIncrement + 1 ) );

        return StringUtils.join( numbers.toArray( new String[0] ), "." ) + "-SNAPSHOT";
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

        Map.Entry<String, Model> current = PomHelper.getModelEntry( reactor, groupId, artifactId );
        if ( current != null )
        {
            current.getValue().setVersion( newVersion );
            addFile( files, project, current.getKey() );
        }

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

            getLog().debug(
                "Looking for modules which use " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId )
                    + " as their parent" );

            for ( Map.Entry<String, Model> stringModelEntry : processAllModules ? reactor.entrySet()
                : PomHelper.getChildModels( reactor, sourceGroupId, sourceArtifactId ).entrySet() )
            {
                final Model targetModel = stringModelEntry.getValue();
                final Parent parent = targetModel.getParent();
                getLog().debug( "Module: " + stringModelEntry.getKey() );
                if ( parent != null && sourceVersion.equals( parent.getVersion() ) )
                {
                    getLog().debug(
                        "    parent already is " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId )
                            + ":" + sourceVersion );
                }
                else
                {
                    getLog().debug(
                        "    parent is " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":" + (
                            parent == null ? "" : parent.getVersion() ) );
                    getLog().debug(
                        "    will become " + ArtifactUtils.versionlessKey( sourceGroupId, sourceArtifactId ) + ":"
                            + sourceVersion );
                }
                final boolean targetExplicit = PomHelper.isExplicitVersion( targetModel );
                if ( ( updateMatchingVersions || !targetExplicit ) //
                    && ( parent != null && StringUtils.equals( parent.getVersion(),
                                                               PomHelper.getVersion( targetModel ) ) ) )
                {
                    getLog().debug(
                        "    module is " + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                                         PomHelper.getArtifactId( targetModel ) ) + ":"
                            + PomHelper.getVersion(
                            targetModel ) );
                    getLog().debug(
                        "    will become " + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                                           PomHelper.getArtifactId( targetModel ) )
                            + ":" + sourceVersion );
                    addChange( PomHelper.getGroupId( targetModel ), PomHelper.getArtifactId( targetModel ),
                               PomHelper.getVersion( targetModel ), sourceVersion );
                    targetModel.setVersion( sourceVersion );
                }
                else
                {
                    getLog().debug(
                        "    module is " + ArtifactUtils.versionlessKey( PomHelper.getGroupId( targetModel ),
                                                                         PomHelper.getArtifactId( targetModel ) ) + ":"
                            + PomHelper.getVersion(
                            targetModel ) );
                }
            }
        }
    }

    private void addFile( Set<File> files, MavenProject project, String relativePath )
    {
        final File moduleDir = new File( project.getBasedir(), relativePath );
        final File projectBaseDir = project.getBasedir();

        final File moduleProjectFile;

        if ( projectBaseDir.equals( moduleDir ) )
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
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong.
     * @throws javax.xml.stream.XMLStreamException            when things go wrong.
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

            VersionChanger changer =
                versionChangerFactory.newVersionChanger( processParent, processProject, processDependencies,
                                                         processPlugins );

            for ( VersionChange versionChange : sourceChanges )
            {
                changer.apply( versionChange );
            }

            if ( updateBuildOutputTimestamp && !"never".equals( updateBuildOutputTimestampPolicy ) )
            {
                if ( "always".equals( updateBuildOutputTimestampPolicy ) || !sourceChanges.isEmpty() )
                {
                    // also update project.build.outputTimestamp
                    updateBuildOutputTimestamp( pom, model );
                }
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        log.clearContext();
    }

    private void updateBuildOutputTimestamp( ModifiedPomXMLEventReader pom, Model model ) throws XMLStreamException
    {
        String buildOutputTimestamp = model.getProperties().getProperty( "project.build.outputTimestamp" );

        if ( buildOutputTimestamp == null || StringUtils.isEmpty( buildOutputTimestamp ) )
        {
            // no Reproducible Builds output timestamp defined
            return;
        }

        if ( StringUtils.isNumeric( buildOutputTimestamp ) )
        {
            // int representing seconds since the epoch, like SOURCE_DATE_EPOCH
            buildOutputTimestamp = String.valueOf( System.currentTimeMillis() / 1000 );
        }
        else if ( buildOutputTimestamp.length() <= 1 )
        {
            // value length == 1 means disable Reproducible Builds
            return;
        }
        else
        {
            // ISO-8601
            DateFormat df = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss'Z'" );
            df.setTimeZone( TimeZone.getTimeZone( "UTC" ) );
            buildOutputTimestamp = df.format( new Date() );
        }

        PomHelper.setPropertyVersion( pom, null, "project.build.outputTimestamp", buildOutputTimestamp );
    }

}
