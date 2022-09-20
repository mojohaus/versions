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
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.text.CaseUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.recording.ChangeRecorder;
import org.codehaus.mojo.versions.recording.ChangeRecorderNull;
import org.codehaus.mojo.versions.recording.ChangeRecorderXML;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.WriterFactory;
import org.codehaus.stax2.XMLInputFactory2;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;

/**
 * Abstract base class for Versions Mojos.
 *
 * @author Stephen Connolly
 */
public abstract class AbstractVersionsUpdaterMojo
    extends AbstractMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * The Maven Project.
     *
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${project}", required = true, readonly = true )
    protected MavenProject project;

    protected RepositorySystem repositorySystem;

    /**
     * @since 1.0-alpha-1
     */
    protected MavenProjectBuilder projectBuilder;

    /**
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${reactorProjects}", required = true, readonly = true )
    protected List<MavenProject> reactorProjects;

    /**
     * The artifact metadata source to use.
     *
     * @since 1.0-alpha-1
     */
    protected ArtifactMetadataSource artifactMetadataSource;

    /**
     * @since 1.0-alpha-3
     */
    @Parameter( defaultValue = "${project.remoteArtifactRepositories}", readonly = true )
    protected List<ArtifactRepository> remoteArtifactRepositories;

    /**
     * @since 1.0-alpha-3
     */
    @Parameter( defaultValue = "${project.distributionManagementArtifactRepository}", readonly = true )
    protected ArtifactRepository snapshotsRepository;


    /**
     * @since 1.0-alpha-3
     */
    @Parameter( defaultValue = "${project.pluginArtifactRepositories}", readonly = true )
    protected List<ArtifactRepository> remotePluginRepositories;

    /**
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${localRepository}", readonly = true )
    protected ArtifactRepository localRepository;

    /**
     * @since 1.0-alpha-3
     */
    private WagonManager wagonManager;

    /**
     * @since 1.0-alpha-3
     */
    @Parameter( defaultValue = "${settings}", readonly = true )
    protected Settings settings;

    /**
     * settings.xml's server id for the URL. This is used when wagon needs extra authentication information.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "maven.version.rules.serverId", defaultValue = "serverId" )
    private String serverId;

    /**
     * URI of a ruleSet file containing the rules that control how to compare
     * version numbers. The URI could be either a Wagon URI or a classpath URI
     * (e.g. <code>classpath:///package/sub/package/rules.xml</code>).
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "maven.version.rules" )
    private String rulesUri;

    /**
     * Controls whether a backup pom should be created.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "generateBackupPoms", defaultValue = "true" )
    private boolean generateBackupPoms;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter( property = "allowSnapshots", defaultValue = "false" )
    protected boolean allowSnapshots;

    /**
     * Our versions helper.
     */
    private VersionsHelper helper;

    /**
     * The Maven Session.
     *
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${session}", required = true, readonly = true )
    protected MavenSession session;

    @Parameter( defaultValue = "${mojoExecution}", required = true, readonly = true )
    private MojoExecution mojoExecution;

    protected ArtifactResolver artifactResolver;
    /**
     * The format used to record changes. If "none" is specified, no changes are recorded.
     *
     * @since 2.11
     */
    @Parameter( property = "changeRecorderFormat",
                defaultValue = "none" )
    private String changeRecorderFormat = "none";
    /**
     * The output file used to record changes.
     *
     * @since 2.11
     */
    @Parameter( property = "changeRecorderOutputFile",
                defaultValue = "${project.build.directory}/versions-changes.xml" )
    private File changeRecorderOutputFile;
    /**
     * The change recorder implementation.
     */
    private ChangeRecorder changeRecorder;

    /**
     * <p>Allows specifying the {@linkplain RuleSet} object describing rules
     * on artifact versions to ignore when considering updates.</p>
     *
     * @see <a href="https://www.mojohaus.org/versions-maven-plugin/version-rules.html#Using_the_ruleSet_element_in_the_POM">
     *     Using the ruleSet element in the POM</a>
     *
     * @since 2.13.0
     */
    @Parameter
    protected RuleSet ruleSet;

    /**
     * <p>Allows specifying ignored versions directly as an alternative
     * to providing the {@linkplain #ruleSet} parameter; mainly created
     * for {@code -D} property usage.</p>
     *
     * <p>
     * Example: {@code "1\.0\.1,.+-M.,.*-SNAPSHOT"}
     * </p>
     *
     * <p><em>Currently, this parameter will override the defined {@link #ruleSet}</em></p>
     * @since 2.13.0
     */
    @Parameter( property = "maven.version.ignore" )
    protected Set<String> ignoredVersions;

    // --------------------- GETTER / SETTER METHODS ---------------------

    @Inject
    protected AbstractVersionsUpdaterMojo( RepositorySystem repositorySystem,
                                          MavenProjectBuilder projectBuilder,
                                          ArtifactMetadataSource artifactMetadataSource,
                                          WagonManager wagonManager,
                                          ArtifactResolver artifactResolver )
    {
        this.repositorySystem = repositorySystem;
        this.projectBuilder = projectBuilder;
        this.artifactMetadataSource = artifactMetadataSource;
        this.wagonManager = wagonManager;
        this.artifactResolver = artifactResolver;
    }

    public VersionsHelper getHelper() throws MojoExecutionException
    {
        if ( helper == null )
        {
            helper = new DefaultVersionsHelper.Builder()
                    .withRepositorySystem( repositorySystem )
                    .withArtifactResolver( artifactResolver )
                    .withArtifactMetadataSource( artifactMetadataSource )
                    .withRemoteArtifactRepositories( remoteArtifactRepositories )
                    .withRemotePluginRepositories( remotePluginRepositories )
                    .withDeploymentArtifactRepository( snapshotsRepository )
                    .withLocalRepository( localRepository )
                    .withWagonManager( wagonManager )
                    .withSettings( settings )
                    .withServerId( serverId )
                    .withRulesUri( rulesUri )
                    .withRuleSet( ruleSet )
                    .withIgnoredVersions( ignoredVersions )
                    .withLog( getLog() )
                    .withMavenSession( session )
                    .withMojoExecution( mojoExecution )
                    .build();
        }
        return helper;
    }

    /**
     * Getter for property 'project'.
     *
     * @return Value for property 'project'.
     * @since 1.0-alpha-1
     */
    public MavenProject getProject()
    {
        return project;
    }

    /**
     * Setter for property 'project'.
     *
     * @param project Value to set for property 'project'.
     * @since 1.0-alpha-1
     */
    public void setProject( MavenProject project )
    {
        this.project = project;
    }

    public String getVersion()
    {
        return getProject() == null ? null : getProject().getVersion();
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * {@inheritDoc}
     *
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        File outFile = project.getFile();
        process( outFile );
    }

    // -------------------------- OTHER METHODS --------------------------

    /**
     * Finds the latest version of the specified artifact that matches the version range.
     *
     * @param artifact              The artifact.
     * @param versionRange          The version range.
     * @param allowingSnapshots     <code>null</code> for no override, otherwise the local override to apply.
     * @param usePluginRepositories Use plugin repositories
     * @return The latest version of the specified artifact that matches the specified version range or
     * <code>null</code> if no matching version could be found.
     * @throws ArtifactMetadataRetrievalException If the artifact metadata could not be found.
     * @throws MojoExecutionException             if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories )
            throws ArtifactMetadataRetrievalException, MojoExecutionException
    {
        return findLatestVersion( artifact, versionRange, allowingSnapshots, usePluginRepositories, false );
    }

    /**
     * Finds the latest version of the specified artifact that matches the version range.
     *
     * @param artifact              The artifact.
     * @param versionRange          The version range.
     * @param allowingSnapshots     <code>null</code> for no override, otherwise the local override to apply.
     * @param usePluginRepositories Use plugin repositories
     * @return The latest version of the specified artifact that matches the specified version range or
     * <code>null</code> if no matching version could be found.
     * @throws ArtifactMetadataRetrievalException If the artifact metadata could not be found.
     * @throws MojoExecutionException             if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories,
                                                 boolean allowDowngrade )
        throws ArtifactMetadataRetrievalException, MojoExecutionException
    {
        boolean includeSnapshots = allowingSnapshots != null ? allowingSnapshots : this.allowSnapshots;
        final ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifact, usePluginRepositories );
        return artifactVersions.getNewestVersion( versionRange, null, includeSnapshots, allowDowngrade );
    }

    /**
     * Gets the property value that is defined in the pom. This is an extension point to allow updating a file external
     * to the reactor.
     *
     * @param pom      The pom.
     * @param property The property.
     * @return The value as defined in the pom or <code>null</code> if not defined.
     * @since 1.0-alpha-1
     */
    protected String getPropertyValue( StringBuilder pom, String property )
    {
        return project.getProperties().getProperty( property );
    }

    /**
     * Processes the specified file. This is an extension point to allow updating a file external to the reactor.
     *
     * @param outFile The file to process.
     * @throws MojoExecutionException If things go wrong.
     * @throws MojoFailureException   If things go wrong.
     * @since 1.0-alpha-1
     */
    protected void process( File outFile )
        throws MojoExecutionException, MojoFailureException
    {
        try
        {
            StringBuilder input = PomHelper.readXmlFile( outFile );
            ModifiedPomXMLEventReader newPom = newModifiedPomXER( input, outFile.getAbsolutePath() );

            update( newPom );

            if ( newPom.isModified() )
            {
                if ( generateBackupPoms )
                {
                    File backupFile = new File( outFile.getParentFile(), outFile.getName() + ".versionsBackup" );
                    if ( !backupFile.exists() )
                    {
                        getLog().debug( "Backing up " + outFile + " to " + backupFile );
                        FileUtils.copyFile( outFile, backupFile );
                    }
                    else
                    {
                        getLog().debug( "Leaving existing backup " + backupFile + " unmodified" );
                    }
                }
                else
                {
                    getLog().debug( "Skipping generation of backup file" );
                }
                writeFile( outFile, input );
            }

            saveChangeRecorderResults();
        }
        catch ( IOException | XMLStreamException e )
        {
            getLog().error( e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

    }

    /**
     * Creates a {@link org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader} from a StringBuilder.
     *
     * @param input The XML to read and modify.
     * @param path  Path pointing to the source of the XML
     * @return The {@link org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader}.
     */
    protected final ModifiedPomXMLEventReader newModifiedPomXER( StringBuilder input, String path )
    {
        ModifiedPomXMLEventReader newPom = null;
        try
        {
            XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
            inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
            newPom = new ModifiedPomXMLEventReader( input, inputFactory, path );
        }
        catch ( XMLStreamException e )
        {
            getLog().error( e );
        }
        return newPom;
    }

    /**
     * Writes a StringBuilder into a file.
     *
     * @param outFile The file to read.
     * @param input   The contents of the file.
     * @throws IOException when things go wrong.
     */
    protected final void writeFile( File outFile, StringBuilder input )
        throws IOException
    {
        try ( Writer writer = WriterFactory.newXmlWriter( outFile ) )
        {
            IOUtil.copy( input.toString(), writer );
        }
    }

    /**
     * Updates the pom.
     *
     * @param pom The pom to update.
     * @throws MojoExecutionException              If things go wrong.
     * @throws MojoFailureException                If things go wrong.
     * @throws javax.xml.stream.XMLStreamException If things go wrong.
     * @throws ArtifactMetadataRetrievalException  if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected abstract void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException;

    /**
     * @param artifact       The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion  The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied.
     * @since 1.0-alpha-1
     * @deprecated This method no longer supported.
     * use shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion, Boolean
     * forceUpdate )
     * <p>
     * Returns <code>true</code> if the update should be applied.
     */
    @Deprecated
    protected boolean shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion )
    {
        return shouldApplyUpdate( artifact, currentVersion, updateVersion, false );
    }

    /**
     * Returns <code>true</code> if the update should be applied.
     *
     * @param artifact       The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion  The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied to the pom.
     * @since 2.9
     */
    protected boolean shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion,
                                         boolean forceUpdate )
    {
        getLog().debug( "Proposal is to update from " + currentVersion + " to " + updateVersion );

        if ( updateVersion == null )
        {
            getLog().warn( "Not updating version: could not resolve any versions" );
            return false;
        }

        if ( forceUpdate )
        {
            getLog().info( "Force update enabled. LATEST or RELEASE versions will be overwritten with real version" );
            return true;
        }

        artifact.setVersion( updateVersion.toString() );
        try
        {
            artifactResolver.resolveAlways( artifact, remoteArtifactRepositories, localRepository );
        }
        catch ( ArtifactResolutionException e )
        {
            getLog().warn( "Not updating version: could not resolve " + artifact, e );
            return false;
        }
        catch ( ArtifactNotFoundException e )
        {
            getLog().warn( "Not updating version: could not find " + artifact, e );
            return false;
        }

        if ( currentVersion.equals( updateVersion.toString() ) )
        {
            getLog().info( "Current version of " + artifact + " is the latest." );
            return false;
        }
        return true;
    }

    /**
     * <p>Based on the passed flags, determines which segment (0-based), which is not to be changed.</p>
     * <p>The method will return, depending on the first parameter on the list to be true:
     * <ul>
     * <li>{@code allowMajorUpdates}: -1</li>
     * <li>{@code allowMinorUpdates}: 0</li>
     * <li>{@code allowIncrementalUpdates}: 1</li>
     * <li>(none): 2</li>
     * </ul>
     *
     * This can be used when determining an upper
     * bound for the "latest" version.
     *
     * @param allowMajorUpdates       Allow major updates
     * @param allowMinorUpdates       Allow minor updates
     * @param allowIncrementalUpdates Allow incremental updates
     * @return Returns the segment (0-based) that is unchangable. If any segment can change, returns -1.
     */
    protected Optional<Segment> determineUnchangedSegment( boolean allowMajorUpdates, boolean allowMinorUpdates,
                                                           boolean allowIncrementalUpdates )
    {
        Optional<Segment> unchangedSegment = allowMajorUpdates ? empty()
                : allowMinorUpdates ? of( MAJOR )
                : allowIncrementalUpdates ? of( MINOR )
                : of( INCREMENTAL );
        if ( getLog().isInfoEnabled() )
        {
            getLog().info(
                    unchangedSegment.map( s ->
                                    CaseUtils.toCamelCase( Segment.of( s.value() + 1 ).toString(), true ) )
                            .orElse( "All" ) + " version changes allowed" );
        }
        return unchangedSegment;
    }

    protected ArtifactVersion updatePropertyToNewestVersion( ModifiedPomXMLEventReader pom, Property property,
                                                             PropertyVersions version, String currentVersion,
                                                             boolean allowDowngrade,
                                                             Optional<Segment> unchangedSegment )
            throws XMLStreamException, InvalidVersionSpecificationException,
            InvalidSegmentException, MojoExecutionException
    {
        ArtifactVersion winner =
            version.getNewestVersion( currentVersion, property, this.allowSnapshots, this.reactorProjects,
                                      this.getHelper(), allowDowngrade, unchangedSegment );

        if ( winner == null || currentVersion.equals( winner.toString() ) )
        {
            getLog().info( "Property ${" + property.getName() + "}: Leaving unchanged as " + currentVersion );
        }
        else if ( PomHelper.setPropertyVersion( pom, version.getProfileId(), property.getName(), winner.toString() ) )
        {
            getLog().info( "Updated ${" + property.getName() + "} from " + currentVersion + " to " + winner );
        }

        return winner;
    }

    /**
     * Configure and return the change recorder.
     *
     * @return The change recorder
     * @throws MojoExecutionException If the provided change recorder format is not valid
     */

    protected ChangeRecorder getChangeRecorder() throws MojoExecutionException
    {
        if ( changeRecorder == null )
        {
            if ( "none".equals( this.changeRecorderFormat ) )
            {
                changeRecorder = ChangeRecorderNull.create();
            }
            else if ( "xml".equals( this.changeRecorderFormat ) )
            {
                changeRecorder = ChangeRecorderXML.create();
            }
            else
            {
                throw new MojoExecutionException( "Only 'xml' or 'none' formats are supported for change recordings" );
            }
        }

        return changeRecorder;
    }

    /**
     * Save all of the changes recorded by the change recorder.
     *
     * @throws IOException On I/O errors
     */

    protected void saveChangeRecorderResults() throws IOException
    {
        /*
         * Nobody did anything that required a change recorder.
         */

        if ( this.changeRecorder == null )
        {
            return;
        }

        if ( "none".equals( this.changeRecorderFormat ) )
        {
            return;
        }

        this.getLog().debug( "writing change record to " + this.changeRecorderOutputFile );

        this.changeRecorderOutputFile.getParentFile().mkdirs();
        try ( FileOutputStream outputStream = new FileOutputStream( this.changeRecorderOutputFile ) )
        {
            this.changeRecorder.serialize( outputStream );
        }
    }
}
