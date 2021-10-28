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
import java.io.InputStream;
import java.io.Writer;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;

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
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.JGitHelper;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.WriterFactory;
import org.codehaus.stax2.XMLInputFactory2;
import org.eclipse.jgit.api.Git;

/**
 * Abstract base class for Versions Mojos.
 *
 * @author Stephen Connolly
 */
public abstract class AbstractVersionsUpdaterMojo
    extends AbstractMojo
{

    // ------------------------------ FIELDS ------------------------------

    private final Lock lock = new ReentrantLock();
    /**
     * The Maven Project.
     *
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${project}", required = true, readonly = true )
    protected MavenProject project;

    /**
     * @since 1.0-alpha-1
     */
    @Component
    protected org.apache.maven.artifact.factory.ArtifactFactory artifactFactory;

    /**
     * @since 1.0-alpha-1
     */
    @Component
    protected org.apache.maven.artifact.resolver.ArtifactResolver resolver;

    /**
     * @since 1.0-alpha-1
     */
    @Component
    protected MavenProjectBuilder projectBuilder;

    /**
     * @since 1.0-alpha-1
     */
    @Parameter( defaultValue = "${reactorProjects}", required = true, readonly = true )
    protected List reactorProjects;

    /**
     * The artifact metadata source to use.
     *
     * @since 1.0-alpha-1
     */
    @Component
    protected ArtifactMetadataSource artifactMetadataSource;

    /**
     * @since 1.0-alpha-3
     */
    @Parameter( defaultValue = "${project.remoteArtifactRepositories}", readonly = true )
    protected List<ArtifactRepository> remoteArtifactRepositories;

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
     * @component
     * @since 1.0-alpha-3
     */
    @Component
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

    @Component
    protected PathTranslator pathTranslator;

    @Component
    protected ArtifactResolver artifactResolver;

    // --------------------- GETTER / SETTER METHODS ---------------------

    public VersionsHelper getHelper()
        throws MojoExecutionException
    {
        if ( helper == null )
        {
            helper = new DefaultVersionsHelper( artifactFactory, artifactResolver, artifactMetadataSource,
                                                remoteArtifactRepositories, remotePluginRepositories, localRepository,
                                                wagonManager, settings, serverId, rulesUri, getLog(), session,
                                                pathTranslator );
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
     * @param artifact The artifact.
     * @param versionRange The version range.
     * @param allowingSnapshots <code>null</code> for no override, otherwise the local override to apply.
     * @param usePluginRepositories Use plugin repositories
     * @return The latest version of the specified artifact that matches the specified version range or
     *         <code>null</code> if no matching version could be found.
     * @throws ArtifactMetadataRetrievalException If the artifact metadata could not be found.
     * @throws MojoExecutionException if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException, MojoExecutionException
    {
        boolean includeSnapshots = this.allowSnapshots;
        if ( Boolean.TRUE.equals( allowingSnapshots ) )
        {
            includeSnapshots = true;
        }
        if ( Boolean.FALSE.equals( allowingSnapshots ) )
        {
            includeSnapshots = false;
        }
        final ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifact, usePluginRepositories );
        return artifactVersions.getNewestVersion( versionRange, includeSnapshots );
    }

    /**
     * Gets the property value that is defined in the pom. This is an extension point to allow updating a file external
     * to the reactor.
     *
     * @param pom The pom.
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
     * @throws MojoFailureException If things go wrong.
     * @since 1.0-alpha-1
     */
    protected void process( File outFile )
        throws MojoExecutionException, MojoFailureException
    {
        try
        {
            StringBuilder input = PomHelper.readXmlFile( outFile );
            ModifiedPomXMLEventReader newPom = newModifiedPomXER( input, outFile.getAbsolutePath() );

            update(newPom, outFile, input);

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
            }
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
     * @param path Path pointing to the source of the XML
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
     * @param input The contents of the file.
     * @throws IOException when things go wrong.
     */
    protected final void writePomFile(File outFile, StringBuilder input )
        throws IOException
    {

        getLog().info(" >>> Writing pom file : " + outFile.getPath());
        try (Writer writer = WriterFactory.newXmlWriter( outFile ) )
        {
            IOUtil.copy( input.toString(), writer );
        }
    }

    /**
     * Updates the pom.
     *
     * @param pom The pom to update.
     * @param outFile The POM file to write
     * @param input The modifications as a {@link StringBuilder}
     * @throws MojoExecutionException If things go wrong.
     * @throws MojoFailureException If things go wrong.
     * @throws javax.xml.stream.XMLStreamException If things go wrong.
     * @throws ArtifactMetadataRetrievalException if something goes wrong.
     * @since 1.0-alpha-1
     */
    protected abstract void update(ModifiedPomXMLEventReader pom,
                                   final File outFile,
                                   final StringBuilder input)
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException;

    /**
     * @deprecated
     *  This method no longer supported.
     *  use shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion, Boolean forceUpdate )
     *
     * Returns <code>true</code> if the update should be applied.
     *
     * @param artifact The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied.
     * @since 1.0-alpha-1
     */
    @Deprecated
    protected boolean shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion )
    {
        return shouldApplyUpdate(artifact,currentVersion,updateVersion,false);
    }

    /**
     * Returns <code>true</code> if the update should be applied.
     *
     * @param artifact The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied to the pom.
     * @since 2.9
     */
    protected boolean shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion, boolean forceUpdate )
    {
        getLog().debug( "Proposal is to update from " + currentVersion + " to " + updateVersion );

        if ( forceUpdate )
        {
            getLog().info( "Force update enabled. LATEST or RELEASE versions will be overwritten with real version" );
            return true;
        }

        if ( updateVersion == null )
        {
            getLog().warn( "Not updating version: could not resolve any versions" );
            return false;
        }

        artifact.setVersion( updateVersion.toString() );
        try
        {
            resolver.resolveAlways( artifact, remoteArtifactRepositories, localRepository );
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
     * Based on the passed flags, determines which segment is unchangable. This can be used when determining an upper
     * bound for the "latest" version.
     *
     * @param allowMajorUpdates Allow major updates
     * @param allowMinorUpdates Allow minor updates
     * @param allowIncrementalUpdates Allow incremental updates
     * @return Returns the segment that is unchangable. If any segment can change, returns -1.
     */
    protected int determineUnchangedSegment( boolean allowMajorUpdates, boolean allowMinorUpdates,
                                             boolean allowIncrementalUpdates )
    {
        int segment;
        if ( allowMajorUpdates )
        {
            segment = -1;
            getLog().info( "Major version changes allowed" );
        }
        else if ( allowMinorUpdates )
        {
            segment = 0;
            getLog().info( "Minor version changes allowed" );
        }
        else if ( allowIncrementalUpdates )
        {
            segment = 1;
            getLog().info( "Incremental version changes allowed" );
        }
        else
        {
            segment = 2;
            getLog().info( "Subincremental version changes allowed" );
        }

        return segment;
    }

    protected void updatePropertyToNewestVersion( ModifiedPomXMLEventReader pom,
                                                  Property property,
                                                  PropertyVersions version,
                                                  String currentVersion,
                                                  File outFile,
                                                  StringBuilder input )
        throws MojoExecutionException, XMLStreamException
    {
        updatePropertyToNewestVersion( pom, property, version, currentVersion, false, -1, outFile, input );
    }

    protected void updatePropertyToNewestVersion( ModifiedPomXMLEventReader pom,
                                                  Property property,
                                                  PropertyVersions version,
                                                  String currentVersion,
                                                  boolean allowDowngrade,
                                                  int segment,
                                                  File outFile,
                                                  StringBuilder input)
        throws MojoExecutionException, XMLStreamException
    {

        ArtifactVersion winner =
            version.getNewestVersion( currentVersion, property, this.allowSnapshots, this.reactorProjects,
                                      this.getHelper(), allowDowngrade, segment );
        if ( winner == null || currentVersion.equals( winner.toString() ) )
        {
            getLog().info( "Property " + property.getName() + ": Leaving unchanged as " + currentVersion );
        }
        else if ( PomHelper.setPropertyVersion( pom, version.getProfileId(), property.getName(), winner.toString() ) ) {
            final String  protpertyName = CaseUtils.toCamelCase(property.getName().replaceAll(".version", "").toLowerCase(), true, null);
            String updateMessage = "Update " + protpertyName + " from " + currentVersion + " to " + winner;

            lock.lock();
            try {
                // To have separated commits we need to write the pom each time a property is updated
                writePomFile(outFile, input);
                getLog().info(updateMessage);

                final Git git = JGitHelper.git();
                git.add().addFilepattern(".").call();

                final StringBuilder commitMessage = new StringBuilder(updateMessage).append("\n\n");
                final String releaseNoteUrl = getUrlFromPropertiesFile(property, "releasenotes.properties");
                if (releaseNoteUrl != null) {
                    commitMessage.append("Release notes: ")
                                 .append(releaseNoteUrl)
                                 .append(winner)
                                 .append("\n")
                                 .toString();
                }
                // Try to find the changelog if any (does not require any version number)
                final String changeLogUrl = getUrlFromPropertiesFile(property, "changelogs.properties");
                if (changeLogUrl != null) {
                    commitMessage.append("Change logs: ")
                                 .append(changeLogUrl)
                                 .append("\n")
                                 .toString();
                }
                git.commit().setMessage(commitMessage.toString()).call();
            } catch (final Exception exception) {
                getLog().error(exception);
            } finally {
                lock.unlock();
            }
        }
    }

    private String getUrlFromPropertiesFile(final Property property,
                                            final String propertyFileName) {
        String url = null;
        final Properties properties = new Properties();
        try (InputStream rn = this.getClass().getClassLoader().getResourceAsStream(propertyFileName)) {
            properties.load(rn);
            url = properties.getProperty(property.getName().replaceAll(".version", ""));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return url;
    }
}
