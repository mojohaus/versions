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
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.WriterFactory;
import org.codehaus.stax2.XMLInputFactory2;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.List;

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
     * @parameter expression="${project}"
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    private MavenProject project;

    /**
     * @component
     * @since 1.0-alpha-1
     */
    protected org.apache.maven.artifact.factory.ArtifactFactory artifactFactory;

    /**
     * @component
     * @since 1.0-alpha-1
     */
    protected org.apache.maven.artifact.resolver.ArtifactResolver resolver;

    /**
     * @component allo
     * @since 1.0-alpha-1
     */
    protected MavenProjectBuilder projectBuilder;

    /**
     * @parameter expression="${reactorProjects}"
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    protected List reactorProjects;

    /**
     * The artifact metadata source to use.
     *
     * @component
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    protected ArtifactMetadataSource artifactMetadataSource;

    /**
     * @parameter expression="${project.remoteArtifactRepositories}"
     * @readonly
     * @since 1.0-alpha-3
     */
    protected List remoteArtifactRepositories;

    /**
     * @parameter expression="${project.pluginArtifactRepositories}"
     * @readonly
     * @since 1.0-alpha-3
     */
    protected List remotePluginRepositories;

    /**
     * @parameter expression="${localRepository}"
     * @readonly
     * @since 1.0-alpha-1
     */
    protected ArtifactRepository localRepository;

    /**
     * @component
     * @since 1.0-alpha-3
     */
    private WagonManager wagonManager;

    /**
     * @parameter expression="${settings}"
     * @readonly
     * @since 1.0-alpha-3
     */
    private Settings settings;

    /**
     * settings.xml's server id for the URL.
     * This is used when wagon needs extra authentication information.
     *
     * @parameter expression="${maven.version.rules.serverId}" default-value="serverId";
     * @since 1.0-alpha-3
     */
    private String serverId;

    /**
     * The Wagon URI of a ruleSet file containing the rules that control how to compare version numbers.
     *
     * @parameter expression="${maven.version.rules}"
     * @since 1.0-alpha-3
     */
    private String rulesUri;

    /**
     * Controls whether a backup pom should be created (default is true).
     *
     * @parameter expression="${generateBackupPoms}"
     * @since 1.0-alpha-3
     */
    private Boolean generateBackupPoms;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @parameter expression="${allowSnapshots}" default-value="false"
     * @since 1.0-alpha-1
     */
    protected Boolean allowSnapshots;

    /**
     * Our versions helper.
     */
    private VersionsHelper helper;

    /**
     * The Maven Sessopm.
     *
     * @parameter expression="${session}"
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    protected MavenSession session;

    /**
     * @component
     */
    protected PathTranslator pathTranslator;

    // --------------------- GETTER / SETTER METHODS ---------------------

    public VersionsHelper getHelper()
        throws MojoExecutionException
    {
        if ( helper == null )
        {
            helper = new DefaultVersionsHelper( artifactFactory, artifactMetadataSource, remoteArtifactRepositories,
                                                remotePluginRepositories, localRepository, wagonManager, settings,
                                                serverId, rulesUri, getLog(), session,
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
     * @param usePluginRepositories
     * @return The latest version of the specified artifact that matches the specified version range or
     *         <code>null</code> if no matching version could be found.
     * @throws ArtifactMetadataRetrievalException
     *          If the artifact metadata could not be found.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories )
        throws ArtifactMetadataRetrievalException, MojoExecutionException
    {
        boolean includeSnapshots = Boolean.TRUE.equals( this.allowSnapshots );
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
     * Gets the property value that is defined in the pom. This is an extension point to allow updating a file
     * external to the reactor.
     *
     * @param pom      The pom.
     * @param property The property.
     * @return The value as defined in the pom or <code>null</code> if not defined.
     * @since 1.0-alpha-1
     */
    protected String getPropertyValue( StringBuffer pom, String property )
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
            StringBuffer input = PomHelper.readXmlFile( outFile );
            ModifiedPomXMLEventReader newPom = newModifiedPomXER( input );

            update( newPom );

            if ( newPom.isModified() )
            {
                if ( Boolean.FALSE.equals( generateBackupPoms ) )
                {
                    getLog().debug( "Skipping generation of backup file" );
                }
                else
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
                writeFile( outFile, input );
            }
        }
        catch ( IOException e )
        {
            getLog().error( e );
        }
        catch ( XMLStreamException e )
        {
            getLog().error( e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

    }

    /**
     * Creates a {@link org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader} from a StringBuffer.
     *
     * @param input The XML to read and modify.
     * @return The {@link org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader}.
     */
    protected final ModifiedPomXMLEventReader newModifiedPomXER( StringBuffer input )
    {
        ModifiedPomXMLEventReader newPom = null;
        try
        {
            XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
            inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
            newPom = new ModifiedPomXMLEventReader( input, inputFactory );
        }
        catch ( XMLStreamException e )
        {
            getLog().error( e );
        }
        return newPom;
    }

    /**
     * Writes a StringBuffer into a file.
     *
     * @param outFile The file to read.
     * @param input   The contents of the file.
     * @throws IOException when things go wrong.
     */
    protected final void writeFile( File outFile, StringBuffer input )
        throws IOException
    {
        Writer writer = WriterFactory.newXmlWriter( outFile );
        try
        {
            IOUtil.copy( input.toString(), writer );
        }
        finally
        {
            IOUtil.close( writer );
        }
    }

    /**
     * Updates the pom.
     *
     * @param pom The pom to update.
     * @throws MojoExecutionException If things go wrong.
     * @throws MojoFailureException   If things go wrong.
     * @throws javax.xml.stream.XMLStreamException
     *                                If things go wrong.
     * @since 1.0-alpha-1
     */
    protected abstract void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException;

    /**
     * Returns <code>true</code> if the update should be applied.
     *
     * @param artifact       The artifact.
     * @param currentVersion The current version of the artifact.
     * @param updateVersion  The proposed new version of the artifact.
     * @return <code>true</code> if the update should be applied.
     * @since 1.0-alpha-1
     */
    protected boolean shouldApplyUpdate( Artifact artifact, String currentVersion, ArtifactVersion updateVersion )
    {
        getLog().debug( "Proposal is to update from " + currentVersion + " to " + updateVersion );

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
            getLog().warn( "Not updating version: could not resolve " + artifact.toString(), e );
            return false;
        }
        catch ( ArtifactNotFoundException e )
        {
            getLog().warn( "Not updating version: could not find " + artifact.toString(), e );
            return false;
        }

        if ( currentVersion.equals( updateVersion.toString() ) )
        {
            getLog().info( "Current version of " + artifact.toString() + " is the latest." );
            return false;
        }
        return true;
    }

    /**
     * Based on the passed flags, determines which segment is unchangable. This
     * can be used when determining an upper bound for the "latest" version.
     * @param allowMajorUpdates
     * @param allowMinorUpdates
     * @param allowIncrementalUpdates
     * @return Returns the segment that is unchangable. If any segment
     * can change, returns -1.
     */
    protected int determineUnchangedSegment(Boolean allowMajorUpdates, Boolean allowMinorUpdates,
            Boolean allowIncrementalUpdates)
    {
        int segment;
        if ( Boolean.TRUE.equals( allowMajorUpdates ) )
        {
            segment = -1;
            getLog().info( "Major version changes allowed" );
        }
        else if ( Boolean.TRUE.equals( allowMinorUpdates ) )
        {
            segment = 0;
            getLog().info( "Minor version changes allowed" );
        }
        else if ( Boolean.TRUE.equals( allowIncrementalUpdates ) )
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
}
