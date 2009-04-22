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
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.stax2.XMLInputFactory2;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Comparator;
import java.util.List;

/**
 * Abstract base class for Versions Mojos.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
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
     * @component
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
     * The versioning rule to use when comparing versions. Valid values are <code>maven</code>,
     * <code>numeric</code> which will handle long version numbers provided all components are numeric, or
     * <code>mercury</code> which will use the mercury version number comparison rules.
     *
     * @parameter expression="${comparisonMethod}"
     * @since 1.0-alpha-1
     */
    protected String comparisonMethod;

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
     * The Maven Project.
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
                                                serverId, rulesUri, comparisonMethod, getLog() );
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
     * @throws MojoExecutionException If the artifact metadata could not be found.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories )
        throws MojoExecutionException
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
        return artifactVersions.getLatestVersion( versionRange, includeSnapshots );
    }

    /**
     * Returns the version comparator to use.
     *
     * @return the version comparator to use.
     * @since 1.0-alpha-1
     */
    protected Comparator getVersionComparator()
    {
        return VersionComparators.getVersionComparator( comparisonMethod );
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
            StringBuffer input = readFile( outFile );
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
        OutputStream out = new BufferedOutputStream( new FileOutputStream( outFile ) );
        out.write( input.toString().getBytes( PomHelper.POM_ENCODING ) );
        out.close();
    }

    /**
     * Reads a file into a StringBuffer.
     *
     * @param outFile The file to read.
     * @return StringBuffer The contents of the file.
     * @throws IOException when things go wrong.
     */
    protected final StringBuffer readFile( File outFile )
        throws IOException
    {
        StringBuffer input;
        BufferedInputStream reader;
        reader = new BufferedInputStream( new FileInputStream( outFile ) );

        byte[] content = new byte[(int) outFile.length()];
        input = new StringBuffer( content.length );
        try
        {
            int length = reader.read( content, 0, content.length );
            input.append( new String( content, 0, length, PomHelper.POM_ENCODING ) );
            return input;
        }
        finally
        {
            reader.close();
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
        throws MojoExecutionException, MojoFailureException, XMLStreamException;

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

    protected ExpressionEvaluator getExpressionEvaluator()
    {
        return new VersionsExpressionEvaluator( session, pathTranslator, project );
    }
}
