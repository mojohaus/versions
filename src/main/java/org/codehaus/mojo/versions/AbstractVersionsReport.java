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
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.siterenderer.Renderer;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.AbstractMavenReport;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.i18n.I18N;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Base class for all versions reports.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public abstract class AbstractVersionsReport
    extends AbstractMavenReport
{

    /**
     * Doxia Site Renderer component.
     *
     * @component
     * @since 1.0-alpha-3
     */
    private Renderer siteRenderer;

    /**
     * Internationalization component.
     *
     * @component
     * @since 1.0-alpha-3
     */
    private I18N i18n;

    /**
     * The Maven Project.
     *
     * @parameter expression="${project}"
     * @required
     * @readonly
     * @since 1.0-alpha-3
     */
    private MavenProject project;

    /**
     * @component
     * @since 1.0-alpha-3
     */
    protected org.apache.maven.artifact.factory.ArtifactFactory artifactFactory;

    /**
     * @component
     * @since 1.0-alpha-3
     */
    private org.apache.maven.artifact.resolver.ArtifactResolver resolver;


    /**
     * The output directory for the report. Note that this parameter is only evaluated if the goal is run directly from
     * the command line. If the goal is run indirectly as part of a site generation, the output directory configured in
     * the Maven Site Plugin is used instead.
     *
     * @parameter expression="${project.reporting.outputDirectory}"
     * @required
     * @since 1.0-alpha-3
     */
    private File outputDirectory;

    /**
     * Skip entire check.
     *
     * @parameter expression="${versions.skip}"
     * @since 1.0-alpha-3
     */
    private Boolean skip;

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
     * @since 1.0-alpha-3
     */
    private Boolean allowSnapshots;

    /**
     * Our versions helper.
     */
    private VersionsHelper helper;

    // --------------------- GETTER / SETTER METHODS ---------------------

    public VersionsHelper getHelper()
        throws MavenReportException
    {
        if ( helper == null )
        {
            try
            {
                helper = new DefaultVersionsHelper( artifactFactory, artifactMetadataSource, remoteArtifactRepositories,
                                                    remotePluginRepositories, localRepository, wagonManager, settings,
                                                    serverId, rulesUri, comparisonMethod, getLog() );
            }
            catch ( MojoExecutionException e )
            {
                throw new MavenReportException( e.getMessage(), e );
            }
        }
        return helper;
    }

    /**
     * {@inheritDoc}
     */
    protected void executeReport( Locale locale )
        throws MavenReportException
    {
        if ( !Boolean.TRUE.equals( skip ) )
        {
            try
            {
                doGenerateReport( locale, getSink() );
            }
            catch ( MojoExecutionException e )
            {
                throw new MavenReportException( e.getMessage(), e );
            }
        }
    }

    /**
     * generates the report.
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool.
     * @throws MavenReportException when things go wrong.
     */
    protected abstract void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException, MojoExecutionException;

    /**
     * Finds the latest version of the specified artifact that matches the version range.
     *
     * @param artifact          The artifact.
     * @param versionRange      The version range.
     * @param allowingSnapshots <code>null</code> for no override, otherwise the local override to apply.
     * @return The latest version of the specified artifact that matches the specified version range or
     *         <code>null</code> if no matching version could be found.
     * @throws MojoExecutionException If the artifact metadata could not be found.
     * @since 1.0-alpha-1
     */
    protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                 Boolean allowingSnapshots, boolean usePluginRepositories )
        throws MavenReportException
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
        try
        {
            final ArtifactVersions artifactVersions =
                getHelper().lookupArtifactVersions( artifact, usePluginRepositories );
            return artifactVersions.getLatestVersion( versionRange, includeSnapshots );
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * @see org.apache.maven.reporting.AbstractMavenReport#getProject()
     */
    protected MavenProject getProject()
    {
        return project;
    }

    /**
     * @see org.apache.maven.reporting.AbstractMavenReport#getOutputDirectory()
     */
    protected String getOutputDirectory()
    {
        if ( !outputDirectory.isAbsolute() )
        {
            outputDirectory = new File( project.getBasedir(), outputDirectory.getPath() );
        }

        return outputDirectory.getAbsolutePath();
    }

    /**
     * @see org.apache.maven.reporting.AbstractMavenReport#getSiteRenderer()
     */
    protected Renderer getSiteRenderer()
    {
        return siteRenderer;
    }

    /**
     * @see org.apache.maven.reporting.MavenReport#getDescription(java.util.Locale)
     */
    public String getDescription( Locale locale )
    {
        return getText( locale, "report.description" );
    }

    /**
     * @see org.apache.maven.reporting.MavenReport#getName(java.util.Locale)
     */
    public String getName( Locale locale )
    {
        return getText( locale, "report.title" );
    }

    /**
     * Gets the localized message for this report.
     *
     * @param locale the locale.
     * @param key    the message key.
     * @return the message.
     */
    public String getText( Locale locale, String key )
    {
        return i18n.getString( getOutputName(), locale, key );
    }

    public Boolean getAllowSnapshots()
    {
        return this.allowSnapshots;
    }

    public String getComparisonMethod()
    {
        return comparisonMethod;
    }

    public ArtifactResolver getResolver()
    {
        return resolver;
    }

    public I18N getI18n()
    {
        return i18n;
    }

    /**
     * Creates an {@link org.codehaus.mojo.versions.ArtifactUpdatesDetails} instance from a dependency.
     *
     * @param dependency The dependency.
     * @return The details of updates to the dependency.
     * @throws MavenReportException when things go wrong.
     * @since 1.0-beta-1
     */
    protected ArtifactUpdatesDetails lookupDependencyUpdates( Dependency dependency )
        throws MavenReportException
    {
        try
        {
            getLog().debug(
                "Checking " + ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() )
                    + " for updates newer than " + dependency.getVersion() );

            VersionRange versionRange = null;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( dependency.getVersion() );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MavenReportException( "Invalid version range specification: " + dependency.getVersion(), e );
            }

            return lookupArtifactUpdateDetails(
                artifactFactory.createDependencyArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                                                          versionRange, dependency.getType(),
                                                          dependency.getClassifier(), dependency.getScope() ),
                getHelper().createArtifactVersion( dependency.getVersion() ) );
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    private ArtifactUpdatesDetails lookupArtifactUpdateDetails( Artifact artifact, ArtifactVersion current )
        throws MojoExecutionException, MavenReportException
    {
        ArtifactVersions artifactVersions =
            getHelper().lookupArtifactVersions( artifact, Boolean.TRUE.equals( getAllowSnapshots() ) );

        VersionComparator versionComparator = artifactVersions.getVersionComparator();
        int segmentCount = versionComparator.getSegmentCount( current );
        ArtifactVersion nextVersion = segmentCount < 3
            ? null
            : artifactVersions.getOldestVersion( current, versionComparator.incrementSegment( current, 2 ),
                                                 Boolean.TRUE.equals( getAllowSnapshots() ), false, false );
        ArtifactVersion nextIncremental = segmentCount < 3
            ? null
            : artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 2 ),
                                                 versionComparator.incrementSegment( current, 1 ),
                                                 Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
        ArtifactVersion latestIncremental = segmentCount < 3
            ? null
            : artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 2 ),
                                                 versionComparator.incrementSegment( current, 1 ),
                                                 Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
        ArtifactVersion nextMinor = segmentCount < 2
            ? null
            : artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 1 ),
                                                 versionComparator.incrementSegment( current, 0 ),
                                                 Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
        ArtifactVersion latestMinor = segmentCount < 2
            ? null
            : artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 1 ),
                                                 versionComparator.incrementSegment( current, 0 ),
                                                 Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
        ArtifactVersion nextMajor =
            artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                               Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
        ArtifactVersion latestMajor =
            artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                               Boolean.TRUE.equals( getAllowSnapshots() ), true, false );

        return new ArtifactUpdatesDetails( artifact, nextVersion, nextIncremental, latestIncremental, nextMinor,
                                    latestMinor, nextMajor, latestMajor,
                                    artifactVersions.getNewerVersions( current ) );
    }

    /**
     * Looks up the updates for a set of dependencies.
     *
     * @param dependencies The set of {@link Dependency} instances to look up.
     * @return A map, keyed by dependency, with values of type {@link org.codehaus.mojo.versions.ArtifactUpdatesDetails}.
     * @throws MavenReportException when things go wrong.
     * @since 1.0-beta-1
     */
    protected Map/*<Dependency,ArtifactUpdatesDetails>*/ lookupDependenciesUpdates( Set dependencies )
        throws MavenReportException
    {
        Map/*<Dependency,ArtifactUpdatesDetails>*/ dependencyUpdates = new TreeMap( new DependencyComparator() );
        Iterator i = dependencies.iterator();
        while ( i.hasNext() )
        {
            Dependency dependency = (Dependency) i.next();
            ArtifactUpdatesDetails details = lookupDependencyUpdates( dependency );
            dependencyUpdates.put( dependency, details );
        }
        return dependencyUpdates;
    }

    /**
     * Looks up the updates for a set of dependencies.
     *
     * @param plugins The set of {@link Dependency} instances to look up.
     * @return A map, keyed by dependency, with values of type {@link org.codehaus.mojo.versions.ArtifactUpdatesDetails}.
     * @throws MavenReportException when things go wrong.
     * @since 1.0-beta-1
     */
    protected Map/*<Plugin,PluginUpdateDetails>*/ lookupPluginsUpdates( Set plugins )
        throws MavenReportException
    {
        Map/*<Plugin,PluginUpdateDetails>*/ pluginUpdates = new TreeMap( new PluginComparator() );
        Iterator i = plugins.iterator();
        while ( i.hasNext() )
        {
            Plugin plugin = (Plugin) i.next();
            PluginUpdatesDetails details = lookupPluginUpdates( plugin );
            pluginUpdates.put( plugin, details );
        }
        return pluginUpdates;
    }

    private PluginUpdatesDetails lookupPluginUpdates( Plugin plugin )
        throws MavenReportException
    {
        try
        {
            getLog().debug( "Checking " + ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() )
                + " for updates newer than " + plugin.getVersion() );

            VersionRange versionRange = VersionRange.createFromVersion( plugin.getVersion() );

            final ArtifactUpdatesDetails pluginArtifactDetails = lookupArtifactUpdateDetails(
                artifactFactory.createPluginArtifact( plugin.getGroupId(), plugin.getArtifactId(), versionRange ),
                getHelper().createArtifactVersion( plugin.getVersion() ) );

            Set/*<Dependency>*/ pluginDependencies = new TreeSet( new DependencyComparator() );
            if ( plugin.getDependencies() != null )
            {
                pluginDependencies.addAll( plugin.getDependencies() );
            }
            Map/*<Dependency,ArtifactUpdatesDetails>*/ pluginDependencyDetails =
                lookupDependenciesUpdates( pluginDependencies );

            return new PluginUpdatesDetails(pluginArtifactDetails, pluginDependencyDetails);
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }
}
