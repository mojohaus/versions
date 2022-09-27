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
import javax.xml.stream.events.XMLEvent;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.maven.BuildFailureException;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.execution.RuntimeInformation;
import org.apache.maven.lifecycle.Lifecycle;
import org.apache.maven.lifecycle.LifecycleExecutionException;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.lifecycle.mapping.LifecycleMapping;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.model.building.DefaultModelBuildingRequest;
import org.apache.maven.model.building.ModelBuildingRequest;
import org.apache.maven.model.building.ModelProblemCollector;
import org.apache.maven.model.building.ModelProblemCollectorRequest;
import org.apache.maven.model.interpolation.ModelInterpolator;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.apache.maven.plugin.InvalidPluginException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.PluginManager;
import org.apache.maven.plugin.PluginManagerException;
import org.apache.maven.plugin.PluginNotFoundException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.version.PluginVersionNotFoundException;
import org.apache.maven.plugin.version.PluginVersionResolutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.DefaultProjectBuilderConfiguration;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.component.repository.exception.ComponentLookupException;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.ReaderFactory;
import org.codehaus.plexus.util.StringUtils;

/**
 * Displays all plugins that have newer versions available, taking care of Maven version prerequisites.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo( name = "display-plugin-updates", threadSafe = true )
public class DisplayPluginUpdatesMojo
    extends AbstractVersionsDisplayMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * The width to pad warn messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int WARN_PAD_SIZE = 65;

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 68;

    /**
     * String to flag a plugin version being forced by the super-pom.
     *
     * @since 1.0-alpha-1
     */
    private static final String FROM_SUPER_POM = "(from super-pom) ";

    /**
     * @since 1.0-alpha-1
     */
    private LifecycleExecutor lifecycleExecutor;

    /**
     * @since 1.0-alpha-3
     */
    private ModelInterpolator modelInterpolator;

    /**
     * The plugin manager.
     *
     * @since 1.0-alpha-1
     */
    private PluginManager pluginManager;

    /**
     * @since 1.3
     */
    private RuntimeInformation runtimeInformation;

    // --------------------- GETTER / SETTER METHODS ---------------------

    @Inject
    @SuppressWarnings( "checkstyle:ParameterNumber" )
    public DisplayPluginUpdatesMojo( RepositorySystem repositorySystem,
                                     MavenProjectBuilder projectBuilder,
                                     ArtifactMetadataSource artifactMetadataSource,
                                     WagonManager wagonManager,
                                     ArtifactResolver artifactResolver,
                                     LifecycleExecutor lifecycleExecutor,
                                     ModelInterpolator modelInterpolator,
                                     PluginManager pluginManager,
                                     RuntimeInformation runtimeInformation )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
        this.lifecycleExecutor = lifecycleExecutor;
        this.modelInterpolator = modelInterpolator;
        this.pluginManager = pluginManager;
        this.runtimeInformation = runtimeInformation;
    }

    /**
     * Returns the pluginManagement section of the super-pom.
     *
     * @return Returns the pluginManagement section of the super-pom.
     * @throws MojoExecutionException when things go wrong.
     */
    private Map<String, String> getSuperPomPluginManagement()
        throws MojoExecutionException
    {
        if ( new DefaultArtifactVersion( "3.0" ).compareTo( runtimeInformation.getApplicationVersion() ) <= 0 )
        {
            getLog().debug( "Using Maven 3.x strategy to determine superpom defined plugins" );
            try
            {
                Method getPluginsBoundByDefaultToAllLifecycles =
                    LifecycleExecutor.class.getMethod( "getPluginsBoundByDefaultToAllLifecycles",
                            String.class );
                Set<Plugin> plugins =
                    (Set<Plugin>) getPluginsBoundByDefaultToAllLifecycles.invoke( lifecycleExecutor, new Object[] {
                        getProject().getPackaging()} );
                // we need to provide a copy with the version blanked out so that inferring from super-pom
                // works as for 2.x as 3.x fills in the version on us!
                Map<String, String> result = new LinkedHashMap<>( plugins.size() );
                for ( Plugin plugin : plugins )
                {
                    result.put( plugin.getKey(), plugin.getVersion() );
                }
                URL superPom = getClass().getClassLoader().getResource( "org/apache/maven/model/pom-4.0.0.xml" );
                if ( superPom != null )
                {
                    try
                    {
                        try ( Reader reader = ReaderFactory.newXmlReader( superPom ) )
                        {
                            StringBuilder buf = new StringBuilder( IOUtil.toString( reader ) );
                            ModifiedPomXMLEventReader pom = newModifiedPomXER( buf, superPom.toString() );

                            Pattern pathRegex = Pattern.compile( "/project(/profiles/profile)?"
                                                                     + "((/build(/pluginManagement)?)|(/reporting))"
                                                                     + "/plugins/plugin" );
                            Stack<StackState> pathStack = new Stack<>();
                            StackState curState = null;
                            while ( pom.hasNext() )
                            {
                                XMLEvent event = pom.nextEvent();
                                if ( event.isStartDocument() )
                                {
                                    curState = new StackState( "" );
                                    pathStack.clear();
                                }
                                else if ( event.isStartElement() )
                                {
                                    String elementName = event.asStartElement().getName().getLocalPart();
                                    if ( curState != null && pathRegex.matcher( curState.path ).matches() )
                                    {
                                        if ( "groupId".equals( elementName ) )
                                        {
                                            curState.groupId = pom.getElementText().trim();
                                            continue;
                                        }
                                        else if ( "artifactId".equals( elementName ) )
                                        {
                                            curState.artifactId = pom.getElementText().trim();
                                            continue;

                                        }
                                        else if ( "version".equals( elementName ) )
                                        {
                                            curState.version = pom.getElementText().trim();
                                            continue;
                                        }
                                    }

                                    pathStack.push( curState );
                                    curState = new StackState( curState.path + "/" + elementName );
                                }
                                else if ( event.isEndElement() )
                                {
                                    if ( curState != null && pathRegex.matcher( curState.path ).matches() )
                                    {
                                        if ( curState.artifactId != null )
                                        {
                                            Plugin plugin = new Plugin();
                                            plugin.setArtifactId( curState.artifactId );
                                            plugin.setGroupId( curState.groupId == null
                                                                   ? PomHelper.APACHE_MAVEN_PLUGINS_GROUPID
                                                                   : curState.groupId );
                                            plugin.setVersion( curState.version );
                                            if ( !result.containsKey( plugin.getKey() ) )
                                            {
                                                result.put( plugin.getKey(), plugin.getVersion() );
                                            }
                                        }
                                    }
                                    curState = pathStack.pop();
                                }
                            }
                        }
                    }
                    catch ( IOException | XMLStreamException e )
                    {
                        // ignore
                    }
                }

                return result;
            }
            catch ( NoSuchMethodException | InvocationTargetException | IllegalAccessException e1 )
            {
                // no much we can do here
            }
        }
        getLog().debug( "Using Maven 2.x strategy to determine superpom defined plugins" );
        Map<String, String> superPomPluginManagement;
        try
        {
            MavenProject superProject =
                projectBuilder.buildStandaloneSuperProject( new DefaultProjectBuilderConfiguration() );
            superPomPluginManagement = new HashMap<>( getPluginManagement( superProject.getOriginalModel() ) );
        }
        catch ( ProjectBuildingException e )
        {
            throw new MojoExecutionException( "Could not determine the super pom.xml", e );
        }
        return superPomPluginManagement;
    }

    /**
     * Gets the plugin management plugins of a specific project.
     *
     * @param model the model to get the plugin management plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getPluginManagement( Model model )
    {
        // we want only those parts of pluginManagement that are defined in this project
        Map<String, String> pluginManagement = new HashMap<>();
        try
        {
            for ( Plugin plugin : model.getBuild().getPluginManagement().getPlugins() )
            {
                String coord = plugin.getKey();
                String version = plugin.getVersion();
                if ( version != null )
                {
                    pluginManagement.put( coord, version );
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        try
        {
            for ( Profile profile : model.getProfiles() )
            {
                try
                {
                    for ( Plugin plugin : profile.getBuild().getPluginManagement().getPlugins() )
                    {
                        String coord = plugin.getKey();
                        String version = plugin.getVersion();
                        if ( version != null )
                        {
                            pluginManagement.put( coord, version );
                        }
                    }
                }
                catch ( NullPointerException e )
                {
                    // guess there are no plugins here
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no profiles here
        }

        return pluginManagement;
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    @SuppressWarnings( "checkstyle:MethodLength" )
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();
        Set<String> pluginsWithVersionsSpecified;
        try
        {
            pluginsWithVersionsSpecified = findPluginsWithVersionsSpecified( getProject() );
        }
        catch ( XMLStreamException | IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

        Map<String, String> superPomPluginManagement = getSuperPomPluginManagement();
        getLog().debug( "superPom plugins = " + superPomPluginManagement );

        List<MavenProject> parents = getParentProjects( getProject() );
        Map<String, String> parentPlugins = getParentsPlugins( parents );
        // TODO remove, not used any more (found while extracting getParentsPlugins method and
        //      renaming parentPluginManagement to parentPlugins)
        // NOTICE: getProjectPlugins() takes profiles while getParentPlugins does not
        //         there is probably a little inconsistency (if plugins configured in profiles of parents)
        Map<String, String> parentBuildPlugins = new HashMap<>();
        Map<String, String> parentReportPlugins = new HashMap<>();

        Set<Plugin> plugins = getProjectPlugins( superPomPluginManagement, parentPlugins, parentBuildPlugins,
                                                 parentReportPlugins, pluginsWithVersionsSpecified );

        List<String> pluginUpdates = new ArrayList<>();
        List<String> pluginLockdowns = new ArrayList<>();
        ArtifactVersion curMavenVersion = runtimeInformation.getApplicationVersion();
        ArtifactVersion specMavenVersion = MinimalMavenBuildVersionFinder.find( getProject(), "2.0", getLog() );
        ArtifactVersion minMavenVersion = null;
        boolean superPomDrivingMinVersion = false;
        // if Maven prerequisite upgraded to a version, Map<plugin compact key, latest compatible plugin vesion>
        Map<ArtifactVersion, Map<String, String>> mavenUpgrades = new TreeMap<>( new MavenVersionComparator() );

        for ( Plugin plugin : plugins )
        {
            String groupId = plugin.getGroupId();
            String artifactId = plugin.getArtifactId();
            String version = plugin.getVersion();
            String coords = ArtifactUtils.versionlessKey( groupId, artifactId );

            if ( version == null )
            {
                version = parentPlugins.get( coords );
            }

            boolean versionSpecifiedInCurrentPom = pluginsWithVersionsSpecified.contains( coords );
            if ( !versionSpecifiedInCurrentPom && parentPlugins.containsKey( coords ) )
            {
                getLog().debug( "Skip " + coords + ", version " + version + " is defined in parent POM." );
                continue;
            }

            getLog().debug( "Checking " + coords + " for updates newer than " + version );
            String effectiveVersion = version;

            Artifact artifactRange = getHelper().createPluginArtifact( plugin.getGroupId(), plugin.getArtifactId(),
                                                                       version );

            ArtifactVersion artifactVersion = null;
            try
            {
                // now we want to find the newest versions and check their Maven version prerequisite
                ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifactRange, true );
                ArtifactVersion[] newerVersions = artifactVersions.getVersions( this.allowSnapshots );
                ArtifactVersion minRequires = null;
                for ( int j = newerVersions.length - 1; j >= 0; j-- )
                {
                    Artifact probe = getHelper().createDependencyArtifact( DependencyBuilder.newBuilder()
                        .withGroupId( groupId )
                        .withArtifactId( artifactId )
                        .withVersion( newerVersions[j].toString() )
                        .withType( "pom" )
                        .withScope( Artifact.SCOPE_RUNTIME )
                        .build() );
                    try
                    {
                        getHelper().resolveArtifact( probe, true );
                        MavenProject pluginMavenProject =
                            projectBuilder.buildFromRepository( probe, remotePluginRepositories, localRepository );
                        ArtifactVersion pluginRequires = getPrerequisitesMavenVersion( pluginMavenProject );
                        if ( artifactVersion == null && compare( specMavenVersion, pluginRequires ) >= 0 )
                        {
                            // ok, newer version compatible with current specMavenVersion
                            artifactVersion = newerVersions[j];
                        }
                        if ( effectiveVersion == null && compare( curMavenVersion, pluginRequires ) >= 0 )
                        {
                            // version was unspecified, current version of maven thinks it should use this
                            effectiveVersion = newerVersions[j].toString();
                        }
                        if ( artifactVersion != null && effectiveVersion != null )
                        {
                            // no need to look at any older versions: latest compatible found
                            break;
                        }
                        // newer version not compatible with current specMavenVersion: track opportunity if Maven spec
                        // upgrade
                        if ( minRequires == null || compare( minRequires, pluginRequires ) > 0 )
                        {
                            Map<String, String> upgradePlugins =
                                mavenUpgrades.computeIfAbsent( pluginRequires, k -> new LinkedHashMap<>() );

                            String upgradePluginKey = compactKey( groupId, artifactId );
                            if ( !upgradePlugins.containsKey( upgradePluginKey ) )
                            {
                                String newer = newerVersions[j].toString();
                                if ( newer.equals( effectiveVersion ) )
                                {
                                    // plugin version configured that require a Maven version higher than spec
                                    upgradePlugins.put( upgradePluginKey,
                                                        pad( upgradePluginKey,
                                                             INFO_PAD_SIZE + getOutputLineWidthOffset(), newer ) );
                                }
                                else
                                {
                                    // plugin that can be upgraded
                                    upgradePlugins.put( upgradePluginKey, pad( upgradePluginKey, INFO_PAD_SIZE
                                                                                   + getOutputLineWidthOffset(),
                                                                               effectiveVersion, " -> ", newer ) );
                                }
                            }
                            minRequires = pluginRequires;
                        }
                    }
                    catch ( ArtifactResolutionException | ArtifactNotFoundException | ProjectBuildingException e )
                    {
                        // ignore bad version
                    }
                }
                if ( effectiveVersion != null )
                {
                    Artifact probe = getHelper().createDependencyArtifact( DependencyBuilder.newBuilder()
                            .withGroupId( groupId )
                            .withArtifactId( artifactId )
                            .withVersion( effectiveVersion )
                            .withType( "pom" )
                            .withScope( Artifact.SCOPE_RUNTIME )
                            .build() );
                    try
                    {
                        getHelper().resolveArtifact( probe, true );
                        MavenProject mavenProject =
                            projectBuilder.buildFromRepository( probe, remotePluginRepositories, localRepository );
                        ArtifactVersion requires = getPrerequisitesMavenVersion( mavenProject );
                        if ( minMavenVersion == null || compare( minMavenVersion, requires ) < 0 )
                        {
                            minMavenVersion = requires;
                        }
                    }
                    catch ( ArtifactResolutionException | ArtifactNotFoundException | ProjectBuildingException e )
                    {
                        // ignore bad version
                    }
                }
            }
            catch ( ArtifactMetadataRetrievalException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }

            String newVersion;

            if ( version == null && versionSpecifiedInCurrentPom )
            {
                // Hack ALERT!
                //
                // All this should be re-written in a less "pom is xml" way... but it'll
                // work for now :-(
                //
                // we have removed the version information, as it was the same as from
                // the super-pom... but it actually was specified.
                version = artifactVersion != null ? artifactVersion.toString() : null;
            }

            getLog().debug( "[" + coords + "].version=" + version );
            getLog().debug( "[" + coords + "].artifactVersion=" + artifactVersion );
            getLog().debug( "[" + coords + "].effectiveVersion=" + effectiveVersion );
            getLog().debug( "[" + coords + "].specified=" + versionSpecifiedInCurrentPom );
            if ( version == null || !versionSpecifiedInCurrentPom )
            {
                version = superPomPluginManagement.get( coords );
                getLog().debug( "[" + coords + "].superPom.version=" + version );

                newVersion = artifactVersion != null ? artifactVersion.toString()
                    : ( version != null ? version
                    : ( effectiveVersion != null ? effectiveVersion : "(unknown)" ) );
                if ( version != null )
                {
                    superPomDrivingMinVersion = true;
                }

                pluginLockdowns.add( pad( compactKey( groupId, artifactId ), WARN_PAD_SIZE + getOutputLineWidthOffset(),
                                          superPomDrivingMinVersion ? FROM_SUPER_POM : "", newVersion ) );
            }
            else if ( artifactVersion != null )
            {
                newVersion = artifactVersion.toString();
            }
            else
            {
                newVersion = null;
            }
            if ( version != null && artifactVersion != null && newVersion != null && effectiveVersion != null
                && new DefaultArtifactVersion( effectiveVersion ).compareTo( new DefaultArtifactVersion( newVersion ) )
                < 0 )
            {
                pluginUpdates.add( pad( compactKey( groupId, artifactId ), INFO_PAD_SIZE + getOutputLineWidthOffset(),
                                        effectiveVersion, " -> ", newVersion ) );
            }
        }

        // info on each plugin gathered: now it's time to display the result!
        //
        logLine( false, "" );

        // updates keeping currently defined Maven version minimum
        if ( pluginUpdates.isEmpty() )
        {
            logLine( false, "All plugins with a version specified are using the latest versions." );
        }
        else
        {
            logLine( false, "The following plugin updates are available:" );
            for ( String update : new TreeSet<>( pluginUpdates ) )
            {
                logLine( false, update );
            }
        }
        logLine( false, "" );

        // has every plugin a specified version?
        if ( pluginLockdowns.isEmpty() )
        {
            logLine( false, "All plugins have a version specified." );
        }
        else
        {
            getLog().warn( "The following plugins do not have their version specified:" );
            for ( String lockdown : new TreeSet<>( pluginLockdowns ) )
            {
                getLog().warn( lockdown );
            }
        }
        logLine( false, "" );

        // information on minimum Maven version
        boolean noMavenMinVersion = MinimalMavenBuildVersionFinder.find( getProject(), null, getLog() ) == null;
        if ( noMavenMinVersion )
        {
            getLog().warn( "Project does not define minimum Maven version required for build, default is: 2.0" );
        }
        else
        {
            logLine( false, "Project requires minimum Maven version for build of: " + specMavenVersion );
        }
        logLine( false, "Plugins require minimum Maven version of: " + minMavenVersion );
        if ( superPomDrivingMinVersion )
        {
            logLine( false, "Note: the super-pom from Maven " + curMavenVersion + " defines some of the plugin" );
            logLine( false, "      versions and may be influencing the plugins required minimum Maven" );
            logLine( false, "      version." );
        }
        logLine( false, "" );

        if ( isMavenPluginProject() )
        {
            if ( noMavenMinVersion )
            {
                getLog().warn( "Project (which is a Maven plugin) does not define required minimum version of Maven." );
                getLog().warn( "Update the pom.xml to contain" );
                getLog().warn( "    <prerequisites>" );
                getLog().warn( "      <maven><!-- minimum version of Maven that the plugin works with --></maven>" );
                getLog().warn( "    </prerequisites>" );
                getLog().warn( "To build this plugin you need at least Maven " + minMavenVersion );
                getLog().warn( "A Maven Enforcer rule can be used to enforce this if you have not already set one up" );
                getLog().warn( "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
            }
            else if ( minMavenVersion != null && compare( specMavenVersion, minMavenVersion ) < 0 )
            {
                getLog().warn( "Project (which is a Maven plugin) targets Maven " + specMavenVersion + " or newer" );
                getLog().warn( "but requires Maven " + minMavenVersion + " or newer to build." );
                getLog().warn( "This may or may not be a problem. A Maven Enforcer rule can help " );
                getLog().warn( "enforce that the correct version of Maven is used to build this plugin." );
                getLog().warn( "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
            }
            else
            {
                logLine( false, "No plugins require a newer version of Maven than specified by the pom." );
            }
        }
        else
        {
            if ( noMavenMinVersion )
            {
                logLine( true, "Project does not define required minimum version of Maven." );
                logLine( true, "Update the pom.xml to contain maven-enforcer-plugin to" );
                logLine( true, "force the Maven version which is needed to build this project." );
                logLine( true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
                logLine( true, "Using the minimum version of Maven: " + minMavenVersion );
            }
            else if ( minMavenVersion != null && compare( specMavenVersion, minMavenVersion ) < 0 )
            {
                logLine( true, "Project requires an incorrect minimum version of Maven." );
                logLine( true, "Update the pom.xml to contain maven-enforcer-plugin to" );
                logLine( true, "force the Maven version which is needed to build this project." );
                logLine( true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
                logLine( true, "Using the minimum version of Maven: " + specMavenVersion );
            }
            else
            {
                logLine( false, "No plugins require a newer version of Maven than specified by the pom." );
            }
        }

        // updates if minimum Maven version is changed
        for ( Map.Entry<ArtifactVersion, Map<String, String>> mavenUpgrade : mavenUpgrades.entrySet() )
        {
            ArtifactVersion mavenUpgradeVersion = mavenUpgrade.getKey();
            Map<String, String> upgradePlugins = mavenUpgrade.getValue();
            if ( upgradePlugins.isEmpty() || compare( specMavenVersion, mavenUpgradeVersion ) >= 0 )
            {
                continue;
            }
            logLine( false, "" );
            logLine( false, "Require Maven " + mavenUpgradeVersion + " to use the following plugin updates:" );
            for ( Map.Entry<String, String> entry : upgradePlugins.entrySet() )
            {
                logLine( false, entry.getValue() );
            }
        }
        logLine( false, "" );
    }

    private static String pad( String start, int len, String... ends )
    {
        StringBuilder buf = new StringBuilder( len );
        buf.append( "  " );
        buf.append( start );
        int padding = len;
        for ( String end : ends )
        {
            padding -= end.length();
        }
        buf.append( ' ' );
        while ( buf.length() < padding )
        {
            buf.append( '.' );
        }
        buf.append( ' ' );
        for ( String end : ends )
        {
            buf.append( end );
        }
        return buf.toString();
    }

    private Map<String, String> getParentsPlugins( List<MavenProject> parents )
        throws MojoExecutionException
    {
        Map<String, String> parentPlugins = new HashMap<>();
        for ( MavenProject parentProject : parents )
        {
            getLog().debug( "Processing parent: " + parentProject.getGroupId() + ":" + parentProject.getArtifactId()
                                + ":" + parentProject.getVersion() + " -> " + parentProject.getFile() );

            StringWriter writer = new StringWriter();
            boolean havePom = false;
            Model interpolatedModel;

            Model originalModel = parentProject.getOriginalModel();
            if ( originalModel == null )
            {
                getLog().warn( "project.getOriginalModel()==null for  " + parentProject.getGroupId() + ":"
                                   + parentProject.getArtifactId() + ":" + parentProject.getVersion()
                                   + " is null, substituting project.getModel()" );
                originalModel = parentProject.getModel();
            }
            try
            {
                new MavenXpp3Writer().write( writer, originalModel );
                writer.close();
                havePom = true;
            }
            catch ( IOException e )
            {
                // ignore
            }
            ModelBuildingRequest modelBuildingRequest = new DefaultModelBuildingRequest();
            modelBuildingRequest.setUserProperties( getProject().getProperties() );
            interpolatedModel = modelInterpolator.interpolateModel( originalModel, null,
                                                                    modelBuildingRequest,
                                                                    new IgnoringModelProblemCollector() );
            if ( havePom )
            {
                try
                {
                    Set<String> withVersionSpecified =
                        findPluginsWithVersionsSpecified( new StringBuilder( writer.toString() ),
                                                          getSafeProjectPathInfo( parentProject ) );

                    Map<String, String> map = getPluginManagement( interpolatedModel );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPlugins.putAll( map );

                    map = getBuildPlugins( interpolatedModel, true );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPlugins.putAll( map );

                    map = getReportPlugins( interpolatedModel, true );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPlugins.putAll( map );
                }
                catch ( XMLStreamException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
            else
            {
                parentPlugins.putAll( getPluginManagement( interpolatedModel ) );
                parentPlugins.putAll( getBuildPlugins( interpolatedModel, true ) );
                parentPlugins.putAll( getReportPlugins( interpolatedModel, true ) );
            }
        }
        return parentPlugins;
    }

    private String getSafeProjectPathInfo( MavenProject project )
    {
        File file = project.getFile();
        if ( file != null )
        {
            return file.getAbsolutePath();
        }
        else
        {
            // path is used only as information in error message, we can fallback to project artifact info here
            return project.toString();
        }
    }

    private boolean isMavenPluginProject()
    {
        return "maven-plugin".equals( getProject().getPackaging() );
    }

    private String compactKey( String groupId, String artifactId )
    {
        if ( PomHelper.APACHE_MAVEN_PLUGINS_GROUPID.equals( groupId ) )
        {
            // a core plugin... group id is not needed
            return artifactId;
        }
        return groupId + ":" + artifactId;
    }

    private static final class StackState
    {
        private final String path;

        private String groupId;

        private String artifactId;

        private String version;

        StackState( String path )
        {
            this.path = path;
        }

        public String toString()
        {
            return path + "[groupId=" + groupId + ", artifactId=" + artifactId + ", version=" + version + "]";
        }
    }

    /**
     * Returns a set of Strings which correspond to the plugin coordinates where there is a version specified.
     *
     * @param project The project to get the plugins with versions specified.
     * @return a set of Strings which correspond to the plugin coordinates where there is a version specified.
     */
    private Set<String> findPluginsWithVersionsSpecified( MavenProject project )
        throws IOException, XMLStreamException
    {
        return findPluginsWithVersionsSpecified( PomHelper.readXmlFile( project.getFile() ),
                                                 getSafeProjectPathInfo( project ) );
    }

    /**
     * Returns a set of Strings which correspond to the plugin coordinates where there is a version specified.
     *
     * @param pomContents The project to get the plugins with versions specified.
     * @param path        Path that points to the source of the XML
     * @return a set of Strings which correspond to the plugin coordinates where there is a version specified.
     */
    private Set<String> findPluginsWithVersionsSpecified( StringBuilder pomContents, String path )
        throws XMLStreamException
    {
        Set<String> result = new HashSet<>();
        ModifiedPomXMLEventReader pom = newModifiedPomXER( pomContents, path );

        Pattern pathRegex = Pattern.compile( "/project(/profiles/profile)?"
                                                 + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin" );
        Stack<StackState> pathStack = new Stack<>();
        StackState curState = null;
        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartDocument() )
            {
                curState = new StackState( "" );
                pathStack.clear();
            }
            else if ( event.isStartElement() )
            {
                String elementName = event.asStartElement().getName().getLocalPart();
                if ( curState != null && pathRegex.matcher( curState.path ).matches() )
                {
                    if ( "groupId".equals( elementName ) )
                    {
                        curState.groupId = pom.getElementText().trim();
                        continue;
                    }
                    else if ( "artifactId".equals( elementName ) )
                    {
                        curState.artifactId = pom.getElementText().trim();
                        continue;

                    }
                    else if ( "version".equals( elementName ) )
                    {
                        curState.version = pom.getElementText().trim();
                        continue;
                    }
                }

                pathStack.push( curState );
                curState = new StackState( curState.path + "/" + elementName );
            }
            else if ( event.isEndElement() )
            {
                if ( curState != null && pathRegex.matcher( curState.path ).matches() )
                {
                    if ( curState.artifactId != null && curState.version != null )
                    {
                        if ( curState.groupId == null )
                        {
                            curState.groupId = PomHelper.APACHE_MAVEN_PLUGINS_GROUPID;
                        }
                        result.add( curState.groupId + ":" + curState.artifactId );
                    }
                }
                curState = pathStack.pop();
            }
        }

        return result;

    }

    // -------------------------- OTHER METHODS --------------------------

    /**
     * Get the minimum required Maven version of the given plugin
     * Same logic as in
     * @see <a href="https://github.com/apache/maven-plugin-tools/blob/c8ddcdcb10d342a5a5e2f38245bb569af5730c7c/maven-plugin-plugin/src/main/java/org/apache/maven/plugin/plugin/PluginReport.java#L711">PluginReport</a>
     *
     * @param pluginProject the plugin for which to retrieve the minimum Maven version which is required
     * @return The minimally required Maven version (never {@code null})
     */
    private ArtifactVersion getPrerequisitesMavenVersion( MavenProject pluginProject )
    {
        Prerequisites prerequisites = pluginProject.getPrerequisites();
        if ( null == prerequisites )
        {
            return new DefaultArtifactVersion( "2.0" );
        }

        String prerequisitesMavenValue = prerequisites.getMaven();
        if ( null == prerequisitesMavenValue )
        {
            return new DefaultArtifactVersion( "2.0" );
        }

        return new DefaultArtifactVersion( prerequisitesMavenValue );
    }

    /**
     * Gets the build plugins of a specific project.
     *
     * @param model                the model to get the build plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getBuildPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map<String, String> buildPlugins = new HashMap<>();
        try
        {
            for ( Plugin plugin : model.getBuild().getPlugins() )
            {
                String coord = plugin.getKey();
                String version = plugin.getVersion();
                if ( version != null && ( !onlyIncludeInherited || getPluginInherited( plugin ) ) )
                {
                    buildPlugins.put( coord, version );
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        try
        {
            for ( Profile profile : model.getProfiles() )
            {
                try
                {
                    for ( Plugin plugin : profile.getBuild().getPlugins() )
                    {
                        String coord = plugin.getKey();
                        String version = plugin.getVersion();
                        if ( version != null && ( !onlyIncludeInherited || getPluginInherited( plugin ) ) )
                        {
                            buildPlugins.put( coord, version );
                        }
                    }
                }
                catch ( NullPointerException e )
                {
                    // guess there are no plugins here
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no profiles here
        }
        return buildPlugins;
    }

    /**
     * Returns the Inherited of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Inherited of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0-alpha-1
     */
    private static boolean getPluginInherited( Object plugin )
    {
        return "true".equalsIgnoreCase( plugin instanceof ReportPlugin ? ( (ReportPlugin) plugin ).getInherited()
                                            : ( (Plugin) plugin ).getInherited() );
    }

    /**
     * Returns the lifecycle plugins of a specific project.
     *
     * @param project the project to get the lifecycle plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @throws org.apache.maven.plugin.MojoExecutionException if things go wrong.
     * @since 1.0-alpha-1
     */
    private Map<String, Plugin> getLifecyclePlugins( MavenProject project )
        throws MojoExecutionException
    {
        Map<String, Plugin> lifecyclePlugins = new HashMap<>();
        try
        {
            Set<Plugin> plugins = getBoundPlugins( project, "clean,deploy,site" );
            for ( Plugin plugin : plugins )
            {
                lifecyclePlugins.put( plugin.getKey(), plugin );
            }
        }
        catch ( PluginNotFoundException e )
        {
            throw new MojoExecutionException( "Could not find plugin", e );
        }
        catch ( LifecycleExecutionException e )
        {
            throw new MojoExecutionException( "Could not determine lifecycle", e );
        }
        catch ( IllegalAccessException e )
        {
            throw new MojoExecutionException( "Could not determine lifecycles", e );
        }
        catch ( NullPointerException e )
        {
            // Maven 3.x

        }
        return lifecyclePlugins;
    }

    /**
     * Gets the plugins that are bound to the defined phases. This does not find plugins bound in the pom to a phase
     * later than the plugin is executing.
     *
     * @param project   the project
     * @param thePhases the the phases
     * @return the bound plugins
     * @throws org.apache.maven.plugin.PluginNotFoundException the plugin not found exception
     * @throws LifecycleExecutionException                     the lifecycle execution exception
     * @throws IllegalAccessException                          the illegal access exception
     */
    // pilfered this from enforcer-rules
    // TODO coordinate with Brian Fox to remove the duplicate code
    private Set<Plugin> getBoundPlugins( MavenProject project, String thePhases )
        throws PluginNotFoundException, LifecycleExecutionException, IllegalAccessException
    {
        if ( new DefaultArtifactVersion( "3.0" ).compareTo( runtimeInformation.getApplicationVersion() ) <= 0 )
        {
            getLog().debug( "Using Maven 3.0+ strategy to determine lifecycle defined plugins" );
            try
            {
                Method getPluginsBoundByDefaultToAllLifecycles =
                    LifecycleExecutor.class.getMethod( "getPluginsBoundByDefaultToAllLifecycles",
                            String.class );
                Set<Plugin> plugins =
                    (Set<Plugin>) getPluginsBoundByDefaultToAllLifecycles.invoke( lifecycleExecutor, new Object[] {
                        project.getPackaging() == null ? "jar" : project.getPackaging()} );
                // we need to provide a copy with the version blanked out so that inferring from super-pom
                // works as for 2.x as 3.x fills in the version on us!
                Set<Plugin> result = new LinkedHashSet<>( plugins.size() );
                for ( Plugin plugin : plugins )
                {
                    Plugin dup = new Plugin();
                    dup.setGroupId( plugin.getGroupId() );
                    dup.setArtifactId( plugin.getArtifactId() );
                    result.add( dup );
                }
                return result;
            }
            catch ( NoSuchMethodException | InvocationTargetException | IllegalAccessException e1 )
            {
                // no much we can do here
            }
        }
        List<Lifecycle> lifecycles = null;
        getLog().debug( "Using Maven 2.0.10+ strategy to determine lifecycle defined plugins" );
        try
        {
            Method getLifecycles = LifecycleExecutor.class.getMethod( "getLifecycles" );
            lifecycles = (List) getLifecycles.invoke( lifecycleExecutor, new Object[0] );
        }
        catch ( NoSuchMethodException | InvocationTargetException | IllegalAccessException e1 )
        {
            // no much we can do here
        }

        Set<Plugin> allPlugins = new HashSet<>();

        // lookup the bindings for all the passed in phases
        for ( String lifecyclePhase : thePhases.split( "," ) )
        {
            if ( StringUtils.isNotEmpty( lifecyclePhase ) )
            {
                try
                {
                    Lifecycle lifecycle = getLifecycleForPhase( lifecycles, lifecyclePhase );
                    allPlugins.addAll( getAllPlugins( project, lifecycle ) );
                }
                catch ( BuildFailureException e )
                {
                    // i'm going to swallow this because the
                    // user may have declared a phase that
                    // doesn't exist for every module.
                }
            }
        }
        return allPlugins;
    }

    /**
     * Gets the lifecycle for phase.
     *
     * @param lifecycles The list of lifecycles.
     * @param phase      the phase
     * @return the lifecycle for phase
     * @throws BuildFailureException       the build failure exception
     * @throws LifecycleExecutionException the lifecycle execution exception
     */
    private Lifecycle getLifecycleForPhase( List<Lifecycle> lifecycles, String phase )
        throws BuildFailureException, LifecycleExecutionException
    {
        Lifecycle lifecycle = getPhaseToLifecycleMap( lifecycles ).get( phase );

        if ( lifecycle == null )
        {
            throw new BuildFailureException( "Unable to find lifecycle for phase '" + phase + "'" );
        }
        return lifecycle;
    }

    /*
     * Uses borrowed lifecycle code to get a list of all plugins bound to the lifecycle.
     */

    /**
     * Gets the all plugins.
     *
     * @param project   the project
     * @param lifecycle the lifecycle
     * @return the all plugins
     * @throws PluginNotFoundException     the plugin not found exception
     * @throws LifecycleExecutionException the lifecycle execution exception
     */
    private Set<Plugin> getAllPlugins( MavenProject project, Lifecycle lifecycle )
        throws PluginNotFoundException, LifecycleExecutionException

    {
        Set<Plugin> plugins = new HashSet<>();
        // first, bind those associated with the packaging
        Map<?, ?> mappings = findMappingsForLifecycle( project, lifecycle );

        for ( Map.Entry<?, ?> entry : mappings.entrySet() )
        {
            String value = (String) entry.getValue();
            String[] tokens = value.split( ":" );

            Plugin plugin = new Plugin();
            plugin.setGroupId( tokens[0] );
            plugin.setArtifactId( tokens[1] );
            plugins.add( plugin );
        }

        for ( String value : findOptionalMojosForLifecycle( project, lifecycle ) )
        {
            String[] tokens = value.split( ":" );

            Plugin plugin = new Plugin();
            plugin.setGroupId( tokens[0] );
            plugin.setArtifactId( tokens[1] );
            plugins.add( plugin );
        }

        plugins.addAll( project.getBuildPlugins() );

        return plugins;
    }

    /**
     * Find mappings for lifecycle.
     *
     * @param project   the project
     * @param lifecycle the lifecycle
     * @return the map
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws PluginNotFoundException     the plugin not found exception
     */
    private Map<?, ?> findMappingsForLifecycle( MavenProject project, Lifecycle lifecycle )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        String packaging = project.getPackaging();
        Map<?, ?> mappings = null;

        LifecycleMapping m = (LifecycleMapping) findExtension( project, LifecycleMapping.ROLE, packaging,
                                                               session.getSettings(), session.getLocalRepository() );
        if ( m != null )
        {
            mappings = m.getPhases( lifecycle.getId() );
        }

        Map<?, ?> defaultMappings = lifecycle.getDefaultPhases();

        if ( mappings == null )
        {
            try
            {
                m = (LifecycleMapping) session.lookup( LifecycleMapping.ROLE, packaging );
                mappings = m.getPhases( lifecycle.getId() );
            }
            catch ( ComponentLookupException e )
            {
                if ( defaultMappings == null )
                {
                    throw new LifecycleExecutionException( "Cannot find lifecycle mapping for packaging: '" + packaging
                                                               + "'.", e );
                }
            }
        }

        if ( mappings == null )
        {
            if ( defaultMappings == null )
            {
                throw new LifecycleExecutionException( "Cannot find lifecycle mapping for packaging: '" + packaging
                                                           + "', and there is no default" );
            }
            else
            {
                mappings = defaultMappings;
            }
        }

        return mappings;
    }

    /**
     * Find optional mojos for lifecycle.
     *
     * @param project   the project
     * @param lifecycle the lifecycle
     * @return the list
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws PluginNotFoundException     the plugin not found exception
     */
    private List<String> findOptionalMojosForLifecycle( MavenProject project, Lifecycle lifecycle )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        String packaging = project.getPackaging();
        List<String> optionalMojos = null;

        LifecycleMapping m = (LifecycleMapping) findExtension( project, LifecycleMapping.ROLE, packaging,
                                                               session.getSettings(), session.getLocalRepository() );

        if ( m != null )
        {
            optionalMojos = m.getOptionalMojos( lifecycle.getId() );
        }

        if ( optionalMojos == null )
        {
            try
            {
                m = (LifecycleMapping) session.lookup( LifecycleMapping.ROLE, packaging );
                optionalMojos = m.getOptionalMojos( lifecycle.getId() );
            }
            catch ( ComponentLookupException e )
            {
                getLog().debug( "Error looking up lifecycle mapping to retrieve optional mojos. Lifecycle ID: "
                                    + lifecycle.getId() + ". Error: " + e.getMessage(), e );
            }
        }

        if ( optionalMojos == null )
        {
            optionalMojos = Collections.emptyList();
        }

        return optionalMojos;
    }

    /**
     * Find extension.
     *
     * @param project         the project
     * @param role            the role
     * @param roleHint        the role hint
     * @param settings        the settings
     * @param localRepository the local repository
     * @return the object
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws PluginNotFoundException     the plugin not found exception
     */
    private Object findExtension( MavenProject project, String role, String roleHint, Settings settings,
                                  ArtifactRepository localRepository )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        Object pluginComponent = null;

        for ( Iterator<?> i = project.getBuildPlugins().iterator(); i.hasNext() && pluginComponent == null; )
        {
            Plugin plugin = (Plugin) i.next();

            if ( plugin.isExtensions() )
            {
                loadPluginDescriptor( plugin, project, session );

                // TODO: if moved to the plugin manager we
                // already have the descriptor from above
                // and so do can lookup the container
                // directly
                try
                {
                    pluginComponent = pluginManager.getPluginComponent( plugin, role, roleHint );
                }
                catch ( ComponentLookupException e )
                {
                    getLog().debug( "Unable to find the lifecycle component in the extension", e );
                }
                catch ( PluginManagerException e )
                {
                    throw new LifecycleExecutionException( "Error getting extensions from the plugin '"
                                                               + plugin.getKey() + "': " + e.getMessage(), e );
                }
            }
        }
        return pluginComponent;
    }

    /**
     * Verify plugin.
     *
     * @param plugin  the plugin
     * @param project the project
     * @param session the session
     * @return the plugin descriptor
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws PluginNotFoundException     the plugin not found exception
     */
    private PluginDescriptor loadPluginDescriptor( Plugin plugin, MavenProject project, MavenSession session )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        PluginDescriptor pluginDescriptor;
        try
        {
            pluginDescriptor = pluginManager.loadPluginDescriptor( plugin, project, session );
        }
        catch ( PluginManagerException e )
        {
            throw new LifecycleExecutionException( "Internal error in the plugin manager getting plugin '"
                                                       + plugin.getKey() + "': " + e.getMessage(), e );
        }
        catch ( PluginVersionResolutionException | InvalidVersionSpecificationException | InvalidPluginException //
                | ArtifactNotFoundException | ArtifactResolutionException | PluginVersionNotFoundException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        return pluginDescriptor;
    }

    /**
     * Returns all the parent projects of the specified project, with the root project first.
     *
     * @param project The maven project to get the parents of
     * @return the parent projects of the specified project, with the root project first.
     * @throws org.apache.maven.plugin.MojoExecutionException if the super-pom could not be created.
     * @since 1.0-alpha-1
     */
    private List<MavenProject> getParentProjects( MavenProject project )
        throws MojoExecutionException
    {
        List<MavenProject> parents = new ArrayList<>();
        while ( project.getParent() != null )
        {
            project = project.getParent();
            parents.add( 0, project );
        }
        return parents;
    }

    /*
     * NOTE: All the code following this point was scooped from the DefaultLifecycleExecutor. There must be a better way
     * but for now it should work.
     */

    /**
     * Gets the phase to lifecycle map.
     *
     * @param lifecycles The list of lifecycles.
     * @return the phase to lifecycle map.
     * @throws LifecycleExecutionException the lifecycle execution exception.
     */
    public Map<String, Lifecycle> getPhaseToLifecycleMap( List<Lifecycle> lifecycles )
        throws LifecycleExecutionException
    {
        Map<String, Lifecycle> phaseToLifecycleMap = new HashMap<>();

        for ( Lifecycle lifecycle : lifecycles )
        {
            for ( String phase : lifecycle.getPhases() )
            {
                if ( phaseToLifecycleMap.containsKey( phase ) )
                {
                    Lifecycle prevLifecycle = phaseToLifecycleMap.get( phase );
                    throw new LifecycleExecutionException( "Phase '" + phase
                                                               + "' is defined in more than one lifecycle: '"
                                                               + lifecycle.getId() + "' and '"
                                                               + prevLifecycle.getId() + "'" );
                }
                else
                {
                    phaseToLifecycleMap.put( phase, lifecycle );
                }
            }
        }
        return phaseToLifecycleMap;
    }

    /**
     * Returns the set of all plugins used by the project.
     *
     * @param superPomPluginManagement     the super pom's pluginManagement plugins.
     * @param parentPluginManagement       the parent pom's pluginManagement plugins.
     * @param parentBuildPlugins           the parent pom's build plugins.
     * @param parentReportPlugins          the parent pom's report plugins.
     * @param pluginsWithVersionsSpecified the plugin coords that have a version defined in the project.
     * @return the set of plugins used by the project.
     * @throws org.apache.maven.plugin.MojoExecutionException if things go wrong.
     */
    @SuppressWarnings( "checkstyle:MethodLength" )
    private Set<Plugin> getProjectPlugins( Map<String, String> superPomPluginManagement,
                                           Map<String, String> parentPluginManagement,
                                           Map<String, String> parentBuildPlugins,
                                           Map<String, String> parentReportPlugins,
                                           Set<String> pluginsWithVersionsSpecified )
        throws MojoExecutionException
    {
        Map<String, Plugin> plugins = new HashMap<>();

        getLog().debug( "Building list of project plugins..." );

        if ( getLog().isDebugEnabled() )
        {
            StringWriter origModel = new StringWriter();

            try
            {
                origModel.write( "Original model:\n" );
                getProject().writeOriginalModel( origModel );
                getLog().debug( origModel.toString() );
            }
            catch ( IOException e )
            {
                // ignore
            }
        }

        debugVersionMap( "super-pom version map", superPomPluginManagement );
        debugVersionMap( "parent version map", parentPluginManagement );

        Map<String, String> excludePluginManagement = new HashMap<>( superPomPluginManagement );
        excludePluginManagement.putAll( parentPluginManagement );

        debugVersionMap( "aggregate version map", excludePluginManagement );

        excludePluginManagement.keySet().removeAll( pluginsWithVersionsSpecified );

        debugVersionMap( "final aggregate version map", excludePluginManagement );

        ModelBuildingRequest modelBuildingRequest = new DefaultModelBuildingRequest();
        modelBuildingRequest.setUserProperties( getProject().getProperties() );
        Model originalModel =
            modelInterpolator.interpolateModel( getProject().getOriginalModel(), getProject().getBasedir(),
                                                modelBuildingRequest, new IgnoringModelProblemCollector() );

        try
        {
            addProjectPlugins( plugins, originalModel.getBuild().getPluginManagement().getPlugins(),
                               excludePluginManagement );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        debugPluginMap( "after adding local pluginManagement", plugins );

        try
        {
            List<Plugin> lifecyclePlugins = new ArrayList<>( getLifecyclePlugins( getProject() ).values() );
            for ( Iterator<Plugin> i = lifecyclePlugins.iterator(); i.hasNext(); )
            {
                Plugin lifecyclePlugin = i.next();
                if ( lifecyclePlugin.getVersion() != null )
                {
                    // version comes from lifecycle, therefore cannot modify
                    i.remove();
                }
                else
                {
                    // lifecycle leaves version open
                    String parentVersion = parentPluginManagement.get( lifecyclePlugin.getKey() );
                    if ( parentVersion != null )
                    {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins( plugins, lifecyclePlugins, parentPluginManagement );

            debugPluginMap( "after adding lifecycle plugins", plugins );
        }
        catch ( NullPointerException e )
        {
            // using maven 3.x or newer
        }

        try
        {
            List<Plugin> buildPlugins = new ArrayList<>( originalModel.getBuild().getPlugins() );
            for ( Iterator<Plugin> i = buildPlugins.iterator(); i.hasNext(); )
            {
                Plugin buildPlugin = i.next();
                if ( buildPlugin.getVersion() == null )
                {
                    String parentVersion = parentPluginManagement.get( buildPlugin.getKey() );
                    if ( parentVersion != null )
                    {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins( plugins, buildPlugins, parentBuildPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        debugPluginMap( "after adding build plugins", plugins );

        try
        {
            List<ReportPlugin> reportPlugins = new ArrayList<>( originalModel.getReporting().getPlugins() );
            for ( Iterator<ReportPlugin> i = reportPlugins.iterator(); i.hasNext(); )
            {
                ReportPlugin reportPlugin = i.next();
                if ( reportPlugin.getVersion() == null )
                {
                    String parentVersion = parentPluginManagement.get( reportPlugin.getKey() );
                    if ( parentVersion != null )
                    {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins( plugins, toPlugins( reportPlugins ), parentReportPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        debugPluginMap( "after adding reporting plugins", plugins );

        for ( Profile profile : originalModel.getProfiles() )
        {
            try
            {
                addProjectPlugins( plugins, profile.getBuild().getPluginManagement().getPlugins(),
                                   excludePluginManagement );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }
            debugPluginMap( "after adding build pluginManagement for profile " + profile.getId(), plugins );

            try
            {
                addProjectPlugins( plugins, profile.getBuild().getPlugins(), parentBuildPlugins );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }
            debugPluginMap( "after adding build plugins for profile " + profile.getId(), plugins );

            try
            {
                addProjectPlugins( plugins, toPlugins( profile.getReporting().getPlugins() ), parentReportPlugins );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }
            debugPluginMap( "after adding reporting plugins for profile " + profile.getId(), plugins );
        }
        Set<Plugin> result = new TreeSet<>( PluginComparator.INSTANCE );
        result.addAll( plugins.values() );
        return result;
    }

    /**
     * Adds those project plugins which are not inherited from the parent definitions to the list of plugins.
     *
     * @param plugins           The list of plugins.
     * @param projectPlugins    The project's plugins.
     * @param parentDefinitions The parent plugin definitions.
     * @since 1.0-alpha-1
     */
    private void addProjectPlugins( Map<String, Plugin> plugins, Collection<Plugin> projectPlugins,
                                    Map<String, String> parentDefinitions )
    {
        for ( Plugin plugin : projectPlugins )
        {
            String coord = plugin.getKey();
            String version = plugin.getVersion();
            String parentVersion = parentDefinitions.get( coord );
            if ( version == null
                && ( !plugins.containsKey( coord ) || plugins.get( coord ).getVersion() == null )
                && parentVersion != null )
            {
                Plugin parentPlugin = new Plugin();
                parentPlugin.setGroupId( plugin.getGroupId() );
                parentPlugin.setArtifactId( plugin.getArtifactId() );
                parentPlugin.setVersion( parentVersion );
                plugins.put( coord, parentPlugin );
            }
            else if ( parentVersion == null || !parentVersion.equals( version ) )
            {
                if ( !plugins.containsKey( coord ) || plugins.get( coord ).getVersion() == null )
                {
                    plugins.put( coord, plugin );
                }
            }
            if ( !plugins.containsKey( coord ) )
            {
                plugins.put( coord, plugin );
            }
        }
    }

    /**
     * Logs at debug level a map of plugins keyed by versionless key.
     *
     * @param description log description
     * @param plugins     a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                    values
     *                    being {@link Plugin} or {@link ReportPlugin}.
     */
    private void debugPluginMap( String description, Map<String, Plugin> plugins )
    {
        if ( getLog().isDebugEnabled() )
        {
            Set<Plugin> sorted = new TreeSet<>( PluginComparator.INSTANCE );
            sorted.addAll( plugins.values() );
            StringBuilder buf = new StringBuilder( description );
            for ( Plugin plugin : sorted )
            {
                buf.append( "\n    " );
                buf.append( plugin.getKey() );
                buf.append( ":" );
                buf.append( plugin.getVersion() );
            }
            getLog().debug( buf.toString() );
        }
    }

    /**
     * Logs at debug level a map of plugin versions keyed by versionless key.
     *
     * @param description    log description
     * @param pluginVersions a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                       values
     *                       being {@link String} plugin version.
     */
    private void debugVersionMap( String description, Map<String, String> pluginVersions )
    {
        if ( getLog().isDebugEnabled() )
        {
            StringBuilder buf = new StringBuilder( description );
            for ( Map.Entry<String, String> pluginVersion : pluginVersions.entrySet() )
            {
                buf.append( "\n    " );
                buf.append( pluginVersion.getKey() );
                buf.append( ":" );
                buf.append( pluginVersion.getValue() );
            }
            getLog().debug( buf.toString() );
        }
    }

    private static Plugin toPlugin( ReportPlugin reportPlugin )
    {
        Plugin plugin = new Plugin();
        plugin.setGroupId( reportPlugin.getGroupId() );
        plugin.setArtifactId( reportPlugin.getArtifactId() );
        plugin.setVersion( reportPlugin.getVersion() );
        return plugin;
    }

    private static List<Plugin> toPlugins( List<ReportPlugin> reportPlugins )
    {
        List<Plugin> result = new ArrayList<>( reportPlugins.size() );
        for ( ReportPlugin reportPlugin : reportPlugins )
        {
            result.add( toPlugin( reportPlugin ) );
        }
        return result;
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param model                the model to get the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getReportPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map<String, String> reportPlugins = new HashMap<>();
        try
        {
            for ( ReportPlugin plugin : model.getReporting().getPlugins() )
            {
                String coord = plugin.getKey();
                String version = plugin.getVersion();
                if ( version != null && ( !onlyIncludeInherited || getPluginInherited( plugin ) ) )
                {
                    reportPlugins.put( coord, version );
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        try
        {
            for ( Profile profile : model.getProfiles() )
            {
                try
                {
                    for ( ReportPlugin plugin : profile.getReporting().getPlugins() )
                    {
                        String coord = plugin.getKey();
                        String version = plugin.getVersion();
                        if ( version != null && ( !onlyIncludeInherited || getPluginInherited( plugin ) ) )
                        {
                            reportPlugins.put( coord, version );
                        }
                    }
                }
                catch ( NullPointerException e )
                {
                    // guess there are no plugins here
                }
            }
        }
        catch ( NullPointerException e )
        {
            // guess there are no profiles here
        }
        return reportPlugins;
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

    private static int compare( ArtifactVersion a, ArtifactVersion b )
    {
        return a.compareTo( b );
    }

    private static class IgnoringModelProblemCollector implements ModelProblemCollector
    {

        @Override
        public void add( ModelProblemCollectorRequest req )
        {
            // ignore
        }
    }

}
