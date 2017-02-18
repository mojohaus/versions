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

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.lifecycle.Lifecycle;
import org.apache.maven.lifecycle.LifecycleExecutionException;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.PluginManager;
import org.apache.maven.plugin.PluginNotFoundException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.DefaultProjectBuilderConfiguration;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.interpolation.ModelInterpolationException;
import org.apache.maven.project.interpolation.ModelInterpolator;
import org.apache.maven.shared.artifact.resolve.ArtifactResolverException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.ReaderFactory;

/**
 * Displays all plugins that have newer versions available.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo( name = "display-plugin-updates", requiresProject = true, requiresDirectInvocation = false )
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
    @Component
    private LifecycleExecutor lifecycleExecutor;

    /**
     * @since 1.0-alpha-3
     */
    @Component
    private ModelInterpolator modelInterpolator;

    /**
     * The plugin manager.
     *
     * @since 1.0-alpha-1
     */
    @Component
    private PluginManager pluginManager;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Returns the pluginManagement section of the super-pom.
     *
     * @return Returns the pluginManagement section of the super-pom.
     * @throws MojoExecutionException when things go wrong.
     */
    private Map<String, String> getSuperPomPluginManagement()
        throws MojoExecutionException
    {
        Map<String, String> result = Collections.emptyMap();

        getLog().debug( "Using Maven 3.x strategy to determine superpom defined plugins" );
        try
        {
            Method getPluginsBoundByDefaultToAllLifecycles =
                LifecycleExecutor.class.getMethod( "getPluginsBoundByDefaultToAllLifecycles",
                                                   new Class[] { String.class } );
            Set<Plugin> plugins =
                (Set<Plugin>) getPluginsBoundByDefaultToAllLifecycles.invoke( lifecycleExecutor, new Object[] {
                    getProject().getPackaging() } );
            // we need to provide a copy with the version blanked out so that inferring from super-pom
            // works as for 2.x as 3.x fills in the version on us!
            result = new LinkedHashMap<String, String>( plugins.size() );
            for ( Plugin plugin : plugins )
            {
                result.put( getPluginCoords( plugin ), getPluginVersion( plugin ) );
            }
            URL superPom = getClass().getClassLoader().getResource( "org/apache/maven/model/pom-4.0.0.xml" );
            if ( superPom != null )
            {
                try
                {
                    readSuperPom( result, superPom );
                }
                catch ( IOException e )
                {
                    // ignore
                }
                catch ( XMLStreamException e )
                {
                    // ignore
                }
            }

            return result;
        }
        catch ( NoSuchMethodException e1 )
        {
            getLog().error( "NoSuchMethodException", e1 );
        }
        catch ( InvocationTargetException e1 )
        {
            getLog().error( "InvocationTargetException", e1 );
        }
        catch ( IllegalAccessException e1 )
        {
            // no much we can do here
            getLog().error( "IllegalAccessException", e1 );
        }

        return result;
    }

    private void readSuperPom( Map<String, String> result, URL superPom )
        throws IOException, XMLStreamException
    {
        Reader reader = ReaderFactory.newXmlReader( superPom );
        try
        {
            StringBuilder buf = new StringBuilder( IOUtil.toString( reader ) );
            ModifiedPomXMLEventReader pom = newModifiedPomXER( buf );

            Pattern pathRegex = Pattern.compile( "/project(/profiles/profile)?"
                + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin" );
            Stack<StackState> pathStack = new Stack<StackState>();
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
                            plugin.setGroupId( curState.groupId == null ? PomHelper.APACHE_MAVEN_PLUGINS_GROUPID
                                            : curState.groupId );
                            plugin.setVersion( curState.version );
                            if ( !result.containsKey( getPluginCoords( plugin ) ) )
                            {
                                result.put( getPluginCoords( plugin ), getPluginVersion( plugin ) );
                            }
                        }
                    }
                    curState = pathStack.pop();
                }
            }
        }
        finally
        {
            IOUtil.close( reader );
        }
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
        Map<String, String> pluginManagement = new HashMap<String, String>();
        try
        {
            for ( Plugin plugin : model.getBuild().getPluginManagement().getPlugins() )
            {
                String coord = getPluginCoords( plugin );
                String version = getPluginVersion( plugin );
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
                        String coord = getPluginCoords( plugin );
                        String version = getPluginVersion( plugin );
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
     * @throws MojoFailureException when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();
        Set<String> pluginsWithVersionsSpecified;
        try
        {
            pluginsWithVersionsSpecified = findPluginsWithVersionsSpecified( getProject() );
        }
        catch ( XMLStreamException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

        Map<String, String> superPomPluginManagement = getSuperPomPluginManagement();
        getLog().debug( "superPom plugins = " + superPomPluginManagement );

        Map<String, String> parentPluginManagement = new HashMap<String, String>();
        Map<String, String> parentBuildPlugins = new HashMap<String, String>();
        Map<String, String> parentReportPlugins = new HashMap<String, String>();

        List<MavenProject> parents = getParentProjects( getProject() );

        for ( MavenProject parentProject : parents )
        {
            getLog().debug( "Processing parent: " + parentProject.getGroupId() + ":" + parentProject.getArtifactId()
                + ":" + parentProject.getVersion() + " -> " + parentProject.getFile() );

            StringWriter writer = new StringWriter();
            boolean havePom = false;
            Model interpolatedModel;
            try
            {
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
                interpolatedModel =
                    modelInterpolator.interpolate( originalModel, null,
                                                   new DefaultProjectBuilderConfiguration().setExecutionProperties( getProject().getProperties() ),
                                                   false );
            }
            catch ( ModelInterpolationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
            if ( havePom )
            {
                try
                {
                    Set<String> withVersionSpecified =
                        findPluginsWithVersionsSpecified( new StringBuilder( writer.toString() ) );
                    Map<String, String> map = getPluginManagement( interpolatedModel );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPluginManagement.putAll( map );

                    map = getBuildPlugins( interpolatedModel, true );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPluginManagement.putAll( map );

                    map = getReportPlugins( interpolatedModel, true );
                    map.keySet().retainAll( withVersionSpecified );
                    parentPluginManagement.putAll( map );
                }
                catch ( IOException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
                catch ( XMLStreamException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
            else
            {
                parentPluginManagement.putAll( getPluginManagement( interpolatedModel ) );
                parentPluginManagement.putAll( getBuildPlugins( interpolatedModel, true ) );
                parentPluginManagement.putAll( getReportPlugins( interpolatedModel, true ) );
            }
        }

        Set<Plugin> plugins = getProjectPlugins( superPomPluginManagement, parentPluginManagement, parentBuildPlugins,
                                                 parentReportPlugins, pluginsWithVersionsSpecified );
        List<String> updates = new ArrayList<String>();
        List<String> lockdowns = new ArrayList<String>();
        Map<ArtifactVersion, Map<String, String>> upgrades =
            new TreeMap<ArtifactVersion, Map<String, String>>( new MavenVersionComparator() );
        ArtifactVersion curMavenVersion = new DefaultArtifactVersion( "3.0.0" );
        ArtifactVersion specMavenVersion = new DefaultArtifactVersion( getRequiredMavenVersion( getProject(), "2.0" ) );
        ArtifactVersion minMavenVersion = null;
        boolean superPomDrivingMinVersion = false;
        Iterator<Plugin> i = plugins.iterator();
        while ( i.hasNext() )
        {
            Object plugin = i.next();
            String groupId = getPluginGroupId( plugin );
            String artifactId = getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            String coords = ArtifactUtils.versionlessKey( groupId, artifactId );

            if ( version == null )
            {
                version = parentPluginManagement.get( coords );
            }
            getLog().debug( new StringBuilder().append( "Checking " ).append( coords ).append( " for updates newer than " ).append( version ).toString() );
            String effectiveVersion = version;

            VersionRange versionRange;
            boolean unspecified = version == null;
            try
            {
                versionRange = unspecified ? VersionRange.createFromVersionSpec( "[0,)" )
                                : VersionRange.createFromVersionSpec( version );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( "Invalid version range specification: " + version, e );
            }

            Artifact artifact = artifactFactory.createPluginArtifact( groupId, artifactId, versionRange );

            ArtifactVersion artifactVersion = null;
            try
            {
                // now we want to find the newest version that is compatible with the invoking version of Maven
                ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifact, true );
                ArtifactVersion[] newerVersions = artifactVersions.getVersions( this.allowSnapshots );
                ArtifactVersion minRequires = null;
                for ( int j = newerVersions.length - 1; j >= 0; j-- )
                {
                    Artifact probe =
                        artifactFactory.createDependencyArtifact( groupId, artifactId,
                                                                  VersionRange.createFromVersion( newerVersions[j].toString() ),
                                                                  "pom", null, "runtime" );
                    try
                    {
                        getHelper().resolveArtifact( probe, true );
                        MavenProject mavenProject =
                            projectBuilder.buildFromRepository( probe, remotePluginRepositories, localRepository );
                        ArtifactVersion requires =
                            new DefaultArtifactVersion( getRequiredMavenVersion( mavenProject, "2.0" ) );
                        if ( specMavenVersion.compareTo( requires ) >= 0 && artifactVersion == null )
                        {
                            artifactVersion = newerVersions[j];
                        }
                        if ( effectiveVersion == null && curMavenVersion.compareTo( requires ) >= 0 )
                        {
                            // version was unspecified, current version of maven thinks it should use this
                            effectiveVersion = newerVersions[j].toString();
                        }
                        if ( artifactVersion != null && effectiveVersion != null )
                        {
                            // no need to look at any older versions.
                            break;
                        }
                        if ( minRequires == null || minRequires.compareTo( requires ) > 0 )
                        {
                            Map<String, String> upgradePlugins = upgrades.get( requires );
                            if ( upgradePlugins == null )
                            {
                                upgrades.put( requires, upgradePlugins = new LinkedHashMap<String, String>() );
                            }
                            String upgradePluginKey = compactKey( groupId, artifactId );
                            if ( !upgradePlugins.containsKey( upgradePluginKey ) )
                            {
                                upgradePlugins.put( upgradePluginKey, newerVersions[j].toString() );
                            }
                            minRequires = requires;
                        }
                    }
                    catch ( ProjectBuildingException e )
                    {
                        // ignore bad version
                    }
                    catch ( ArtifactResolverException e )
                    {
                        e.printStackTrace();
                    }
                }
                if ( effectiveVersion != null )
                {
                    VersionRange currentVersionRange = VersionRange.createFromVersion( effectiveVersion );
                    Artifact probe = artifactFactory.createDependencyArtifact( groupId, artifactId, currentVersionRange,
                                                                               "pom", null, "runtime" );
                    try
                    {
                        getHelper().resolveArtifact( probe, true );
                        MavenProject mavenProject =
                            projectBuilder.buildFromRepository( probe, remotePluginRepositories, localRepository );
                        ArtifactVersion requires =
                            new DefaultArtifactVersion( getRequiredMavenVersion( mavenProject, "2.0" ) );
                        if ( minMavenVersion == null || minMavenVersion.compareTo( requires ) < 0 )
                        {
                            minMavenVersion = requires;
                        }
                    }
                    catch ( ProjectBuildingException e )
                    {
                        // ignore bad version
                    }
                    catch ( ArtifactResolverException e )
                    {
                        e.printStackTrace();
                    }
                }
            }
            catch ( ArtifactMetadataRetrievalException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }

            String newVersion;

            if ( version == null && pluginsWithVersionsSpecified.contains( coords ) )
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
            getLog().debug( "[" + coords + "].specified=" + pluginsWithVersionsSpecified.contains( coords ) );
            if ( version == null || !pluginsWithVersionsSpecified.contains( coords ) )
            {
                version = (String) superPomPluginManagement.get( ArtifactUtils.versionlessKey( artifact ) );
                getLog().debug( "[" + coords + "].superPom.version=" + version );

                newVersion = artifactVersion != null ? artifactVersion.toString()
                                : ( version != null ? version
                                                : ( effectiveVersion != null ? effectiveVersion : "(unknown)" ) );
                StringBuilder buf = new StringBuilder( compactKey( groupId, artifactId ) );
                buf.append( ' ' );
                int padding = WARN_PAD_SIZE - newVersion.length() - ( version != null ? FROM_SUPER_POM.length() : 0 );
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                if ( version != null )
                {
                    buf.append( FROM_SUPER_POM );
                    superPomDrivingMinVersion = true;
                }
                buf.append( newVersion );
                lockdowns.add( buf.toString() );
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
                && new DefaultArtifactVersion( effectiveVersion ).compareTo( new DefaultArtifactVersion( newVersion ) ) < 0 )
            {
                StringBuilder buf = new StringBuilder( compactKey( groupId, artifactId ) );
                buf.append( ' ' );
                int padding = INFO_PAD_SIZE - version.length() - newVersion.length() - 4;
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( effectiveVersion );
                buf.append( " -> " );
                buf.append( newVersion );
                updates.add( buf.toString() );
            }
        }
        logLine( false, "" );
        if ( updates.isEmpty() )
        {
            logLine( false, "All plugins with a version specified are using the latest versions." );
        }
        else
        {
            logLine( false, "The following plugin updates are available:" );
            for ( String update : updates )
            {
                logLine( false, "  " + update );
            }
        }
        logLine( false, "" );
        if ( lockdowns.isEmpty() )
        {
            logLine( false, "All plugins have a version specified." );
        }
        else
        {
            getLog().warn( "The following plugins do not have their version specified:" );
            for ( String lockdown : lockdowns )
            {
                getLog().warn( "  " + lockdown );
            }
        }
        logLine( false, "" );
        boolean noMavenMinVersion = getRequiredMavenVersion( getProject(), null ) == null;
        boolean noExplicitMavenMinVersion =
            getProject().getPrerequisites() == null || getProject().getPrerequisites().getMaven() == null;
        if ( noMavenMinVersion )
        {
            getLog().warn( "Project does not define minimum Maven version, default is: 2.0" );
        }
        else if ( noExplicitMavenMinVersion )
        {
            logLine( false, "Project inherits minimum Maven version as: " + specMavenVersion );
        }
        else
        {
            ArtifactVersion explicitMavenVersion =
                new DefaultArtifactVersion( getProject().getPrerequisites().getMaven() );
            if ( explicitMavenVersion.compareTo( specMavenVersion ) < 0 )
            {
                logLine( true, "Project's effective minimum Maven (from parent) is: " + specMavenVersion );
                logLine( true, "Project defines minimum Maven version as: " + explicitMavenVersion );
            }
            else
            {
                logLine( false, "Project defines minimum Maven version as: " + specMavenVersion );
            }
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
                getLog().warn( "Project (which is a Maven Plugin) does not define required minimum version of Maven." );
                getLog().warn( "Update the pom.xml to contain" );
                getLog().warn( "    <prerequisites>" );
                getLog().warn( "      <maven><!-- minimum version of Maven that the plugin works with --></maven>" );
                getLog().warn( "    </prerequisites>" );
                getLog().warn( "To build this plugin you need at least Maven " + minMavenVersion );
                getLog().warn( "A Maven Enforcer rule can be used to enforce this if you have not already set one up" );
                getLog().warn( "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
            }
            else if ( minMavenVersion != null && specMavenVersion.compareTo( minMavenVersion ) < 0 )
            {
                getLog().warn( "Project (which is a Maven Plugin) targets Maven " + specMavenVersion + " or newer" );
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
                logLine( true, "force the maven version which is needed to build this project." );
                logLine( true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
                logLine( true, "Using the minimum version of Maven: " + minMavenVersion );
            }
            else if ( minMavenVersion != null && specMavenVersion.compareTo( minMavenVersion ) < 0 )
            {
                logLine( true, "Project requires an incorrect minimum version of Maven." );
                logLine( true, "Update the pom.xml to contain maven-enforcer-plugin to" );
                logLine( true, "force the maven version which is needed to build this project." );
                logLine( true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html" );
                logLine( true, "Using the minimum version of Maven: " + specMavenVersion );
            }
            else
            {
                logLine( false, "No plugins require a newer version of Maven than specified by the pom." );
            }
        }
        for ( Map.Entry<ArtifactVersion, Map<String, String>> mavenUpgrade : upgrades.entrySet() )
        {
            ArtifactVersion mavenUpgradeVersion = (ArtifactVersion) mavenUpgrade.getKey();
            Map<String, String> upgradePlugins = mavenUpgrade.getValue();
            if ( upgradePlugins.isEmpty() || specMavenVersion.compareTo( mavenUpgradeVersion ) >= 0 )
            {
                continue;
            }
            logLine( false, "" );
            logLine( false, "Require Maven " + mavenUpgradeVersion + " to use the following plugin updates:" );
            for ( Map.Entry<String, String> entry : upgradePlugins.entrySet() )
            {
                StringBuilder buf = new StringBuilder( "  " );
                buf.append( entry.getKey() );
                buf.append( ' ' );
                String s = entry.getValue();
                int padding = INFO_PAD_SIZE - s.length() + 2;
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( s );
                logLine( false, buf.toString() );
            }
        }
        logLine( false, "" );
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

    private String getRequiredMavenVersion( MavenProject mavenProject, String defaultValue )
    {
        ArtifactVersion requiredMavenVersion = null;
        while ( mavenProject != null )
        {
            final Prerequisites prerequisites = mavenProject.getPrerequisites();
            final String mavenVersion = prerequisites == null ? null : prerequisites.getMaven();
            if ( mavenVersion != null )
            {
                final ArtifactVersion v = new DefaultArtifactVersion( mavenVersion );
                if ( requiredMavenVersion == null || requiredMavenVersion.compareTo( v ) < 0 )
                {
                    requiredMavenVersion = v;
                }
            }
            mavenProject = mavenProject.getParent();
        }
        return requiredMavenVersion == null ? defaultValue : requiredMavenVersion.toString();
    }

    private static final class StackState
    {
        private final String path;

        private String groupId;

        private String artifactId;

        private String version;

        public StackState( String path )
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
        return findPluginsWithVersionsSpecified( PomHelper.readXmlFile( project.getFile() ) );
    }

    /**
     * Returns a set of Strings which correspond to the plugin coordinates where there is a version specified.
     *
     * @param pomContents The project to get the plugins with versions specified.
     * @return a set of Strings which correspond to the plugin coordinates where there is a version specified.
     */
    private Set<String> findPluginsWithVersionsSpecified( StringBuilder pomContents )
        throws IOException, XMLStreamException
    {
        Set<String> result = new HashSet<String>();
        ModifiedPomXMLEventReader pom = newModifiedPomXER( pomContents );

        Pattern pathRegex = Pattern.compile( "/project(/profiles/profile)?"
            + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin" );
        Stack<StackState> pathStack = new Stack<StackState>();
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
     * Gets the build plugins of a specific project.
     *
     * @param model the model to get the build plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *            child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getBuildPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map<String, String> buildPlugins = new HashMap<String, String>();
        try
        {
            for ( Plugin plugin : model.getBuild().getPlugins() )
            {
                String coord = getPluginCoords( plugin );
                String version = getPluginVersion( plugin );
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
                        String coord = getPluginCoords( plugin );
                        String version = getPluginVersion( plugin );
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
        Map<String, Plugin> lifecyclePlugins = new HashMap<String, Plugin>();
        try
        {
            Set<Plugin> plugins = getBoundPlugins( project, "clean,deploy,site" );
            for ( Plugin plugin : plugins )
            {
                lifecyclePlugins.put( getPluginCoords( plugin ), plugin );
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
     * @param project the project
     * @param thePhases the the phases
     * @return the bound plugins
     * @throws org.apache.maven.plugin.PluginNotFoundException the plugin not found exception
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws IllegalAccessException the illegal access exception
     */
    // pilfered this from enforcer-rules
    // TODO coordinate with Brian Fox to remove the duplicate code
    private Set<Plugin> getBoundPlugins( MavenProject project, String thePhases )
        throws PluginNotFoundException, LifecycleExecutionException, IllegalAccessException
    {
        Set<Plugin> result = new LinkedHashSet<Plugin>();
        getLog().debug( "Using Maven 3.0+ strategy to determine lifecycle defined plugins" );
        try
        {
            Method getPluginsBoundByDefaultToAllLifecycles =
                LifecycleExecutor.class.getMethod( "getPluginsBoundByDefaultToAllLifecycles",
                                                   new Class[] { String.class } );
            Set<Plugin> plugins =
                (Set<Plugin>) getPluginsBoundByDefaultToAllLifecycles.invoke( lifecycleExecutor, new Object[] {
                    project.getPackaging() == null ? "jar" : project.getPackaging() } );
            // we need to provide a copy with the version blanked out so that inferring from super-pom
            // works as for 2.x as 3.x fills in the version on us!
            result = new LinkedHashSet<Plugin>( plugins.size() );
            for ( Plugin plugin : plugins )
            {
                Plugin dup = new Plugin();
                dup.setGroupId( plugin.getGroupId() );
                dup.setArtifactId( plugin.getArtifactId() );
                result.add( dup );
            }
            return result;
        }
        catch ( NoSuchMethodException e1 )
        {
            getLog().error( "NoSuchMethodException", e1 );
        }
        catch ( InvocationTargetException e1 )
        {
            // no much we can do here
        }
        catch ( IllegalAccessException e1 )
        {
            // no much we can do here
        }
        return result;
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
        List<MavenProject> parents = new ArrayList<MavenProject>();
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
    public Map getPhaseToLifecycleMap( List lifecycles )
        throws LifecycleExecutionException
    {
        Map phaseToLifecycleMap = new HashMap();

        for ( Iterator i = lifecycles.iterator(); i.hasNext(); )
        {
            Lifecycle lifecycle = (Lifecycle) i.next();

            for ( Iterator p = lifecycle.getPhases().iterator(); p.hasNext(); )
            {
                String phase = (String) p.next();

                if ( phaseToLifecycleMap.containsKey( phase ) )
                {
                    Lifecycle prevLifecycle = (Lifecycle) phaseToLifecycleMap.get( phase );
                    throw new LifecycleExecutionException( "Phase '" + phase
                        + "' is defined in more than one lifecycle: '" + lifecycle.getId() + "' and '"
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
     * @param superPomPluginManagement the super pom's pluginManagement plugins.
     * @param parentPluginManagement the parent pom's pluginManagement plugins.
     * @param parentBuildPlugins the parent pom's build plugins.
     * @param parentReportPlugins the parent pom's report plugins.
     * @param pluginsWithVersionsSpecified the plugin coords that have a version defined in the project.
     * @return the set of plugins used by the project.
     * @throws org.apache.maven.plugin.MojoExecutionException if things go wrong.
     */
    private Set<Plugin> getProjectPlugins( Map<String, String> superPomPluginManagement,
                                           Map<String, String> parentPluginManagement,
                                           Map<String, String> parentBuildPlugins,
                                           Map<String, String> parentReportPlugins,
                                           Set<String> pluginsWithVersionsSpecified )
        throws MojoExecutionException
    {
        Map<String, Plugin> plugins = new HashMap<String, Plugin>();

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

        Map<String, String> excludePluginManagement = new HashMap<String, String>( superPomPluginManagement );
        excludePluginManagement.putAll( parentPluginManagement );

        debugVersionMap( "aggregate version map", excludePluginManagement );

        excludePluginManagement.keySet().removeAll( pluginsWithVersionsSpecified );

        debugVersionMap( "final aggregate version map", excludePluginManagement );

        Model originalModel;
        try
        {
            originalModel =
                modelInterpolator.interpolate( getProject().getOriginalModel(), getProject().getBasedir(),
                                               new DefaultProjectBuilderConfiguration().setExecutionProperties( getProject().getProperties() ),
                                               true );
        }
        catch ( ModelInterpolationException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
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
            List<Plugin> lifecyclePlugins = new ArrayList<Plugin>( getLifecyclePlugins( getProject() ).values() );
            for ( Iterator<Plugin> i = lifecyclePlugins.iterator(); i.hasNext(); )
            {
                Plugin lifecyclePlugin = i.next();
                if ( getPluginVersion( lifecyclePlugin ) != null )
                {
                    // version comes from lifecycle, therefore cannot modify
                    i.remove();
                }
                else
                {
                    // lifecycle leaves version open
                    String parentVersion = parentPluginManagement.get( getPluginCoords( lifecyclePlugin ) );
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
            List<Plugin> buildPlugins = new ArrayList<Plugin>( originalModel.getBuild().getPlugins() );
            for ( Iterator<Plugin> i = buildPlugins.iterator(); i.hasNext(); )
            {
                Plugin buildPlugin = i.next();
                if ( getPluginVersion( buildPlugin ) == null )
                {
                    String parentVersion = parentPluginManagement.get( getPluginCoords( buildPlugin ) );
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
            List<ReportPlugin> reportPlugins = new ArrayList<ReportPlugin>( originalModel.getReporting().getPlugins() );
            for ( Iterator<ReportPlugin> i = reportPlugins.iterator(); i.hasNext(); )
            {
                ReportPlugin reportPlugin = i.next();
                if ( getPluginVersion( reportPlugin ) == null )
                {
                    String parentVersion = parentPluginManagement.get( getPluginCoords( reportPlugin ) );
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
        Set<Plugin> result = new TreeSet<Plugin>( new PluginComparator() );
        result.addAll( plugins.values() );
        return result;
    }

    /**
     * Adds those project plugins which are not inherited from the parent definitions to the list of plugins.
     *
     * @param plugins The list of plugins.
     * @param projectPlugins The project's plugins.
     * @param parentDefinitions The parent plugin definitions.
     * @since 1.0-alpha-1
     */
    private void addProjectPlugins( Map<String, Plugin> plugins, Collection<Plugin> projectPlugins,
                                    Map<String, String> parentDefinitions )
    {
        for ( Plugin plugin : projectPlugins )
        {
            String coord = getPluginCoords( plugin );
            String version = getPluginVersion( plugin );
            String parentVersion = parentDefinitions.get( coord );
            if ( version == null
                && ( !plugins.containsKey( coord ) || getPluginVersion( plugins.get( coord ) ) == null )
                && parentVersion != null )
            {
                Plugin parentPlugin = new Plugin();
                parentPlugin.setGroupId( getPluginGroupId( plugin ) );
                parentPlugin.setArtifactId( getPluginArtifactId( plugin ) );
                parentPlugin.setVersion( parentVersion );
                plugins.put( coord, parentPlugin );
            }
            else if ( parentVersion == null || !parentVersion.equals( version ) )
            {
                if ( !plugins.containsKey( coord ) || getPluginVersion( plugins.get( coord ) ) == null )
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
     * @param plugins a map with keys being the {@link String} corresponding to the versionless artifact key and values
     *            being {@link Plugin} or {@link ReportPlugin}.
     */
    private void debugPluginMap( String description, Map<String, Plugin> plugins )
    {
        if ( getLog().isDebugEnabled() )
        {
            Set<Plugin> sorted = new TreeSet<Plugin>( new PluginComparator() );
            sorted.addAll( plugins.values() );
            StringBuilder buf = new StringBuilder( description );
            for ( Plugin plugin : sorted )
            {
                buf.append( "\n    " );
                buf.append( getPluginCoords( plugin ) );
                buf.append( ":" );
                buf.append( getPluginVersion( plugin ) );
            }
            getLog().debug( buf.toString() );
        }
    }

    /**
     * Logs at debug level a map of plugin versions keyed by versionless key.
     *
     * @param description log description
     * @param plugins a map with keys being the {@link String} corresponding to the versionless artifact key and values
     *            being {@link String} plugin version.
     */
    private void debugVersionMap( String description, Map<String, String> plugins )
    {
        if ( getLog().isDebugEnabled() )
        {
            StringBuilder buf = new StringBuilder( description );
            for ( Map.Entry<String, String> plugin : plugins.entrySet() )
            {
                buf.append( "\n    " );
                buf.append( plugin.getKey() );
                buf.append( ":" );
                buf.append( plugin.getValue() );
            }
            getLog().debug( buf.toString() );
        }
    }

    /**
     * Returns the coordinates of a plugin.
     *
     * @param plugin The plugin
     * @return The groupId and artifactId separated by a colon.
     * @since 1.0-alpha-1
     */
    private static String getPluginCoords( Object plugin )
    {
        return getPluginGroupId( plugin ) + ":" + getPluginArtifactId( plugin );
    }

    /**
     * Returns the ArtifactId of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the ArtifactId of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0-alpha-1
     */
    private static String getPluginArtifactId( Object plugin )
    {
        return plugin instanceof ReportPlugin ? ( (ReportPlugin) plugin ).getArtifactId()
                        : ( (Plugin) plugin ).getArtifactId();
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
        List<Plugin> result = new ArrayList<Plugin>( reportPlugins.size() );
        for ( ReportPlugin reportPlugin : reportPlugins )
        {
            result.add( toPlugin( reportPlugin ) );
        }
        return result;
    }

    /**
     * Returns the GroupId of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the GroupId of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0-alpha-1
     */
    private static String getPluginGroupId( Object plugin )
    {
        return plugin instanceof ReportPlugin ? ( (ReportPlugin) plugin ).getGroupId()
                        : ( (Plugin) plugin ).getGroupId();
    }

    /**
     * Returns the Version of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Version of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0-alpha-1
     */
    private static String getPluginVersion( Object plugin )
    {
        return plugin instanceof ReportPlugin ? ( (ReportPlugin) plugin ).getVersion()
                        : ( (Plugin) plugin ).getVersion();
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param model the model to get the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *            child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getReportPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map<String, String> reportPlugins = new HashMap<String, String>();
        try
        {
            for ( ReportPlugin plugin : model.getReporting().getPlugins() )
            {
                String coord = getPluginCoords( plugin );
                String version = getPluginVersion( plugin );
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
                        String coord = getPluginCoords( plugin );
                        String version = getPluginVersion( plugin );
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
     * @throws MojoFailureException when things go wrong in a very bad way
     * @throws XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

}
