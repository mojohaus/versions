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

import org.apache.maven.BuildFailureException;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.lifecycle.Lifecycle;
import org.apache.maven.lifecycle.LifecycleExecutionException;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.lifecycle.mapping.LifecycleMapping;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.plugin.InvalidPluginException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.PluginManager;
import org.apache.maven.plugin.PluginManagerException;
import org.apache.maven.plugin.PluginNotFoundException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.version.PluginVersionNotFoundException;
import org.apache.maven.plugin.version.PluginVersionResolutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.interpolation.ModelInterpolationException;
import org.apache.maven.project.interpolation.ModelInterpolator;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.component.repository.exception.ComponentLookupException;
import org.codehaus.plexus.util.ReflectionUtils;
import org.codehaus.plexus.util.StringUtils;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.regex.Pattern;

/**
 * Displays all plugins that have newer versions available.
 *
 * @author Stephen Connolly
 * @goal display-plugin-updates
 * @requiresProject true
 * @requiresDirectInvocation false
 * @since 1.0-alpha-1
 */
public class DisplayPluginUpdatesMojo
    extends AbstractVersionsUpdaterMojo
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
     * @component
     * @since 1.0-alpha-1
     */
    private LifecycleExecutor lifecycleExecutor;

    /**
     * @component
     * @since 1.0-alpha-3
     */
    private ModelInterpolator modelInterpolator;

    /**
     * The session.
     *
     * @readonly
     * @parameter expression="${session}"
     * @since 1.0-alpha-1
     */
    private MavenSession session;

    /**
     * The plugin manager.
     *
     * @component
     * @since 1.0-alpha-1
     */
    private PluginManager pluginManager;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Returns the pluginManagement section of the super-pom.
     *
     * @return Returns the pluginManagement section of the super-pom.
     * @throws MojoExecutionException when things go wrong.
     */
    private Map getSuperPomPluginManagement()
        throws MojoExecutionException
    {
        Map superPomPluginManagement = new HashMap();
        try
        {
            MavenProject superProject = projectBuilder.buildStandaloneSuperProject( localRepository );
            superPomPluginManagement.putAll( getPluginManagement( superProject.getOriginalModel() ) );
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
    private Map getPluginManagement( Model model )
    {
        // we want only those parts of pluginManagement that are defined in this project
        Map pluginManagement = new HashMap();
        try
        {
            Iterator j = model.getBuild().getPluginManagement().getPlugins().iterator();
            while ( j.hasNext() )
            {
                Plugin plugin = (Plugin) j.next();
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
            Iterator i = model.getProfiles().iterator();
            while ( i.hasNext() )
            {
                Profile profile = (Profile) i.next();
                try
                {
                    Iterator j = profile.getBuild().getPluginManagement().getPlugins().iterator();
                    while ( j.hasNext() )
                    {
                        Plugin plugin = (Plugin) j.next();
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
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        Set pluginsWithVersionsSpecified;
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

        Map superPomPluginManagement = getSuperPomPluginManagement();

        Map parentPluginManagement = new HashMap();
        Map parentBuildPlugins = new HashMap();
        Map parentReportPlugins = new HashMap();

        List parents = getParentProjects( getProject() );

        Iterator i = parents.iterator();
        while ( i.hasNext() )
        {
            MavenProject parentProject = (MavenProject) i.next();

            Model originalModel;
            try
            {
                originalModel =
                    modelInterpolator.interpolate( parentProject.getOriginalModel(), getProject().getProperties() );
            }
            catch ( ModelInterpolationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
            parentPluginManagement.putAll( getPluginManagement( originalModel ) );
            parentBuildPlugins.putAll( getBuildPlugins( originalModel, true ) );
            parentReportPlugins.putAll( getReportPlugins( originalModel, true ) );
        }

        Set plugins = getProjectPlugins( superPomPluginManagement, parentPluginManagement, parentBuildPlugins,
                                         parentReportPlugins, pluginsWithVersionsSpecified );
        List updates = new ArrayList();
        List lockdown = new ArrayList();
        i = plugins.iterator();
        while ( i.hasNext() )
        {
            Object plugin = i.next();
            String groupId = getPluginGroupId( plugin );
            String artifactId = getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            String coords = ArtifactUtils.versionlessKey( groupId, artifactId );

            if ( version == null )
            {
                version = (String) parentPluginManagement.get( coords );
            }
            getLog().debug(
                new StringBuffer().append( "Checking " ).append( coords ).append( " for updates newer than " ).append(
                    version ).toString() );

            VersionRange versionRange;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( version == null ? "LATEST" : version );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( "Invalid version range specification: " + version, e );
            }

            Artifact artifact = artifactFactory.createPluginArtifact( groupId, artifactId, versionRange );

            ArtifactVersion artifactVersion;
            try
            {
                artifactVersion = findLatestVersion( artifact, versionRange, null, true );
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

            getLog().debug( "" + version );
            getLog().debug( "" + artifactVersion );
            getLog().debug( "" + pluginsWithVersionsSpecified.contains( coords ) );
            if ( version == null && !pluginsWithVersionsSpecified.contains( coords ) )
            {
                version = (String) superPomPluginManagement.get( ArtifactUtils.versionlessKey( artifact ) );
                ;
                newVersion = version != null ? version : artifactVersion.toString();
                newVersion = artifactVersion != null ? artifactVersion.toString() : version;
                StringBuffer buf = new StringBuffer();
                if ( PomHelper.APACHE_MAVEN_PLUGINS_GROUPID.equals( groupId ) )
                {
                    // a core plugin... group id is not needed
                }
                else
                {
                    buf.append( groupId ).append( ':' );
                }
                buf.append( artifactId );
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
                }
                buf.append( newVersion );
                lockdown.add( buf.toString() );
            }
            else if ( artifactVersion != null )
            {
                newVersion = artifactVersion.toString();
            }
            else
            {
                newVersion = null;
            }
            if ( version != null && artifactVersion != null && newVersion != null
                && new DefaultArtifactVersion( version ).compareTo( new DefaultArtifactVersion( newVersion ) ) < 0 )
            {
                StringBuffer buf = new StringBuffer();
                if ( PomHelper.APACHE_MAVEN_PLUGINS_GROUPID.equals( groupId ) )
                {
                    // a core plugin... group id is not needed
                }
                else
                {
                    buf.append( groupId ).append( ':' );
                }
                buf.append( artifactId );
                buf.append( ' ' );
                int padding = INFO_PAD_SIZE - version.length() - newVersion.length() - 4;
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( version );
                buf.append( " -> " );
                buf.append( newVersion );
                updates.add( buf.toString() );
            }
        }
        getLog().info( "" );
        if ( updates.isEmpty() )
        {
            getLog().info( "All plugins with a version specified are using the latest versions." );
        }
        else
        {
            getLog().info( "The following plugin updates are available:" );
            i = updates.iterator();
            while ( i.hasNext() )
            {
                getLog().info( "  " + i.next() );
            }
        }
        getLog().info( "" );
        if ( lockdown.isEmpty() )
        {
            getLog().info( "All plugins have a version specified." );
        }
        else
        {
            getLog().warn( "The following plugins do not have their version specified:" );
            i = lockdown.iterator();
            while ( i.hasNext() )
            {
                getLog().warn( "  " + i.next() );
            }
        }
        getLog().info( "" );
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
     * Returns a set of Strings which correspond to the plugin coordinates where there is a version
     * specified.
     *
     * @param project The project to get the plugins with versions specified.
     * @return a set of Strings which correspond to the plugin coordinates where there is a version
     *         specified.
     */
    private Set findPluginsWithVersionsSpecified( MavenProject project )
        throws IOException, XMLStreamException
    {
        Set result = new HashSet();
        ModifiedPomXMLEventReader pom = newModifiedPomXER( PomHelper.readFile( project.getFile() ) );

        Pattern pathRegex = Pattern.compile(
            "/project(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin" );
        Stack pathStack = new Stack();
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
                curState = (StackState) pathStack.pop();
            }
        }

        return result;

    }

// -------------------------- OTHER METHODS --------------------------

    /**
     * Gets the build plugins of a specific project.
     *
     * @param model                the model to get the build plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be
     *                             inherited by child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map getBuildPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map buildPlugins = new HashMap();
        try
        {
            Iterator j = model.getBuild().getPlugins().iterator();
            while ( j.hasNext() )
            {
                Object plugin = j.next();
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
            Iterator i = model.getProfiles().iterator();
            while ( i.hasNext() )
            {
                Profile profile = (Profile) i.next();
                try
                {
                    Iterator j = profile.getBuild().getPlugins().iterator();
                    while ( j.hasNext() )
                    {
                        Object plugin = j.next();
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
        return "true".equalsIgnoreCase( plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getInherited()
            : ( (Plugin) plugin ).getInherited() );
    }

    /**
     * Returns the lifecycle plugins of a specific project.
     *
     * @param project the project to get the lifecycle plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          if things go wrong.
     * @since 1.0-alpha-1
     */
    private Map getLifecyclePlugins( MavenProject project )
        throws MojoExecutionException
    {
        Map lifecyclePlugins = new HashMap();
        try
        {
            Set plugins = getBoundPlugins( project, "clean,deploy,site" );
            Iterator i = plugins.iterator();
            while ( i.hasNext() )
            {
                Plugin plugin = (Plugin) i.next();
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
        return lifecyclePlugins;
    }

    /**
     * Gets the plugins that are bound to the defined phases. This does not find plugins bound in the pom to a phase
     * later than the plugin is executing.
     *
     * @param project   the project
     * @param thePhases the the phases
     * @return the bound plugins
     * @throws org.apache.maven.plugin.PluginNotFoundException
     *                                     the plugin not found exception
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws IllegalAccessException      the illegal access exception
     */
    // pilfered this from enforcer-rules
    // TODO coordinate with Brian Fox to remove the duplicate code
    private Set getBoundPlugins( MavenProject project, String thePhases )
        throws PluginNotFoundException, LifecycleExecutionException, IllegalAccessException
    {
        // I couldn't find a direct way to get at the lifecycles list.
        List lifecycles = (List) ReflectionUtils.getValueIncludingSuperclasses( "lifecycles", lifecycleExecutor );

        Set allPlugins = new HashSet();

        // lookup the bindings for all the passed in phases
        String[] lifecyclePhases = thePhases.split( "," );
        for ( int i = 0; i < lifecyclePhases.length; i++ )
        {
            String lifecyclePhase = lifecyclePhases[i];
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
    private Lifecycle getLifecycleForPhase( List lifecycles, String phase )
        throws BuildFailureException, LifecycleExecutionException
    {
        Lifecycle lifecycle = (Lifecycle) getPhaseToLifecycleMap( lifecycles ).get( phase );

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
    private Set getAllPlugins( MavenProject project, Lifecycle lifecycle )
        throws PluginNotFoundException, LifecycleExecutionException

    {
        HashSet plugins = new HashSet();
        // first, bind those associated with the packaging
        Map mappings = findMappingsForLifecycle( project, lifecycle );

        Iterator iter = mappings.entrySet().iterator();
        while ( iter.hasNext() )
        {
            Map.Entry entry = (Map.Entry) iter.next();
            String value = (String) entry.getValue();
            String[] tokens = value.split( ":" );

            Plugin plugin = new Plugin();
            plugin.setGroupId( tokens[0] );
            plugin.setArtifactId( tokens[1] );
            plugins.add( plugin );
        }

        List mojos = findOptionalMojosForLifecycle( project, lifecycle );
        iter = mojos.iterator();
        while ( iter.hasNext() )
        {
            String value = (String) iter.next();
            String[] tokens = value.split( ":" );

            Plugin plugin = new Plugin();
            plugin.setGroupId( tokens[0] );
            plugin.setArtifactId( tokens[1] );
            plugins.add( plugin );
        }

        for ( Iterator i = project.getBuildPlugins().iterator(); i.hasNext(); )
        {
            plugins.add( i.next() );
        }

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
    private Map findMappingsForLifecycle( MavenProject project, Lifecycle lifecycle )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        String packaging = project.getPackaging();
        Map mappings = null;

        LifecycleMapping m =
            (LifecycleMapping) findExtension( project, LifecycleMapping.ROLE, packaging, session.getSettings(),
                                              session.getLocalRepository() );
        if ( m != null )
        {
            mappings = m.getPhases( lifecycle.getId() );
        }

        Map defaultMappings = lifecycle.getDefaultPhases();

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
                    throw new LifecycleExecutionException(
                        "Cannot find lifecycle mapping for packaging: \'" + packaging + "\'.", e );
                }
            }
        }

        if ( mappings == null )
        {
            if ( defaultMappings == null )
            {
                throw new LifecycleExecutionException(
                    "Cannot find lifecycle mapping for packaging: \'" + packaging + "\', and there is no default" );
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
    private List findOptionalMojosForLifecycle( MavenProject project, Lifecycle lifecycle )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        String packaging = project.getPackaging();
        List optionalMojos = null;

        LifecycleMapping m =
            (LifecycleMapping) findExtension( project, LifecycleMapping.ROLE, packaging, session.getSettings(),
                                              session.getLocalRepository() );

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
                getLog().debug(
                    "Error looking up lifecycle mapping to retrieve optional mojos. Lifecycle ID: " + lifecycle.getId()
                        + ". Error: " + e.getMessage(), e );
            }
        }

        if ( optionalMojos == null )
        {
            optionalMojos = Collections.EMPTY_LIST;
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

        for ( Iterator i = project.getBuildPlugins().iterator(); i.hasNext() && pluginComponent == null; )
        {
            Plugin plugin = (Plugin) i.next();

            if ( plugin.isExtensions() )
            {
                verifyPlugin( plugin, project, settings, localRepository );

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
                    throw new LifecycleExecutionException(
                        "Error getting extensions from the plugin '" + plugin.getKey() + "': " + e.getMessage(), e );
                }
            }
        }
        return pluginComponent;
    }

    /**
     * Verify plugin.
     *
     * @param plugin          the plugin
     * @param project         the project
     * @param settings        the settings
     * @param localRepository the local repository
     * @return the plugin descriptor
     * @throws LifecycleExecutionException the lifecycle execution exception
     * @throws PluginNotFoundException     the plugin not found exception
     */
    private PluginDescriptor verifyPlugin( Plugin plugin, MavenProject project, Settings settings,
                                           ArtifactRepository localRepository )
        throws LifecycleExecutionException, PluginNotFoundException
    {
        PluginDescriptor pluginDescriptor;
        try
        {
            pluginDescriptor = pluginManager.verifyPlugin( plugin, project, settings, localRepository );
        }
        catch ( PluginManagerException e )
        {
            throw new LifecycleExecutionException(
                "Internal error in the plugin manager getting plugin '" + plugin.getKey() + "': " + e.getMessage(), e );
        }
        catch ( PluginVersionResolutionException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        catch ( InvalidPluginException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        catch ( ArtifactNotFoundException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        catch ( ArtifactResolutionException e )
        {
            throw new LifecycleExecutionException( e.getMessage(), e );
        }
        catch ( PluginVersionNotFoundException e )
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
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          if the super-pom could not be created.
     * @since 1.0-alpha-1
     */
    private List getParentProjects( MavenProject project )
        throws MojoExecutionException
    {
        List parents = new ArrayList();
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
                    throw new LifecycleExecutionException(
                        "Phase '" + phase + "' is defined in more than one lifecycle: '" + lifecycle.getId() + "' and '"
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
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          if things go wrong.
     */
    private Set getProjectPlugins( Map superPomPluginManagement, Map parentPluginManagement, Map parentBuildPlugins,
                                   Map parentReportPlugins, Set pluginsWithVersionsSpecified )
        throws MojoExecutionException
    {
        Map plugins = new HashMap();

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

        Map excludePluginManagement = new HashMap( superPomPluginManagement );
        excludePluginManagement.putAll( parentPluginManagement );

        debugVersionMap( "aggregate version map", excludePluginManagement );

        excludePluginManagement.keySet().removeAll( pluginsWithVersionsSpecified );

        debugVersionMap( "final aggregate version map", excludePluginManagement );

        Model originalModel;
        try
        {
            originalModel =
                modelInterpolator.interpolate( getProject().getOriginalModel(), getProject().getProperties() );
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

        addProjectPlugins( plugins, getLifecyclePlugins( getProject() ).values(), parentPluginManagement );

        debugPluginMap( "after adding lifecycle plugins", plugins );

        try
        {
            addProjectPlugins( plugins, originalModel.getBuild().getPlugins(), parentBuildPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        debugPluginMap( "after adding build plugins", plugins );

        try
        {
            addProjectPlugins( plugins, originalModel.getReporting().getPlugins(), parentReportPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        debugPluginMap( "after adding reporting plugins", plugins );

        Iterator i = originalModel.getProfiles().iterator();
        while ( i.hasNext() )
        {
            Profile profile = (Profile) i.next();
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
                addProjectPlugins( plugins, profile.getReporting().getPlugins(), parentReportPlugins );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }
            debugPluginMap( "after adding reporting plugins for profile " + profile.getId(), plugins );
        }
        Set result = new TreeSet( new PluginComparator() );
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
    private void addProjectPlugins( Map plugins, Collection projectPlugins, Map parentDefinitions )
    {
        Iterator j = projectPlugins.iterator();
        while ( j.hasNext() )
        {
            Object plugin = j.next();
            String coord = getPluginCoords( plugin );
            String version = getPluginVersion( plugin );
            String parentVersion = (String) parentDefinitions.get( coord );
            if ( parentVersion == null || !parentVersion.equals( version ) )
            {
                if ( !plugins.containsKey( coord ) || getPluginVersion( plugins.get( coord ) ) == null )
                {
                    plugins.put( coord, plugin );
                }
            }
        }
    }

    /**
     * Logs at debug level a map of plugins keyed by versionless key.
     *
     * @param description log description
     * @param plugins     a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                    values being {@link Plugin} or {@link ReportPlugin}.
     */
    private void debugPluginMap( String description, Map plugins )
    {
        if ( getLog().isDebugEnabled() )
        {
            Set sorted = new TreeSet( new PluginComparator() );
            sorted.addAll( plugins.values() );
            StringBuffer buf = new StringBuffer( description );
            Iterator i = sorted.iterator();
            while ( i.hasNext() )
            {
                Object plugin = i.next();
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
     * @param plugins     a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                    values being {@link String} plugin version.
     */
    private void debugVersionMap( String description, Map plugins )
    {
        if ( getLog().isDebugEnabled() )
        {
            StringBuffer buf = new StringBuffer( description );
            Iterator i = plugins.entrySet().iterator();
            while ( i.hasNext() )
            {
                Map.Entry plugin = (Map.Entry) i.next();
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
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getArtifactId()
            : ( (Plugin) plugin ).getArtifactId();
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
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getGroupId()
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
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getVersion()
            : ( (Plugin) plugin ).getVersion();
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param model                the model to get the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be
     *                             inherited by child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map getReportPlugins( Model model, boolean onlyIncludeInherited )
    {
        Map reportPlugins = new HashMap();
        try
        {
            Iterator j = model.getReporting().getPlugins().iterator();
            while ( j.hasNext() )
            {
                Object plugin = j.next();
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
            Iterator i = model.getProfiles().iterator();
            while ( i.hasNext() )
            {
                Profile profile = (Profile) i.next();
                try
                {
                    Iterator j = profile.getReporting().getPlugins().iterator();
                    while ( j.hasNext() )
                    {
                        Object plugin = j.next();
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


}
