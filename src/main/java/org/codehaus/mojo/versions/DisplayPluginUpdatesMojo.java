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
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuildingException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Displays all plugins that have newer versions available.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal display-plugin-updates
 * @requiresProject true
 * @requiresDirectInvocation false
 * @since 1.0
 */
public class DisplayPluginUpdatesMojo
    extends AbstractVersionsUpdaterMojo
{
    /**
     * The width to pad warn messages.
     */
    private static final int WARN_PAD_SIZE = 65;

    /**
     * The width to pad info messages.
     */
    private static final int INFO_PAD_SIZE = 68;

    /**
     * String to flag a plugin version being forced by the super-pom.
     */
    private static final String FROM_SUPER_POM = "(from super-pom) ";

// ------------------------ INTERFACE METHODS ------------------------

// --------------------- Interface Mojo ---------------------

    /**
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        List parents = getParentProjects( getProject() );

        Map superPomPluginManagement = getSuperPomPluginManagement();

        Map parentPluginManagement = new HashMap();
        Map parentBuildPlugins = new HashMap();
        Map parentReportPlugins = new HashMap();
        Iterator i = parents.iterator();
        while ( i.hasNext() )
        {
            MavenProject parentProject = (MavenProject) i.next();
            parentPluginManagement.putAll( getPluginManagement( parentProject ) );
            parentBuildPlugins.putAll( getBuildPlugins( parentProject, true ) );
            parentReportPlugins.putAll( getReportPlugins( parentProject, true ) );
        }

        Set plugins = getProjectPlugins( superPomPluginManagement, parentPluginManagement, parentBuildPlugins,
                                         parentReportPlugins );
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
            getLog().debug( new StringBuffer()
                .append( "Checking " )
                .append( coords )
                .append( " for updates newer than " )
                .append( version )
                .toString() );

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

            ArtifactVersion artifactVersion = findLatestVersion( artifact, versionRange, null );

            String newVersion;

            if ( version == null && artifactVersion != null )
            {
                version = (String) superPomPluginManagement.get( ArtifactUtils.versionlessKey( artifact ) );
                newVersion = version != null ? version : artifactVersion.toString();
                StringBuffer buf = new StringBuffer();
                if ( "org.apache.maven.plugins".equals( groupId ) )
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
            if ( version != null && artifactVersion != null && version.compareTo( newVersion ) < 0 )
            {
                StringBuffer buf = new StringBuffer();
                if ( "org.apache.maven.plugins".equals( groupId ) )
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
            getLog().info( "All plugins are using the latest versions." );
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

    /**
     * Returns the set of all plugins used by the project.
     *
     * @param superPomPluginManagement the super pom's pluginManagement plugins.
     * @param parentPluginManagement   the parent pom's pluginManagement plugins.
     * @param parentBuildPlugins       the parent pom's build plugins.
     * @param parentReportPlugins      the parent pom's report plugins.
     * @return the set of plugins used by the project.
     */
    private Set getProjectPlugins( Map superPomPluginManagement, Map parentPluginManagement, Map parentBuildPlugins,
                                   Map parentReportPlugins )
    {
        Iterator i;
        Set plugins = new TreeSet( new PluginComparator() );

        Map excludePluginManagement = new HashMap( superPomPluginManagement );
        excludePluginManagement.putAll( parentPluginManagement );

        try
        {
            addProjectPlugins( plugins, getProject().getOriginalModel().getBuild().getPluginManagement().getPlugins(),
                               excludePluginManagement );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }

        try
        {
            addProjectPlugins( plugins, getProject().getOriginalModel().getBuild().getPlugins(), parentBuildPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }

        try
        {
            addProjectPlugins( plugins, getProject().getOriginalModel().getReporting().getPlugins(),
                               parentReportPlugins );
        }
        catch ( NullPointerException e )
        {
            // guess there are no plugins here
        }
        i = getProject().getOriginalModel().getProfiles().iterator();
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

            try
            {
                addProjectPlugins( plugins, profile.getBuild().getPlugins(), parentBuildPlugins );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }

            try
            {
                addProjectPlugins( plugins, profile.getReporting().getPlugins(), parentReportPlugins );
            }
            catch ( NullPointerException e )
            {
                // guess there are no plugins here
            }
        }
        return plugins;
    }

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
            superPomPluginManagement.putAll(
                getPluginManagement( projectBuilder.buildStandaloneSuperProject( localRepository ) ) );
        }
        catch ( ProjectBuildingException e )
        {
            throw new MojoExecutionException( "Could not determine the super pom.xml", e );
        }
        return superPomPluginManagement;
    }

// -------------------------- OTHER METHODS --------------------------

    /**
     * Adds those project plugins which are not inherited from the parent definitions to the list of plugins.
     *
     * @param plugins           The list of plugins.
     * @param projectPlugins    The project's plugins.
     * @param parentDefinitions The parent plugin definitions.
     * @since 1.0
     */
    private void addProjectPlugins( Set plugins, Collection projectPlugins, Map parentDefinitions )
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
                plugins.add( plugin );
            }
        }
    }

    /**
     * Returns the coordinates of a plugin.
     *
     * @param plugin The plugin
     * @return The groupId and artifactId separated by a colon.
     * @since 1.0
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
     * @since 1.0
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
     * @since 1.0
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
     * @since 1.0
     */
    private static String getPluginVersion( Object plugin )
    {
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getVersion()
            : ( (Plugin) plugin ).getVersion();
    }

    /**
     * Gets the build plugins of a specific project.
     *
     * @param project              the project to get the build plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be
     *                             inherited by child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0
     */
    private Map getBuildPlugins( MavenProject project, boolean onlyIncludeInherited )
    {
        Map buildPlugins = new HashMap();
        try
        {
            Iterator j = project.getOriginalModel().getBuild().getPlugins().iterator();
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
        Iterator i = project.getOriginalModel().getProfiles().iterator();
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
        return buildPlugins;
    }

    /**
     * Returns the Inherited of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Inherited of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0
     */
    private static boolean getPluginInherited( Object plugin )
    {
        return "true".equalsIgnoreCase( plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getInherited()
            : ( (Plugin) plugin ).getInherited() );
    }

    /**
     * Returns all the parent projects of the specified project, with the root project first.
     *
     * @param project The maven project to get the parents of
     * @return the parent projects of the specified project, with the root project first.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          if the super-pom could not be created.
     * @since 1.0
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

    /**
     * Gets the plugin management plugins of a specific project.
     *
     * @param project the project to get the plugin management plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0
     */
    private Map getPluginManagement( MavenProject project )
    {
        // we want only those parts of pluginManagement that are defined in this project
        Map pluginManagement = new HashMap();
        try
        {
            Iterator j = project.getOriginalModel().getBuild().getPluginManagement().getPlugins().iterator();
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
        Iterator i = project.getOriginalModel().getProfiles().iterator();
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

        return pluginManagement;
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param project              the project to get the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be
     *                             inherited by child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0
     */
    private Map getReportPlugins( MavenProject project, boolean onlyIncludeInherited )
    {
        Map reportPlugins = new HashMap();
        try
        {
            Iterator j = project.getOriginalModel().getReporting().getPlugins().iterator();
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
        Iterator i = project.getOriginalModel().getProfiles().iterator();
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
        return reportPlugins;
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

// -------------------------- INNER CLASSES --------------------------

    /**
     * A comparator used to sort plugins by group id, artifact id and finally version.
     *
     * @since 1.0
     */
    private static class PluginComparator
        implements Comparator
    {

        /**
         * @param o1 the first object
         * @param o2 the second object.
         * @return the comparison result
         * @see java.util.Comparator#compare(Object, Object)
         * @since 1.0
         */
        public int compare( Object o1, Object o2 )
        {
            int r = getPluginGroupId( o1 ).compareTo( getPluginGroupId( o2 ) );
            if ( r == 0 )
            {
                r = getPluginArtifactId( o1 ).compareTo( getPluginArtifactId( o2 ) );
            }
            if ( r == 0 )
            {
                String v1 = getPluginVersion( o1 );
                String v2 = getPluginVersion( o2 );
                if ( v1 == null )
                {
                    // hope I got the +1/-1 the right way around
                    return v2 == null ? 0 : -1;
                }
                if ( v2 == null )
                {
                    return 1;
                }
                r = v1.compareTo( v2 );
            }
            return r;
        }

    }

}
