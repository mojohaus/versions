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
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuildingException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Displays the available updates for a project.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal display-plugin-updates
 * @requires-project
 * @description Displays all plugins that have newer versions available.
 */
public class DisplayPluginUpdatesMojo
    extends AbstractVersionsUpdaterMojo
{
    /**
     * @see AbstractVersionsUpdaterMojo#execute()
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        List parents = getParentProjects( getProject() );

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

        Set plugins = new TreeSet( new PluginComparator() );

        addProjectPlugins( plugins, getProject().getPluginManagement().getPlugins(), parentPluginManagement );

        addProjectPlugins( plugins, getProject().getBuildPlugins(), parentBuildPlugins );

        addProjectPlugins( plugins, getProject().getReportPlugins(), parentReportPlugins );

        List updates = new ArrayList();
        List lockdown = new ArrayList();
        i = plugins.iterator();
        while ( i.hasNext() )
        {
            Object plugin = i.next();
            String groupId = getPluginGroupId( plugin );
            String artifactId = getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            getLog().debug( "Checking " + groupId + ":" + artifactId + " for updates newer than " + version );

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
                newVersion = artifactVersion.toString();
                StringBuilder buf = new StringBuilder();
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
                int padding = 65 - newVersion.length();
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( newVersion );
                lockdown.add( buf.toString() );
            }
            else if ( artifactVersion != null && version.compareTo( newVersion = artifactVersion.toString() ) < 0 )
            {
                StringBuilder buf = new StringBuilder();
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
                int padding = 68 - version.length() - newVersion.length() - 4;
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

    private void addProjectPlugins( Set plugins, List pluginSource, Map parentDefinitions )
    {
        Iterator j = pluginSource.iterator();
        while ( j.hasNext() )
        {
            Object plugin = j.next();
            String coord = getPluginGroupId( plugin ) + ":" + getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            String parentVersion = (String) parentDefinitions.get( coord );
            if ( parentVersion == null || !parentVersion.equals( version ) )
            {
                plugins.add( plugin );
            }
        }
    }

    private Map getReportPlugins( MavenProject parentProject, boolean onlyInherited )
    {
        Iterator j;
        Map reportPlugins = new HashMap();
        j = parentProject.getReportPlugins().iterator();
        while ( j.hasNext() )
        {
            Object plugin = j.next();
            String coord = getPluginGroupId( plugin ) + ":" + getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            if ( version != null && ( !onlyInherited || getPluginInherited( plugin ) ) )
            {
                reportPlugins.put( coord, version );
            }
        }
        return reportPlugins;
    }

    private Map getBuildPlugins( MavenProject project, boolean onlyInherited )
    {
        Iterator j = project.getBuildPlugins().iterator();
        Map buildPlugins = new HashMap();
        while ( j.hasNext() )
        {
            Object plugin = j.next();
            String coord = getPluginGroupId( plugin ) + ":" + getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            if ( version != null && ( !onlyInherited || getPluginInherited( plugin ) ) )
            {
                buildPlugins.put( coord, version );
            }
        }
        return buildPlugins;
    }

    private Map getPluginManagement( MavenProject project )
    {
        Map pluginManagement = new HashMap();
        Iterator j = project.getPluginManagement().getPlugins().iterator();
        while ( j.hasNext() )
        {
            Object plugin = j.next();
            String coord = getPluginGroupId( plugin ) + ":" + getPluginArtifactId( plugin );
            String version = getPluginVersion( plugin );
            if ( version != null )
            {
                pluginManagement.put( coord, version );
            }
        }
        return pluginManagement;
    }

    /**
     * Returns all the parent projects of the specified project, with the root project first.
     *
     * @param project The maven project to get the parents of
     * @return the parent projects of the specified project, with the root project first.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          if the super-pom could not be created.
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
        try
        {
            parents.add( 0, projectBuilder.buildStandaloneSuperProject( localRepository ) );
        }
        catch ( ProjectBuildingException e )
        {
            throw new MojoExecutionException( "Could not determine the super pom.xml", e );
        }

        return parents;
    }

    /**
     * Returns the GroupId of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the GroupId of the {@link Plugin} or {@link ReportPlugin}
     */
    private static String getPluginGroupId( Object plugin )
    {
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getGroupId()
            : ( (Plugin) plugin ).getGroupId();
    }

    /**
     * Returns the ArtifactId of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the ArtifactId of the {@link Plugin} or {@link ReportPlugin}
     */
    private static String getPluginArtifactId( Object plugin )
    {
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getArtifactId()
            : ( (Plugin) plugin ).getArtifactId();
    }

    /**
     * Returns the Version of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Version of the {@link Plugin} or {@link ReportPlugin}
     */
    private static String getPluginVersion( Object plugin )
    {
        return plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getVersion()
            : ( (Plugin) plugin ).getVersion();
    }

    /**
     * Returns the Inherited of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Inherited of the {@link Plugin} or {@link ReportPlugin}
     */
    private static boolean getPluginInherited( Object plugin )
    {
        return Boolean.TRUE.equals( plugin instanceof ReportPlugin
            ? ( (ReportPlugin) plugin ).getInherited()
            : ( (Plugin) plugin ).getInherited() );
    }

    /**
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

    /**
     * A comparator used to sort plugins by group id, artifact id and finally version.
     */
    private static class PluginComparator
        implements Comparator
    {
        /**
         * @see java.util.Comparator#compare(Object, Object)
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
