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

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Plugin;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.util.StringUtils;

import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Generates a report of available updates for the dependencies of a project.
 *
 * @author Stephen Connolly
 * @goal plugin-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 1.0-beta-1
 */
public class PluginUpdatesReport
    extends AbstractVersionsReport
{
    /**
     * {@inheritDoc}
     */
    public boolean isExternalReport()
    {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public boolean canGenerateReport()
    {
        return haveBuildPlugins() || haveBuildPluginManagementPlugins();
    }

    private boolean haveBuildPluginManagementPlugins()
    {
        return getProject().getBuild() != null && getProject().getBuild().getPluginManagement() != null
            && getProject().getBuild().getPluginManagement().getPlugins() != null
            && !getProject().getBuild().getPluginManagement().getPlugins().isEmpty();
    }

    private boolean haveBuildPlugins()
    {
        return getProject().getBuild() != null && getProject().getBuild().getPlugins() != null
            && !getProject().getBuild().getPlugins().isEmpty();
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        Set pluginManagement = new TreeSet( new PluginComparator() );
        if ( haveBuildPluginManagementPlugins() )
        {
            pluginManagement.addAll( getProject().getBuild().getPluginManagement().getPlugins() );
        }

        Set plugins = new TreeSet( new PluginComparator() );
        if ( haveBuildPlugins() )
        {
            plugins.addAll( getProject().getBuild().getPlugins() );
        }
        plugins.addAll( getProject().getDependencies() );
        plugins = removePluginManagment( plugins, pluginManagement );

        Map/*<Plugin,PluginUpdateDetails>*/ pluginUpdates = lookupPluginsUpdates( plugins );
        Map/*<Plugin,PluginUpdateDetails>*/ pluginManagementUpdates = lookupPluginsUpdates( pluginManagement );
        PluginUpdatesRenderer renderer =
            new PluginUpdatesRenderer( sink, getI18n(), getOutputName(), locale, pluginUpdates,
                                           pluginManagementUpdates );
        renderer.render();
    }

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param plugins          The set of dependencies.
     * @param pluginManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set removePluginManagment( Set plugins, Set pluginManagement )
    {
        Set result = new TreeSet( new PluginComparator() );
        for ( Iterator i = plugins.iterator(); i.hasNext(); )
        {
            Plugin c = (Plugin) i.next();
            boolean matched = false;
            Iterator j = pluginManagement.iterator();
            while ( !matched && j.hasNext() )
            {
                Plugin t = (Plugin) j.next();
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() ) && ( c.getVersion() == null
                    || t.getVersion() == null || StringUtils.equals( t.getVersion(), c.getVersion() ) ) )
                {
                    matched = true;
                    break;
                }
            }
            if ( !matched )
            {
                result.add( c );
            }
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "plugin-updates-report";
    }

}