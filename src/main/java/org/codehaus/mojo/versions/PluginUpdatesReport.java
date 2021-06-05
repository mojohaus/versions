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

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.util.StringUtils;

import java.io.File;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Generates a report of available updates for the plugins of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "plugin-updates-report", requiresProject = true, requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true )
public class PluginUpdatesReport
    extends AbstractVersionsReport
{

    /**
     * Report formats (html and/or xml). HTML by default.
     * 
     */
    @Parameter( property = "pluginUpdatesReportFormats", defaultValue = "html" )
    private String[] formats = new String[] { "html" };

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
     * @param sink the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        Set<Plugin> pluginManagement = new TreeSet<>( new PluginComparator() );
        if ( haveBuildPluginManagementPlugins() )
        {
            pluginManagement.addAll( getProject().getBuild().getPluginManagement().getPlugins() );
        }

        Set<Plugin> plugins = new TreeSet<>( new PluginComparator() );
        if ( haveBuildPlugins() )
        {
            plugins.addAll( getProject().getBuild().getPlugins() );
        }

        plugins = removePluginManagment( plugins, pluginManagement );

        try
        {
            Map<Plugin, PluginUpdatesDetails> pluginUpdates =
                getHelper().lookupPluginsUpdates( plugins, getAllowSnapshots() );
            Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates =
                getHelper().lookupPluginsUpdates( pluginManagement, getAllowSnapshots() );
            for ( String format : formats )
            {
                if ( "html".equals( format ) )
                {
                    PluginUpdatesRenderer renderer =
                        new PluginUpdatesRenderer( sink, getI18n(), getOutputName(), locale, pluginUpdates,
                                                   pluginManagementUpdates );
                    renderer.render();
                }
                else if ( "xml".equals( format ) )
                {
                    File outputDir = new File(getProject().getBuild().getDirectory());
                    if (!outputDir.exists())
                    {
                        outputDir.mkdirs();
                    }
                    String outputFile =
                        outputDir.getAbsolutePath() + File.separator + getOutputName() + ".xml";
                    PluginUpdatesXmlRenderer xmlGenerator =
                        new PluginUpdatesXmlRenderer( pluginUpdates, pluginManagementUpdates, outputFile );
                    xmlGenerator.render();
                }
            }
        }
        catch ( InvalidVersionSpecificationException | ArtifactMetadataRetrievalException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param plugins The set of dependencies.
     * @param pluginManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set<Plugin> removePluginManagment( Set<Plugin> plugins, Set<Plugin> pluginManagement )
    {
        Set<Plugin> result = new TreeSet<>( new PluginComparator() );
        for ( Plugin c : plugins )
        {
            boolean matched = false;
            for ( Plugin t : pluginManagement )
            {
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() ) )
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