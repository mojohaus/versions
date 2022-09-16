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

import java.io.File;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.i18n.I18N;

import static org.codehaus.mojo.versions.utils.MiscUtils.filter;

/**
 * Generates a report of available updates for the plugins of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "plugin-updates-report", requiresDependencyResolution = ResolutionScope.RUNTIME,
       threadSafe = true )
public class PluginUpdatesReport extends AbstractVersionsReport
{

    /**
     * Report formats (html and/or xml). HTML by default.
     */
    @Parameter( property = "pluginUpdatesReportFormats", defaultValue = "html" )
    private String[] formats = new String[] { "html" };

    /**
     * If <code>true</code>, only shows the subsection of the <code>pluginManagement</code> artifacts that
     * are actually used in the project's <code>plugin</code> graph. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter( property = "onlyProjectPlugins", defaultValue = "false" )
    protected boolean onlyProjectPlugins;

    /**
     * If <code>true</code>, only shows upgradable plugins in the report. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter( property = "onlyUpgradable", defaultValue = "false" )
    protected boolean onlyUpgradable;

    @Inject
    protected PluginUpdatesReport( I18N i18n, RepositorySystem repositorySystem, ArtifactResolver artifactResolver,
                                       ArtifactMetadataSource artifactMetadataSource, WagonManager wagonManager )
    {
        super( i18n, repositorySystem, artifactResolver, artifactMetadataSource, wagonManager );
    }

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
                && getProject().getBuild().getPluginManagement().getPlugins() != null && !getProject().getBuild()
                .getPluginManagement().getPlugins().isEmpty();
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
    protected void doGenerateReport( Locale locale, Sink sink ) throws MavenReportException
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

        PluginComparator comparator = new PluginComparator();
        if ( !onlyProjectPlugins )
        {
            // Retains only plugins not present in pluginManagement
            plugins.removeIf( plugin -> pluginManagement.stream()
                    .anyMatch( pmPlugin -> comparator.compare( plugin, pmPlugin ) == 0 ) );
        }
        else
        {
            // Retain only plugins in pluginManagement that are also present in plugins
            pluginManagement.removeIf(
                    pmPlugin -> plugins.stream().noneMatch( plugin -> comparator.compare( plugin, pmPlugin ) == 0 ) );
        }

        try
        {
            Map<Plugin, PluginUpdatesDetails> pluginUpdates =
                    getHelper().lookupPluginsUpdates( plugins, getAllowSnapshots() );
            Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates =
                    getHelper().lookupPluginsUpdates( pluginManagement, getAllowSnapshots() );

            if ( onlyUpgradable )
            {
                pluginUpdates =
                        filter( pluginUpdates, plugin -> plugin.getArtifactVersions().getVersions().length > 1 );
                pluginManagementUpdates = filter( pluginManagementUpdates,
                        plugin -> plugin.getArtifactVersions().getVersions().length > 1 );
            }

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
                    File outputDir = new File( getProject().getBuild().getDirectory() );
                    if ( !outputDir.exists() )
                    {
                        if ( !outputDir.mkdirs() )
                        {
                            throw new MavenReportException( "Could not create output directory" );
                        }
                    }
                    String outputFile = outputDir.getAbsolutePath() + File.separator + getOutputName() + ".xml";
                    PluginUpdatesXmlRenderer xmlGenerator =
                            new PluginUpdatesXmlRenderer( pluginUpdates, pluginManagementUpdates, outputFile );
                    xmlGenerator.render();
                }
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "plugin-updates-report";
    }

}
