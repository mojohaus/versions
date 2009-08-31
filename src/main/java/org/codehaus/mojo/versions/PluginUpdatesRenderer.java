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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.parser.Parser;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.i18n.I18N;

import java.text.MessageFormat;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * @since 1.0-beta-1
 */
public class PluginUpdatesRenderer
    extends AbstractVersionsReportRenderer
{
    private final Map/*<Plugin,PluginUpdateDetails>*/ pluginUpdates;

    private final Map/*<Plugin,PluginUpdateDetails>*/ pluginManagementUpdates;

    public PluginUpdatesRenderer( Sink sink, I18N i18n, String bundleName, Locale locale, Map pluginUpdates,
                                  Map pluginManagementUpdates )
    {
        super( sink, bundleName, i18n, locale );
        this.pluginUpdates = pluginUpdates;
        this.pluginManagementUpdates = pluginManagementUpdates;
    }

    protected void renderBody()
    {
        Map allUpdates = new TreeMap( new PluginComparator() );
        allUpdates.putAll( pluginManagementUpdates );
        allUpdates.putAll( pluginUpdates );

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.overview.text" ) );
        sink.paragraph_();

        renderSummaryTotalsTable( allUpdates );

        renderSummaryTable( "report.overview.pluginManagement", pluginManagementUpdates,
                            "report.overview.noPluginManagement" );

        renderSummaryTable( "report.overview.plugin", pluginUpdates, "report.overview.noPlugin" );

        sink.section1_();

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.detail.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.detail.text" ) );
        sink.paragraph_();

        for ( Iterator it = allUpdates.entrySet().iterator(); it.hasNext(); )
        {
            final Map.Entry/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ entry =
                (Map.Entry) it.next();
            Plugin plugin = (Plugin) entry.getKey();
            PluginUpdatesDetails details = (PluginUpdatesDetails) entry.getValue();
            renderPluginDetail( plugin, details );
        }
        sink.section1_();

    }

    private void renderSummaryTotalsTable( Map allUpdates )
    {
        int numInc = 0;
        int numMin = 0;
        int numMaj = 0;
        int numAny = 0;
        int numCur = 0;
        int numDep = 0;
        for ( Iterator iterator = allUpdates.values().iterator(); iterator.hasNext(); )
        {
            PluginUpdatesDetails pluginDetails = (PluginUpdatesDetails) iterator.next();
            ArtifactVersions details = pluginDetails.getArtifactVersions();
            if ( details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
            {
                numAny++;
            }
            else if ( details.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
            {
                numInc++;
            }
            else if ( details.getOldestUpdate( UpdateScope.MINOR ) != null )
            {
                numMin++;
            }
            else if ( details.getOldestUpdate( UpdateScope.MAJOR ) != null )
            {
                numMaj++;
            }
            else
            {
                numCur++;
            }
            if ( pluginDetails.isDependencyUpdateAvailable() )
            {
                numDep++;
            }
        }
        sink.table();
        sink.tableRow();
        sink.tableCell();
        renderSuccessIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numUpToDate" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numCur ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerVersionAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numAny ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerIncrementalAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numInc ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerMinorAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numMin ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerMajorAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numMaj ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerDependenciesAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numDep ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.table_();
    }

    private void renderSummaryTable( String titleKey, Map contents, String emptyKey )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( getText( titleKey ) );
        sink.sectionTitle2_();

        if ( contents.isEmpty() )
        {
            sink.paragraph();
            sink.text( getText( emptyKey ) );
            sink.paragraph_();
        }
        else
        {
            sink.table();
            renderSummaryTableHeader();
            for ( Iterator it = contents.entrySet().iterator(); it.hasNext(); )
            {
                final Map.Entry/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ entry =
                    (Map.Entry) it.next();
                Plugin dependency = (Plugin) entry.getKey();
                PluginUpdatesDetails details = (PluginUpdatesDetails) entry.getValue();
                renderPluginSummary( dependency, details );
            }
            renderSummaryTableHeader();
            sink.table_();
        }
        sink.section2_();
    }

    private void renderSummaryTableHeader()
    {
        sink.tableRow();
        sink.tableHeaderCell();
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.groupId" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextVersion" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextMinor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextMajor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.dependencyStatus" ) );
        sink.tableHeaderCell_();
        sink.tableRow_();
    }

    private void renderPluginSummary( Plugin plugin, PluginUpdatesDetails details )
    {
        sink.tableRow();
        sink.tableCell();
        if ( !details.isUpdateAvailable() )
        {
            renderSuccessIcon();
        }
        else
        {
            renderWarningIcon();
        }
        sink.tableCell_();
        sink.tableCell();
        sink.text( plugin.getGroupId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( plugin.getArtifactId() );
        sink.tableCell_();
        sink.tableCell();
        if ( !details.isArtifactUpdateAvailable() )
        {
            safeBold();
        }
        sink.text( plugin.getVersion() );
        if ( !details.isArtifactUpdateAvailable() )
        {
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            safeBold();
            sink.text( details.getArtifactVersions().getOldestUpdate( UpdateScope.SUBINCREMENTAL ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            safeBold();
            sink.text( details.getArtifactVersions().getOldestUpdate( UpdateScope.INCREMENTAL ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            safeBold();
            sink.text( details.getArtifactVersions().getOldestUpdate( UpdateScope.MINOR ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.MAJOR ) != null )
        {
            safeBold();
            sink.text( details.getArtifactVersions().getOldestUpdate( UpdateScope.MAJOR ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.isDependencyUpdateAvailable() )
        {
            renderWarningIcon();
        }
        else
        {
            renderSuccessIcon();
        }
        sink.tableCell_();

        sink.tableRow_();
    }

    private void renderPluginDetail( Plugin plugin, PluginUpdatesDetails details )
    {
        final String cellWidth = "80%";
        final String headerWidth = "20%";
        sink.section2();
        sink.sectionTitle2();
        sink.text( MessageFormat.format( getText( "report.plugin" ), new Object[]{
            ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() )} ) );
        sink.sectionTitle2_();
        sink.table();
        sink.tableRows( new int[]{Parser.JUSTIFY_RIGHT, Parser.JUSTIFY_LEFT}, false );
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        ArtifactVersion[] versions = details.getArtifactVersions().getAllUpdates( UpdateScope.ANY );
        if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( details.getArtifactVersions().getOldestUpdate( UpdateScope.MAJOR ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.majorUpdatesAvailable" ) );
        }
        else
        {
            renderSuccessIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.noUpdatesAvailable" ) );
        }
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.groupId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( plugin.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( plugin.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( plugin.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( versions.length > 0 )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerWidth );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellWidth );
            for ( int i = 0; i < versions.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.lineBreak();
                }
                boolean bold = equals( versions[i],
                                       details.getArtifactVersions().getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) ||
                    equals( versions[i], details.getArtifactVersions().getOldestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( versions[i], details.getArtifactVersions().getNewestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( versions[i], details.getArtifactVersions().getOldestUpdate( UpdateScope.MINOR ) ) ||
                    equals( versions[i], details.getArtifactVersions().getNewestUpdate( UpdateScope.MINOR ) ) ||
                    equals( versions[i], details.getArtifactVersions().getOldestUpdate( UpdateScope.MAJOR ) ) ||
                    equals( versions[i], details.getArtifactVersions().getNewestUpdate( UpdateScope.MAJOR ) );
                if ( bold )
                {
                    safeBold();
                }
                sink.text( versions[i].toString() );
                if ( bold )
                {
                    safeBold_();
                    sink.nonBreakingSpace();
                    safeItalic();
                    if ( equals( versions[i],
                                 details.getArtifactVersions().getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextVersion" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getOldestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextIncremental" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getNewestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.latestIncremental" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getOldestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.nextMinor" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getNewestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.latestMinor" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getOldestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.nextMajor" ) );
                    }
                    else if ( equals( versions[i],
                                      details.getArtifactVersions().getNewestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.latestMajor" ) );
                    }

                    safeItalic_();
                }
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRows_();
        sink.table_();

        if ( !details.getDependencyVersions().isEmpty() )
        {
            sink.section3();
            sink.sectionTitle3();
            sink.text( MessageFormat.format( getText( "report.pluginDependencies" ), new Object[]{
                ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() )} ) );
            sink.sectionTitle3_();

            renderDependencySummaryTable( details.getDependencyVersions(), false, true, true );

            sink.section3_();

            for ( Iterator i = details.getDependencyVersions().entrySet().iterator(); i.hasNext(); )
            {
                Map.Entry entry = (Map.Entry) i.next();
                renderDependencyDetail( (Dependency) entry.getKey(), (ArtifactVersions) entry.getValue() );
            }
        }
        sink.section2_();
    }

    private void renderDependencyDetail( Dependency dependency, ArtifactVersions details )
    {
        sink.section3();
        sink.sectionTitle3();
        sink.text( MessageFormat.format( getText( "report.pluginDependency" ), new Object[]{
            ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() )} ) );
        sink.sectionTitle3_();
        renderDependencyDetailTable( dependency, details, false, true, true );
        sink.section3_();
    }


}
