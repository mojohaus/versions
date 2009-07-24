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
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.i18n.I18N;

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
            ArtifactUpdatesDetails details = pluginDetails.getArtifactDetails();
            if ( details.getNextVersion() != null )
            {
                numAny++;
            }
            else if ( details.getNextIncremental() != null )
            {
                numInc++;
            }
            else if ( details.getNextMinor() != null )
            {
                numMin++;
            }
            else if ( details.getNextMajor() != null )
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
        sink.text( Integer.toString( numMaj ) );
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
        sink.text( getText( "report.hasDependencyUpdates" ) );
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
        sink.text( plugin.getVersion() );
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactDetails().getNextVersion() != null )
        {
            sink.bold();
            sink.text( details.getArtifactDetails().getNextVersion().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactDetails().getNextIncremental() != null )
        {
            sink.bold();
            sink.text( details.getArtifactDetails().getNextIncremental().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactDetails().getNextMinor() != null )
        {
            sink.bold();
            sink.text( details.getArtifactDetails().getNextMinor().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getArtifactDetails().getNextMajor() != null )
        {
            sink.bold();
            sink.text( details.getArtifactDetails().getNextMajor().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        sink.text( details.isDependencyUpdateAvailable() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
    }

    private void renderPluginDetail( Plugin plugin, PluginUpdatesDetails details )
    {
        //To change body of created methods use File | Settings | File Templates.
    }

}
