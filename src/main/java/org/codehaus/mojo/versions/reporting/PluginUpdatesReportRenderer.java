package org.codehaus.mojo.versions.reporting;

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

import java.text.MessageFormat;
import java.util.Locale;
import java.util.Map;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.sink.SinkEventAttributes;
import org.apache.maven.doxia.sink.impl.SinkEventAttributeSet;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * @since 1.0-beta-1
 */
public class PluginUpdatesReportRenderer extends AbstractVersionsReportRenderer<PluginUpdatesModel>
{
    protected ArtifactVersionsCache newestUpdateCache
            = new ArtifactVersionsCache( AbstractVersionDetails::getNewestUpdate );

    public PluginUpdatesReportRenderer( I18N i18n, Sink sink, Locale locale, String bundleName,
                                        PluginUpdatesModel model )
    {
        super( i18n, sink, locale, bundleName, model );
    }

    @Override
    protected void renderSummaryTable()
    {
        renderTable( "report.overview.plugin", model.getArtifactUpdates(),
                "report.overview.noPlugin" );
    }

    protected void renderManagementSummaryTable()
    {
        renderTable( "report.overview.pluginManagement", model.getArtifactManagementUpdates(),
                "report.overview.noPluginManagement" );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails()
    {
        model.getAllUpdates().forEach( this::renderPluginDetail );
    }

    private void renderDependencyDetail( Dependency dependency, ArtifactVersions details )
    {
        sink.section3();
        sink.sectionTitle3();
        sink.text( MessageFormat.format( getText( "report.pluginDependency" ),
                ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() ) ) );
        sink.sectionTitle3_();
        renderDependencyDetailTable( dependency, details, false );
        sink.section3_();
    }

    private void renderTable( String titleKey, Map<Dependency, PluginUpdatesDetails> contents, String emptyKey )
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
            renderSummaryTable( contents );
        }
        sink.section2_();
    }

    protected void renderSummaryTable( Map<Dependency, PluginUpdatesDetails> contents )
    {
        sink.table();

        sink.tableRow();
        renderSummaryTableHeader( false, false );
        sink.tableRow_();

        contents.forEach( this::renderSummaryTableRow );

        sink.tableRow();
        renderSummaryTableHeader( false, false );
        sink.tableRow_();

        sink.table_();
    }

    /**
     * Extension of the {@linkplain OverviewStats} adding dependency stats
     */
    static class PluginOverviewStats extends OverviewStats
    {
        private int dependencies;

        public int getDependencies()
        {
            return dependencies;
        }

        public void incrementDependencies()
        {
            dependencies++;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PluginOverviewStats computeOverviewStats()
    {
        PluginOverviewStats stats = new PluginOverviewStats();
        model.getAllUpdates().values().forEach( details ->
        {
            if ( oldestUpdateCache.get( details, of( SUBINCREMENTAL ) ) != null )
            {
                stats.incrementAny();
            }
            else if ( oldestUpdateCache.get( details, of( INCREMENTAL ) ) != null )
            {
                stats.incrementIncremental();
            }
            else if ( oldestUpdateCache.get( details, of( MINOR ) ) != null )
            {
                stats.incrementMinor();
            }
            else if ( oldestUpdateCache.get( details, of( MAJOR ) ) != null )
            {
                stats.incrementMajor();
            }
            else
            {
                stats.incrementUpToDate();
            }
            if ( details.isDependencyUpdateAvailable() )
            {
                stats.incrementDependencies();
            }
        } );
        return stats;
    }

    @Override
    protected void renderSummaryTableHeader( boolean hasScope, boolean hasType )
    {
        super.renderSummaryTableHeader( hasScope, hasType );
        renderTableHeaderCells( "report.dependencyStatus" );
    }

    @Override
    protected <T extends OverviewStats> void renderOverviewTableRow( T stats )
    {
        super.renderOverviewTableRow( stats );
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerDependenciesAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( ( (PluginOverviewStats) stats ).getDependencies() ) );
        sink.tableCell_();
    }

    protected void renderSummaryTableRow( Dependency artifact, PluginUpdatesDetails details )
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
        sink.text( artifact.getGroupId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( artifact.getArtifactId() );
        sink.tableCell_();
        sink.tableCell();
        if ( !details.isArtifactUpdateAvailable() )
        {
            safeBold();
        }
        sink.text( artifact.getVersion() );
        if ( !details.isArtifactUpdateAvailable() )
        {
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( newestUpdateCache.get( details, of( SUBINCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( newestUpdateCache.get( details, of( SUBINCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( newestUpdateCache.get( details, of( INCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( newestUpdateCache.get( details, of( INCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( newestUpdateCache.get( details, of( MINOR ) ) != null )
        {
            safeBold();
            sink.text( newestUpdateCache.get( details, of( MINOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( newestUpdateCache.get( details, of( MAJOR ) ) != null )
        {
            safeBold();
            sink.text( newestUpdateCache.get( details, of( MAJOR ) ).toString() );
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

    @SuppressWarnings( "checkstyle:MethodLength" )
    private void renderPluginDetail( Dependency artifact, PluginUpdatesDetails plugin )
    {
        final SinkEventAttributes headerAttributes = new SinkEventAttributeSet();
        headerAttributes.addAttribute( SinkEventAttributes.WIDTH, "20%" );
        final SinkEventAttributes cellAttributes = new SinkEventAttributeSet();
        headerAttributes.addAttribute( SinkEventAttributes.WIDTH, "80%" );
        sink.section2();
        sink.sectionTitle2();
        sink.text( MessageFormat.format( getText( "report.plugin" ),
                ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() ) ) );
        sink.sectionTitle2_();
        sink.table();
        sink.tableRows( new int[] {Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT}, false );
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        ArtifactVersion[] versions = plugin.getAllUpdates( empty() );
        if ( plugin.getOldestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( plugin.getOldestUpdate( of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( plugin.getOldestUpdate( of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( plugin.getOldestUpdate( of( MAJOR ) ) != null )
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
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.groupId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( plugin.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( plugin.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( plugin.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( versions != null && versions.length > 0 )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            for ( int i = 0; i < versions.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.lineBreak();
                }
                String label = getLabel( versions[i], plugin );
                if ( label != null )
                {
                    safeBold();
                }
                sink.text( versions[i].toString() );
                if ( label != null )
                {
                    safeBold_();
                    sink.nonBreakingSpace();
                    safeItalic();
                    sink.text( label );
                    safeItalic_();
                }
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRows_();
        sink.table_();

        if ( !plugin.getDependencyVersions().isEmpty() )
        {
            sink.section3();
            sink.sectionTitle3();
            sink.text( MessageFormat.format( getText( "report.pluginDependencies" ),
                    ArtifactUtils.versionlessKey( plugin.getGroupId(), plugin.getArtifactId() ) ) );
            sink.sectionTitle3_();

            renderSummaryTable( plugin.getDependencyVersions(), false );

            sink.section3_();

            plugin.getDependencyVersions().forEach( this::renderDependencyDetail );
        }
        sink.section2_();
    }
}

