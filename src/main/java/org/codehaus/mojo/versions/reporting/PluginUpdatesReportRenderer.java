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
        model.getAllUpdates().forEach( ( artifact, details ) -> renderPluginDetail( artifact, details, true ) );
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

        contents.forEach( ( artifact, details ) -> renderSummaryTableRow( artifact, details, false ) );

        sink.tableRow();
        renderSummaryTableHeader( false, false );
        sink.tableRow_();

        sink.table_();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PluginOverviewStats computeOverviewStats()
    {
        return PluginOverviewStats.fromUpdates( model.getAllUpdates().values(), newestUpdateCache );
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
        super.renderStatRow( "report.overview.numNewerDependenciesAvailable",
                ( (PluginOverviewStats) stats ).getDependencies(), false );
    }

    protected void renderSummaryTableRow( Dependency artifact, PluginUpdatesDetails details, boolean verbose )
    {
        // THIS ONE
        boolean upToDate = !details.isUpdateAvailable();
        if ( upToDate && !verbose )
        {
            return;
        }
        ArtifactVersionsCache cache = verbose ? allUpdatesCache : newestUpdateCache;

        sink.tableRow();

        sink.tableCell();
        renderIcon( upToDate );
        sink.tableCell_();

        renderCell( artifact.getGroupId() );
        renderCell( artifact.getArtifactId() );
        renderBoldCell( upToDate, artifact.getVersion() );
        renderVersions( cache, details );

        sink.tableCell();
        renderIcon( !details.isDependencyUpdateAvailable() );
        sink.tableCell_();

        sink.tableRow_();
    }

    @SuppressWarnings( "checkstyle:MethodLength" )
    private void renderPluginDetail( Dependency artifact, PluginUpdatesDetails details, boolean verbose )
    {
        boolean upToDate = !details.isUpdateAvailable();
        if ( upToDate && !verbose )
        {
            return;
        }

        renderPluginDetailTable( details );

        if ( !details.getDependencyVersions().isEmpty() )
        {
            sink.section3();
            sink.sectionTitle3();
            sink.text( MessageFormat.format( getText( "report.pluginDependencies" ),
                    ArtifactUtils.versionlessKey( details.getGroupId(), details.getArtifactId() ) ) );
            sink.sectionTitle3_();

            renderSummaryTable( details.getDependencyVersions(), false );

            sink.section3_();

            details.getDependencyVersions().forEach( this::renderDependencyDetail );
        }

    }

    private void renderPluginDetailTable( PluginUpdatesDetails details )
    {
        // THAT ONE
        // warning: using caches here may break plugin report
        ArtifactVersion[] allUpdates = details.getAllUpdates( empty() );
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        sink.table();
        sink.tableRows( new int[] { Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT }, false );

        renderTwoCellsRow( "report.status", () -> renderStatus( details ) );
        renderTwoCellsRow( "report.groupId", details.getGroupId() );
        renderTwoCellsRow( "report.artifactId", details.getArtifactId() );
        renderTwoCellsRow( "report.currentVersion", details.getVersion() );
        if ( !upToDate )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell();
            for ( int i = 0; i < allUpdates.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.lineBreak();
                }
                String label = getLabel( allUpdates[i], details );
                if ( label != null )
                {
                    safeBold();
                }
                sink.text( allUpdates[i].toString() );
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
    }

    protected void renderStatus( AbstractVersionDetails details )
    {
        // warning: using caches here may break plugin report
        if ( details.getNewestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( details.getNewestUpdate( of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( details.getNewestUpdate( of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( details.getNewestUpdate( of( MAJOR ) ) != null )
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
    }

}
