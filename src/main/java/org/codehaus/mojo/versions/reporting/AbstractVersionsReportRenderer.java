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

import java.util.Locale;
import java.util.Map;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.sink.SinkEventAttributes;
import org.apache.maven.doxia.sink.impl.SinkEventAttributeSet;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.api.ReportRenderer;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Base class for report renderers.
 * @param <T> modelled report object
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionsReportRenderer<T> extends VersionsReportRendererBase implements ReportRenderer
{

    /**
     * Model of the object being rendered
     *
     * @since 2.13.0
     */
    protected T model;

    protected final ArtifactVersionsCache newestUpdateCache
            = new ArtifactVersionsCache( AbstractVersionDetails::getNewestUpdate );

    protected final ArtifactVersionsCache allUpdatesCache
            = new ArtifactVersionsCache( AbstractVersionDetails::getAllUpdates );

    protected final SinkEventAttributes headerAttributes
            = new SinkEventAttributeSet( SinkEventAttributes.WIDTH, "30%" );

    /**
     * Constructor to be called by the dependency injection framework
     * @param i18n i18n object to be injected
     */
    protected AbstractVersionsReportRenderer( I18N i18n, Sink sink, Locale locale, String bundleName, T model )
    {
        super( sink, i18n, locale, bundleName );
        this.model = model;
    }

    /**
     * {@inheritDoc}
     */
    protected void renderBody()
    {
        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.overview.text" ) );
        sink.paragraph_();

        renderOverview();

        renderManagementSummaryTable();
        renderSummaryTable();

        sink.section1_();

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.detail.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.detail.text" ) );
        sink.paragraph_();

        renderDetails();

        sink.section1_();
    }

    /**
     * Renders the "Overview" table
     */
    protected void renderOverview()
    {
        sink.table();
        sink.tableRow();
        renderOverviewTableRow( computeOverviewStats() );
        sink.tableRow_();
        sink.table_();
    }

    /**
     * Renders the "management" (dependencyManagement, pluginManagement, etc.) summary table
     */
    protected abstract void renderManagementSummaryTable();

    /**
     * Renders the regular ("dependencies", "plugins", etc.) summary table
     */
    protected abstract void renderSummaryTable();

    /**
     * Renders the singular summary table row
     * @param stats summary statistics object to render
     * @param <Q> concrete {@linkplain OverviewStats} class
     */
    protected <Q extends OverviewStats> void renderOverviewTableRow( Q stats )
    {
        renderStatRow( "report.overview.numUpToDate", stats.getUpToDate(), true );
        renderStatRow( "report.overview.numNewerVersionAvailable", stats.getAny(), false );
        renderStatRow( "report.overview.numNewerIncrementalAvailable", stats.getIncremental(), false );
        renderStatRow( "report.overview.numNewerMinorAvailable", stats.getMinor(), false );
        renderStatRow( "report.overview.numNewerMajorAvailable", stats.getMajor(), false );
    }

    protected void renderStatRow( String textKey, int statCount, boolean forceSuccessIcon )
    {
        sink.tableRow();
        sink.tableCell();
        renderIcon( statCount == 0 || forceSuccessIcon );
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( textKey ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( statCount ) );
        sink.tableCell_();
        sink.tableRow_();
    }

    protected void renderIcon( boolean success )
    {
        if ( success )
        {
            renderSuccessIcon();
        }
        else
        {
            renderWarningIcon();
        }
    }

    /**
     * Computes the {@linkplain OverviewStats} object needed to render the summary table row
     * @param <Q> concrete {@linkplain OverviewStats} class
     * @return stats object
     */
    protected abstract <Q extends OverviewStats> Q computeOverviewStats();

    /**
     * Renders the details table
     */
    protected abstract void renderDetails();

    protected void renderSummaryTable( Map<Dependency, ArtifactVersions> contents, boolean hasScope )
    {
        sink.table();

        sink.tableRow();
        renderSummaryTableHeader( hasScope, true );
        sink.tableRow_();

        contents.forEach( ( artifact, artifactVersions ) ->
                renderSummaryTableRow( artifact, artifactVersions, hasScope, false ) );

        sink.tableRow();
        renderSummaryTableHeader( hasScope, true );
        sink.tableRow_();

        sink.table_();
    }

    protected void renderSummaryTableHeader( boolean hasScope, boolean hasType )
    {
        renderTableHeaderCells( "report.status", "report.groupId", "report.artifactId",
                "report.currentVersion" );
        if ( hasScope )
        {
            renderTableHeaderCells( "report.scope" );
        }
        if ( hasType )
        {
            renderTableHeaderCells( "report.classifier", "report.type" );
        }
        renderTableHeaderCells( "report.latestSubIncremental",
                "report.latestIncremental", "report.latestMinor", "report.latestMajor" );
    }

    protected void renderSummaryTableRow( Dependency artifact, ArtifactVersions details, boolean includeScope,
                                          boolean verbose )
    {
        // THIS ONE
        ArtifactVersion[] allUpdates = allUpdatesCache.get( details, empty() );
        boolean upToDate = allUpdates == null || allUpdates.length == 0;
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
        renderCell( artifact.getVersion() );
        if ( includeScope )
        {
            renderCell( artifact.getScope() );
        }
        renderCell( artifact.getClassifier() );
        renderCell( artifact.getType() );
        renderVersions( cache, details );

        sink.tableRow_();
    }

    protected void renderVersions( ArtifactVersionsCache cache, ArtifactVersions details )
    {
        renderBoldCell( cache.get( details, of( SUBINCREMENTAL ) ) );
        renderBoldCell( cache.get( details, of( INCREMENTAL ) ) );
        renderBoldCell( cache.get( details, of( MINOR ) ) );
        renderBoldCell( cache.get( details, of( MAJOR ) ) );
    }

    protected void renderTwoCellsRow( String textKey, String textValue )
    {
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( textKey ) );
        sink.tableHeaderCell_();
        sink.tableCell();
        sink.text( textValue );
        sink.tableCell_();
        sink.tableRow_();
    }

    protected void renderTwoCellsRow( String textKey, Runnable runnable )
    {
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( textKey ) );
        sink.tableHeaderCell_();
        sink.tableCell();
        runnable.run();
        sink.tableCell_();
        sink.tableRow_();
    }

    protected void renderStatus( ArtifactVersions details )
    {
        if ( newestUpdateCache.get( details, of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( newestUpdateCache.get( details, of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( newestUpdateCache.get( details, of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( newestUpdateCache.get( details, of( MAJOR ) ) != null )
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

    @SuppressWarnings( "checkstyle:MethodLength" )
    protected void renderDependencyDetailTable( Dependency artifact, ArtifactVersions details, boolean includeScope )
    {
        renderSummaryTableRow( artifact, details, includeScope, true );
    }

    protected String getLabel( ArtifactVersion version, AbstractVersionDetails details )
    {

        if ( equals( version, newestUpdateCache.get( details, of( SUBINCREMENTAL ) ) ) )
        {
            return getText( "report.latestSubIncremental" );
        }

        if ( equals( version, newestUpdateCache.get( details, of( INCREMENTAL ) ) ) )
        {
            return getText( "report.latestIncremental" );
        }

        if ( equals( version, newestUpdateCache.get( details, of( MINOR ) ) ) )
        {
            return getText( "report.latestMinor" );
        }

        if ( equals( version, newestUpdateCache.get( details, of( MAJOR ) ) ) )
        {
            return getText( "report.latestMajor" );
        }

        return null;
    }
}
