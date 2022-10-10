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

import java.util.Arrays;
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

    protected ArtifactVersionsCache oldestUpdateCache
            = new ArtifactVersionsCache( AbstractVersionDetails::getOldestUpdate );

    protected ArtifactVersionsCache allUpdatesCache
            = new ArtifactVersionsCache( AbstractVersionDetails::getAllUpdates );

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
        sink.tableCell();
        renderSuccessIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numUpToDate" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( stats.getUpToDate() ) );
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
        sink.text( Integer.toString( stats.getAny() ) );
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
        sink.text( Integer.toString( stats.getIncremental() ) );
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
        sink.text( Integer.toString( stats.getMinor() ) );
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
        sink.text( Integer.toString( stats.getMajor() ) );
        sink.tableCell_();
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
                renderSummaryTableRow( artifact, artifactVersions, hasScope ) );

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

    protected void renderSummaryTableRow( Dependency artifact, ArtifactVersions artifactVersions,
                                        boolean includeScope )
    {
        sink.tableRow();
        sink.tableCell();
        ArtifactVersion[] allUpdates = artifactVersions.getAllUpdates( empty() );
        if ( allUpdates == null || allUpdates.length == 0 )
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
        sink.text( artifact.getVersion() );
        sink.tableCell_();
        if ( includeScope )
        {
            sink.tableCell();
            sink.text( artifact.getScope() );
            sink.tableCell_();
        }
        sink.tableCell();
        sink.text( artifact.getClassifier() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( artifact.getType() );
        sink.tableCell_();

        sink.tableCell();
        if ( artifactVersions.getNewestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( artifactVersions.getNewestUpdate( of( SUBINCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( artifactVersions.getNewestUpdate( of( INCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( artifactVersions.getNewestUpdate( of( INCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( artifactVersions.getNewestUpdate( of( MINOR ) ) != null )
        {
            safeBold();
            sink.text( artifactVersions.getNewestUpdate( of( MINOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( artifactVersions.getNewestUpdate( of( MAJOR ) ) != null )
        {
            safeBold();
            sink.text( artifactVersions.getNewestUpdate( of( MAJOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableRow_();
    }

    @SuppressWarnings( "checkstyle:MethodLength" )
    protected void renderDependencyDetailTable( Dependency artifact, ArtifactVersions details, boolean includeScope )
    {
        final SinkEventAttributes headerAttributes = new SinkEventAttributeSet();
        headerAttributes.addAttribute( SinkEventAttributes.WIDTH, "20%" );
        final SinkEventAttributes cellAttributes = new SinkEventAttributeSet();
        headerAttributes.addAttribute( SinkEventAttributes.WIDTH, "80%" );
        sink.table();
        sink.tableRows( new int[] { Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT }, false );
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        ArtifactVersion[] versions = allUpdatesCache.get( details, empty() );
        if ( oldestUpdateCache.get( details, of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( oldestUpdateCache.get( details, of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( oldestUpdateCache.get( details, of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( oldestUpdateCache.get( details, of( MAJOR ) ) != null )
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
        sink.text( artifact.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( artifact.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( artifact.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( includeScope )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.scope" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            sink.text( artifact.getScope() );
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.classifier" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( artifact.getClassifier() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.type" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( artifact.getType() );
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
                String label = getLabel( versions[i], details );
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
    }

    /**
     * Renders a table header containing elements denoted by the given keys
     * @param keys variable argument list containing keys of the property file to retrieve the
     *             headers from
     */
    protected void renderTableHeaderCells( String... keys )
    {
        Arrays.stream( keys )
                .map( this::getText )
                .forEachOrdered( str ->
                {
                    sink.tableHeaderCell();
                    sink.text( str );
                    sink.tableHeaderCell_();
                } );
    }
}
