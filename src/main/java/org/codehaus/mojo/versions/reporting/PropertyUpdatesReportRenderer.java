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

import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.sink.SinkEventAttributes;
import org.apache.maven.doxia.sink.impl.SinkEventAttributeSet;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.reporting.model.PropertyUpdatesModel;
import org.codehaus.plexus.i18n.I18N;
import org.codehaus.plexus.util.StringUtils;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * @since 1.0-beta-1
 */
public class PropertyUpdatesReportRenderer extends AbstractVersionsReportRenderer<PropertyUpdatesModel>
{
    public PropertyUpdatesReportRenderer( I18N i18n, Sink sink, Locale locale, String bundleName,
                                          PropertyUpdatesModel model )
    {
        super( i18n, sink, locale, bundleName, model );
    }

    @Override
    protected void renderManagementSummaryTable()
    {
    }

    @Override
    protected void renderSummaryTable()
    {
        renderTable( "report.overview.property", model.getAllUpdates(),
                "report.overview.noProperty" );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails()
    {
        model.getAllUpdates().forEach( this::renderPropertyDetail );
    }

    protected void renderTable( String titleKey, Map<Property, PropertyVersions> contents, String emptyKey )
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

    protected void renderSummaryTable( Map<Property, PropertyVersions> contents )
    {
        sink.table();

        sink.tableRow();
        renderSummaryTableHeader( false, false );
        sink.tableRow_();

        contents.forEach( this::renderPropertySummaryTableRow );

        sink.tableRow();
        renderSummaryTableHeader( false, false );
        sink.tableRow_();

        sink.table_();
    }

    private void renderPropertySummaryTableRow( Property property, PropertyVersions versions )
    {
        sink.tableRow();
        sink.tableCell();
        if ( versions.getAllUpdates( empty() ).length == 0 )
        {
            renderSuccessIcon();
        }
        else
        {
            renderWarningIcon();
        }
        sink.tableCell_();
        sink.tableCell();
        sink.text( "${" + property.getName() + "}" );
        sink.tableCell_();
        sink.tableCell();
        sink.text( versions.getCurrentVersion().toString() );
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getNewestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( versions.getNewestUpdate( of( SUBINCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getNewestUpdate( of( INCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( versions.getNewestUpdate( of( INCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getNewestUpdate( of( MINOR ) ) != null )
        {
            safeBold();
            sink.text( versions.getNewestUpdate( of( MINOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getNewestUpdate( of( MAJOR ) ) != null )
        {
            safeBold();
            sink.text( versions.getNewestUpdate( of( MAJOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableRow_();
    }

    @SuppressWarnings( "checkstyle:MethodLength" )
    protected void renderPropertyDetailTable( Property property, PropertyVersions versions )
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
        ArtifactVersion[] artifactVersions = versions.getAllUpdates( empty() );
        Set<String> rangeVersions = getVersionsInRange( property, versions, artifactVersions );
        if ( versions.getOldestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( of( MAJOR ) ) != null )
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
        sink.text( getText( "report.property" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( "${" + property.getName() + "}" );
        sink.tableCell_();
        sink.tableRow_();

        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.associations" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        ArtifactAssociation[] associations = versions.getAssociations();
        for ( int i = 0; i < associations.length; i++ )
        {
            if ( i > 0 )
            {
                sink.lineBreak();
            }
            sink.text( ArtifactUtils.versionlessKey( associations[i].getArtifact() ) );
        }
        sink.tableCell_();
        sink.tableRow_();

        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( versions.getCurrentVersion().toString() );
        sink.tableCell_();
        sink.tableRow_();
        if ( artifactVersions.length > 0 )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            boolean someNotAllowed = false;
            for ( int i = 0; i < artifactVersions.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.lineBreak();
                }
                boolean allowed = ( rangeVersions.contains( artifactVersions[i].toString() ) );
                String label = getLabel( artifactVersions[i], versions );
                if ( !allowed )
                {
                    sink.text( "* " );
                    someNotAllowed = true;
                }
                if ( allowed && label != null )
                {
                    safeBold();
                }
                sink.text( artifactVersions[i].toString() );
                if ( label != null )
                {
                    if ( allowed )
                    {
                        safeBold_();
                    }
                    sink.nonBreakingSpace();
                    safeItalic();
                    sink.text( label );
                    safeItalic_();
                }
            }
            if ( someNotAllowed )
            {
                sink.lineBreak();
                sink.lineBreak();
                sink.text( "* " );
                safeItalic();
                sink.text( getText( "report.excludedVersion" ) );
                safeItalic_();
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.versionRange" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( StringUtils.isEmpty( property.getVersion() ) ? "[,)" : property.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.autoLinkDependencies" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( property.isAutoLinkDependencies() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.banSnapshots" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( property.isBanSnapshots() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.searchReactor" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( property.isSearchReactor() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.preferReactor" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( property.isPreferReactor() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();

        sink.tableRows_();
        sink.table_();
    }

    @SuppressWarnings( "checkstyle:MethodLength" )
    protected Set<String> getVersionsInRange( Property property, PropertyVersions versions,
                                              ArtifactVersion[] artifactVersions )
    {
        VersionRange range;
        Set<String> rangeVersions = new HashSet<>();
        ArtifactVersion[] tmp;
        if ( property.getVersion() != null )
        {
            try
            {
                range = VersionRange.createFromVersionSpec( property.getVersion() );
                tmp = versions.getAllUpdates( range );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                tmp = artifactVersions;
            }
        }
        else
        {
            tmp = artifactVersions;
        }
        for ( ArtifactVersion artifactVersion : tmp )
        {
            rangeVersions.add( artifactVersion.toString() );
        }
        return rangeVersions;
    }

    @Override
    protected void renderSummaryTableHeader( boolean hasScope, boolean hasType )
    {
        renderTableHeaderCells( "report.status", "report.property", "report.currentVersion",
                "report.latestSubIncremental", "report.latestIncremental", "report.latestMinor",
                "report.latestMajor" );
    }

    @Override
    protected OverviewStats computeOverviewStats()
    {
        OverviewStats stats = new OverviewStats();
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
        } );
        return stats;
    }

    private void renderPropertyDetail( Property property, PropertyVersions versions )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( "${" + property.getName() + "}" );
        sink.sectionTitle2_();
        renderPropertyDetailTable( property, versions );
        sink.section2_();
    }

}
