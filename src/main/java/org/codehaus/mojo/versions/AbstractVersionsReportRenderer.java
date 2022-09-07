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
import org.apache.maven.model.Dependency;
import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.plexus.i18n.I18N;
import org.codehaus.plexus.util.StringUtils;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Base class for report renderers.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionsReportRenderer
    extends AbstractMavenReportRenderer
{
    /**
     * Internationalization component.
     *
     * @since 1.0-beta-1
     */
    protected final I18N i18n;

    /**
     * The locale we are rendering for.
     *
     * @since 1.0-beta-1
     */
    protected final Locale locale;

    /**
     * The name of the bundle containing our I18n resources.
     *
     * @since 1.0-beta-1
     */
    protected final String bundleName;

    public AbstractVersionsReportRenderer( org.apache.maven.doxia.sink.Sink sink, String bundleName, I18N i18n,
                                           Locale locale )
    {
        super( sink );
        this.bundleName = bundleName;
        this.i18n = i18n;
        this.locale = locale;
    }

    public String getTitle()
    {
        return getText( "report.title" );
    }

    /**
     * Gets the localized message for this report.
     *
     * @param key the message key.
     * @return the message.
     */
    public String getText( String key )
    {
        return i18n.getString( bundleName, locale, key );
    }

    protected void renderWarningIcon()
    {
        sink.figureGraphics( "images/icon_warning_sml.gif" );
    }

    protected void renderSuccessIcon()
    {
        sink.figureGraphics( "images/icon_success_sml.gif" );
    }

    protected boolean equals( ArtifactVersion v1, ArtifactVersion v2 )
    {
        return v1 == v2 || ( v1 != null && v1.equals( v2 ) )
            || ( v1 != null && v2 != null && v1.toString().equals( v2.toString() ) );
    }

    protected void renderDependencySummaryTableRow( Dependency dependency, ArtifactVersions details )
    {
        renderDependencySummaryTableRow( dependency, details, true, true, true );
    }

    protected void renderDependencySummaryTableRow( Dependency dependency, ArtifactVersions details,
                                                    boolean includeScope, boolean includeClassifier,
                                                    boolean includeType )
    {
        sink.tableRow();
        sink.tableCell();
        ArtifactVersion[] allUpdates = details.getAllUpdates( empty() );
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
        sink.text( dependency.getGroupId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( dependency.getArtifactId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( dependency.getVersion() );
        sink.tableCell_();
        if ( includeScope )
        {
            sink.tableCell();
            sink.text( dependency.getScope() );
            sink.tableCell_();
        }
        if ( includeClassifier )
        {
            sink.tableCell();
            sink.text( dependency.getClassifier() );
            sink.tableCell_();
        }
        if ( includeType )
        {
            sink.tableCell();
            sink.text( dependency.getType() );
            sink.tableCell_();
        }

        sink.tableCell();
        if ( details.getNewestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( details.getNewestUpdate( of( SUBINCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getNewestUpdate( of( INCREMENTAL ) ) != null )
        {
            safeBold();
            sink.text( details.getNewestUpdate( of( INCREMENTAL ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getNewestUpdate( of( MINOR ) ) != null )
        {
            safeBold();
            sink.text( details.getNewestUpdate( of( MINOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getNewestUpdate( of( MAJOR ) ) != null )
        {
            safeBold();
            sink.text( details.getNewestUpdate( of( MAJOR ) ).toString() );
            safeBold_();
        }
        sink.tableCell_();

        sink.tableRow_();
    }

    protected void safeBold()
    {
        try
        {
            sink.bold();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    @SuppressWarnings( "checkstyle:MethodName" )
    protected void safeBold_()
    {
        try
        {
            sink.bold_();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    protected void safeItalic()
    {
        try
        {
            sink.italic();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    @SuppressWarnings( "checkstyle:MethodName" )
    protected void safeItalic_()
    {
        try
        {
            sink.italic_();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    protected void renderDependencySummaryTableHeader()
    {
        renderDependencySummaryTableHeader( true, true, true );
    }

    protected void renderDependencySummaryTableHeader( boolean includeScope, boolean includeClassifier,
                                                       boolean includeType )
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
        if ( includeScope )
        {
            sink.tableHeaderCell();
            sink.text( getText( "report.scope" ) );
            sink.tableHeaderCell_();
        }
        if ( includeClassifier )
        {
            sink.tableHeaderCell();
            sink.text( getText( "report.classifier" ) );
            sink.tableHeaderCell_();
        }
        if ( includeType )
        {
            sink.tableHeaderCell();
            sink.text( getText( "report.type" ) );
            sink.tableHeaderCell_();
        }
        sink.tableHeaderCell();
        sink.text( getText( "report.latestSubIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMinor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMajor" ) );
        sink.tableHeaderCell_();
        sink.tableRow_();
    }

    protected void renderDependencyDetailTable( Dependency dependency, ArtifactVersions details )
    {
        renderDependencyDetailTable( dependency, details, true, true, true );
    }

    @SuppressWarnings( "checkstyle:MethodLength" )
    protected void renderDependencyDetailTable( Dependency dependency, ArtifactVersions details, boolean includeScope,
                                                boolean includeClassifier, boolean includeType )
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
        ArtifactVersion[] versions = details.getAllUpdates( empty() );
        if ( details.getOldestUpdate( of( SUBINCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( of( INCREMENTAL ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( of( MINOR ) ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( of( MAJOR ) ) != null )
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
        sink.text( dependency.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( dependency.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerAttributes );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellAttributes );
        sink.text( dependency.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( includeScope )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.scope" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            sink.text( dependency.getScope() );
            sink.tableCell_();
            sink.tableRow_();
        }
        if ( includeClassifier )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.classifier" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            sink.text( dependency.getClassifier() );
            sink.tableCell_();
            sink.tableRow_();
        }
        if ( includeType )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerAttributes );
            sink.text( getText( "report.type" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellAttributes );
            sink.text( dependency.getType() );
            sink.tableCell_();
            sink.tableRow_();
        }
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

    protected void renderDependencySummaryTable( Map<Dependency, ArtifactVersions> map )
    {
        renderDependencySummaryTable( map, true, true, true );
    }

    protected void renderDependencySummaryTable( Map<Dependency, ArtifactVersions> map, boolean includeScope,
                                                 boolean includeClassifier, boolean includeType )
    {
        sink.table();
        renderDependencySummaryTableHeader( includeScope, includeClassifier, includeType );
        for ( Map.Entry<Dependency, ArtifactVersions> entry : map.entrySet() )
        {
            renderDependencySummaryTableRow( entry.getKey(), entry.getValue(), includeScope, includeClassifier,
                                             includeType );
        }
        renderDependencySummaryTableHeader( includeScope, includeClassifier, includeType );
        sink.table_();
    }

    protected void renderPropertySummaryTable( Map<Property, PropertyVersions> map )
    {
        sink.table();
        renderPropertySummaryTableHeader();
        for ( Map.Entry<Property, PropertyVersions> entry : map.entrySet() )
        {
            renderPropertySummaryTableRow( entry.getKey(), entry.getValue() );
        }
        renderPropertySummaryTableHeader();
        sink.table_();
    }

    protected void renderPropertySummaryTableRow( Property property, PropertyVersions versions )
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

    protected void renderPropertySummaryTableHeader()
    {
        sink.tableRow();
        sink.tableHeaderCell();
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.property" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestSubIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMinor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMajor" ) );
        sink.tableHeaderCell_();
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

    private Set<String> getVersionsInRange( Property property, PropertyVersions versions,
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

    protected String getLabel( ArtifactVersion version, AbstractVersionDetails versions )
    {
        String label = null;
        if ( equals( version, versions.getNewestUpdate( of( MAJOR ) ) ) )
        {
            label = getText( "report.latestMajor" );
        }
        else if ( equals( version, versions.getOldestUpdate( of( MAJOR ) ) ) )
        {
            label = getText( "report.nextMajor" );
        }
        else if ( equals( version, versions.getNewestUpdate( of( MINOR ) ) ) )
        {
            label = getText( "report.latestMinor" );
        }
        else if ( equals( version, versions.getOldestUpdate( of( MINOR ) ) ) )
        {
            label = getText( "report.nextMinor" );
        }
        else if ( equals( version, versions.getNewestUpdate( of( INCREMENTAL ) ) ) )
        {
            label = getText( "report.latestIncremental" );
        }
        else if ( equals( version, versions.getOldestUpdate( of( INCREMENTAL ) ) ) )
        {
            label = getText( "report.nextIncremental" );
        }
        else if ( equals( version, versions.getNewestUpdate( of( SUBINCREMENTAL ) ) ) )
        {
            label = getText( "report.latestSubIncremental" );
        }
        else if ( equals( version, versions.getOldestUpdate( of( SUBINCREMENTAL ) ) ) )
        {
            label = getText( "report.nextVersion" );
        }
        return label;
    }

}
