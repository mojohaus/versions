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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.parser.Parser;
import org.apache.maven.model.Dependency;
import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.plexus.i18n.I18N;
import org.codehaus.plexus.util.StringUtils;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

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
        sink.figure();
        sink.figureGraphics( "images/icon_warning_sml.gif" );
        sink.figure_();
    }

    protected void renderSuccessIcon()
    {
        sink.figure();
        sink.figureGraphics( "images/icon_success_sml.gif" );
        sink.figure_();
    }

    protected boolean equals( ArtifactVersion v1, ArtifactVersion v2 )
    {
        return v1 == v2 || ( v1 != null && v1.equals( v2 ) ) ||
            ( v1 != null && v2 != null && v1.toString().equals( v2.toString() ) );
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
        if ( details.getAllUpdates( UpdateScope.ANY ).length == 0 )
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
        if ( details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            sink.bold();
            sink.text( details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            sink.bold();
            sink.text( details.getOldestUpdate( UpdateScope.INCREMENTAL ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            sink.bold();
            sink.text( details.getOldestUpdate( UpdateScope.MINOR ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getOldestUpdate( UpdateScope.MAJOR ) != null )
        {
            sink.bold();
            sink.text( details.getOldestUpdate( UpdateScope.MAJOR ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableRow_();
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
        sink.tableRow_();
    }

    protected void renderDependencyDetailTable( Dependency dependency, ArtifactVersions details )
    {
        renderDependencyDetailTable( dependency, details, true, true, true );
    }

    protected void renderDependencyDetailTable( Dependency dependency, ArtifactVersions details, boolean includeScope,
                                                boolean includeClassifier, boolean includeType )
    {
        final String cellWidth = "80%";
        final String headerWidth = "20%";
        sink.table();
        sink.tableRows( new int[]{Parser.JUSTIFY_RIGHT, Parser.JUSTIFY_LEFT}, false );
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        ArtifactVersion[] versions = details.getAllUpdates( UpdateScope.ANY );
        if ( details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( details.getOldestUpdate( UpdateScope.MAJOR ) != null )
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
        sink.text( dependency.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( dependency.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( dependency.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( includeScope )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerWidth );
            sink.text( getText( "report.scope" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellWidth );
            sink.text( dependency.getScope() );
            sink.tableCell_();
            sink.tableRow_();
        }
        if ( includeClassifier )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerWidth );
            sink.text( getText( "report.classifier" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellWidth );
            sink.text( dependency.getClassifier() );
            sink.tableCell_();
            sink.tableRow_();
        }
        if ( includeType )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerWidth );
            sink.text( getText( "report.type" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellWidth );
            sink.text( dependency.getType() );
            sink.tableCell_();
            sink.tableRow_();
        }
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
                boolean bold = equals( versions[i], details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) ||
                    equals( versions[i], details.getOldestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( versions[i], details.getNewestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( versions[i], details.getOldestUpdate( UpdateScope.MINOR ) ) ||
                    equals( versions[i], details.getNewestUpdate( UpdateScope.MINOR ) ) ||
                    equals( versions[i], details.getOldestUpdate( UpdateScope.MAJOR ) ) ||
                    equals( versions[i], details.getNewestUpdate( UpdateScope.MAJOR ) );
                if ( bold )
                {
                    sink.bold();
                }
                sink.text( versions[i].toString() );
                if ( bold )
                {
                    sink.bold_();
                    sink.nonBreakingSpace();
                    sink.italic();
                    if ( equals( versions[i], details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextVersion" ) );
                    }
                    else if ( equals( versions[i], details.getOldestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextIncremental" ) );
                    }
                    else if ( equals( versions[i], details.getNewestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.latestIncremental" ) );
                    }
                    else if ( equals( versions[i], details.getOldestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.nextMinor" ) );
                    }
                    else if ( equals( versions[i], details.getNewestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.latestMinor" ) );
                    }
                    else if ( equals( versions[i], details.getOldestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.nextMajor" ) );
                    }
                    else if ( equals( versions[i], details.getNewestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.latestMajor" ) );
                    }

                    sink.italic_();
                }
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRows_();
        sink.table_();
    }

    protected void renderDependencySummaryTable( Map map )
    {
        renderDependencySummaryTable( map, true, true, true );
    }

    protected void renderDependencySummaryTable( Map map, boolean includeScope, boolean includeClassifier,
                                                 boolean includeType )
    {
        sink.table();
        renderDependencySummaryTableHeader( includeScope, includeClassifier, includeType );
        for ( Iterator i = map.entrySet().iterator(); i.hasNext(); )
        {
            Map.Entry entry = (Map.Entry) i.next();
            renderDependencySummaryTableRow( (Dependency) entry.getKey(), (ArtifactVersions) entry.getValue(),
                                             includeScope, includeClassifier, includeType );
        }
        renderDependencySummaryTableHeader( includeScope, includeClassifier, includeType );
        sink.table_();
    }

    protected void renderPropertySummaryTable( Map map )
    {
        sink.table();
        renderPropertySummaryTableHeader();
        for ( Iterator i = map.entrySet().iterator(); i.hasNext(); )
        {
            Map.Entry entry = (Map.Entry) i.next();
            renderPropertySummaryTableRow( (Property) entry.getKey(), (PropertyVersions) entry.getValue() );
        }
        renderPropertySummaryTableHeader();
        sink.table_();
    }

    protected void renderPropertySummaryTableRow( Property property, PropertyVersions versions )
    {
        sink.tableRow();
        sink.tableCell();
        if ( versions.getAllUpdates( UpdateScope.ANY ).length == 0 )
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
        if ( versions.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            sink.bold();
            sink.text( versions.getOldestUpdate( UpdateScope.SUBINCREMENTAL ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            sink.bold();
            sink.text( versions.getOldestUpdate( UpdateScope.INCREMENTAL ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            sink.bold();
            sink.text( versions.getOldestUpdate( UpdateScope.MINOR ).toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( versions.getOldestUpdate( UpdateScope.MAJOR ) != null )
        {
            sink.bold();
            sink.text( versions.getOldestUpdate( UpdateScope.MAJOR ).toString() );
            sink.bold_();
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
        sink.tableRow_();
    }

    protected void renderPropertyDetailTable( Property property, PropertyVersions versions )
    {
        final String cellWidth = "80%";
        final String headerWidth = "20%";
        sink.table();
        sink.tableRows( new int[]{Parser.JUSTIFY_RIGHT, Parser.JUSTIFY_LEFT}, false );
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.status" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        VersionRange range = null;
        ArtifactVersion[] artifactVersions = versions.getAllUpdates( UpdateScope.ANY );
        Set/*<String>*/ rangeVersions = getVersionsInRange( property, versions, artifactVersions );
        if ( versions.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.otherUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.incrementalUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( UpdateScope.MINOR ) != null )
        {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text( getText( "report.minorUpdatesAvailable" ) );
        }
        else if ( versions.getOldestUpdate( UpdateScope.MAJOR ) != null )
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
        sink.text( getText( "report.property" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( "${" + property.getName() + "}" );
        sink.tableCell_();
        sink.tableRow_();

        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.associations" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
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
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( versions.getCurrentVersion().toString() );
        sink.tableCell_();
        sink.tableRow_();
        if ( artifactVersions.length > 0 )
        {
            sink.tableRow();
            sink.tableHeaderCell( headerWidth );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell( cellWidth );
            boolean someNotAllowed = false;
            for ( int i = 0; i < artifactVersions.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.lineBreak();
                }
                boolean allowed = ( rangeVersions.contains( artifactVersions[i].toString() ) );
                boolean bold = equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) ||
                    equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.INCREMENTAL ) ) ||
                    equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.MINOR ) ) ||
                    equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.MINOR ) ) ||
                    equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.MAJOR ) ) ||
                    equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.MAJOR ) );
                if ( !allowed )
                {
                    sink.text( "* " );
                    someNotAllowed = true;
                }
                if ( allowed && bold )
                {
                    sink.bold();
                }
                sink.text( artifactVersions[i].toString() );
                if ( bold )
                {
                    if ( allowed )
                    {
                        sink.bold_();
                    }
                    sink.nonBreakingSpace();
                    sink.italic();
                    if ( equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextVersion" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.nextIncremental" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.INCREMENTAL ) ) )
                    {
                        sink.text( getText( "report.latestIncremental" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.nextMinor" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.MINOR ) ) )
                    {
                        sink.text( getText( "report.latestMinor" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getOldestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.nextMajor" ) );
                    }
                    else if ( equals( artifactVersions[i], versions.getNewestUpdate( UpdateScope.MAJOR ) ) )
                    {
                        sink.text( getText( "report.latestMajor" ) );
                    }

                    sink.italic_();
                }
            }
            if ( someNotAllowed )
            {
                sink.lineBreak();
                sink.lineBreak();
                sink.text( "* " );
                sink.italic();
                sink.text( getText( "report.excludedVersion" ) );
                sink.italic_();
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.versionRange" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( StringUtils.isEmpty( property.getVersion() ) ? "[,)" : property.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.autoLinkDependencies" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( property.isAutoLinkDependencies() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.banSnapshots" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( property.isBanSnapshots() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.searchReactor" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( property.isSearchReactor() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( headerWidth );
        sink.text( getText( "report.preferReactor" ) );
        sink.tableHeaderCell_();
        sink.tableCell( cellWidth );
        sink.text( property.isPreferReactor() ? getText( "report.yes" ) : getText( "report.no" ) );
        sink.tableCell_();
        sink.tableRow_();

        sink.tableRows_();
        sink.table_();
    }

    private Set getVersionsInRange( Property property, PropertyVersions versions, ArtifactVersion[] artifactVersions )
    {
        VersionRange range;
        Set/*<String>*/ rangeVersions = new HashSet();
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
        for ( int i = 0; i < tmp.length; i++ )
        {
            rangeVersions.add( tmp[i].toString() );
        }
        return rangeVersions;
    }

}
