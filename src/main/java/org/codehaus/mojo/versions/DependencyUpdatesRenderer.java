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
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.i18n.I18N;

import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * @since 1.0-beta-1
 */
public class DependencyUpdatesRenderer
    extends AbstractVersionsReportRenderer
{

    private final Map/*<Dependency,ArtifactUpdateDetails>*/ dependencyUpdates;

    private final Map/*<Dependency,ArtifactUpdateDetails>*/ dependencyManagementUpdates;

    public DependencyUpdatesRenderer( Sink sink, I18N i18n, String bundleName, Locale locale,
                                      Map/*<Dependency,ArtifactUpdateDetails>*/ dependencyUpdates,
                                      Map/*<Dependency,ArtifactUpdateDetails>*/ dependencyManagementUpdates )
    {
        super( sink, bundleName, i18n, locale );
        this.dependencyUpdates = dependencyUpdates;
        this.dependencyManagementUpdates = dependencyManagementUpdates;
    }


    protected void renderBody()
    {
        Map allUpdates = new TreeMap( new DependencyComparator() );
        allUpdates.putAll( dependencyManagementUpdates );
        allUpdates.putAll( dependencyUpdates );

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.overview.text" ) );
        sink.paragraph_();

        renderSummaryTotalsTable( allUpdates );

        renderSummaryTable( "report.overview.dependencyManagement", dependencyManagementUpdates,
                            "report.overview.noDependencyManagement" );

        renderSummaryTable( "report.overview.dependency", dependencyUpdates, "report.overview.noDependency" );

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
            renderDependencyDetail( (Dependency) entry.getKey(), (ArtifactUpdatesDetails) entry.getValue() );
        }
        sink.section1_();
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
            renderDependencySummaryTable( contents );
        }
        sink.section2_();
    }

    private void renderSummaryTotalsTable( Map allUpdates )
    {
        int numInc = 0;
        int numMin = 0;
        int numMaj = 0;
        int numAny = 0;
        int numCur = 0;
        for ( Iterator iterator = allUpdates.values().iterator(); iterator.hasNext(); )
        {
            ArtifactUpdatesDetails details = (ArtifactUpdatesDetails) iterator.next();
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
        sink.table_();
    }

    private void renderDependencyDetail( Dependency dependency, ArtifactUpdatesDetails details )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() ) );
        sink.sectionTitle2_();
        renderDependencyDetailTable( dependency, details );
        sink.section2_();
    }

}
