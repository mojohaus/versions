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
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.codehaus.plexus.i18n.I18N;

import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

/**
 * @since 1.0-beta-1
 */
public class DependencyUpdatesRenderer
    extends AbstractVersionsReportRenderer
{

    private final Map/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ dependencyUpdates;

    public DependencyUpdatesRenderer( Sink sink, I18N i18n, String bundleName, Locale locale,
                                      Map/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ dependencyUpdates )
    {
        super( sink, bundleName, i18n, locale );
        this.dependencyUpdates = dependencyUpdates;
    }

    protected void renderBody()
    {
        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.overview.text" ) );
        sink.paragraph_();

        // Loop over all the artifacts (they're already sorted), printing each
        sink.table();
        renderHeader();
        for ( Iterator it = dependencyUpdates.entrySet().iterator(); it.hasNext(); )
        {
            final Map.Entry/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ entry =
                (Map.Entry) it.next();
            Dependency dependency = (Dependency) entry.getKey();
            ArtifactUpdatesDetails details = (ArtifactUpdatesDetails) entry.getValue();
            if ( details.getAll().length == 0 )
            {
                continue;
            }
            renderDependencySummary( dependency, details );
        }
        renderHeader();
        sink.table_();
        sink.section1_();

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.detail.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.detail.text" ) );
        sink.paragraph_();
        for ( Iterator it = dependencyUpdates.entrySet().iterator(); it.hasNext(); )
        {
            final Map.Entry/*<Dependency,DependencyUpdatesReport.DependencyUpdateDetails>*/ entry =
                (Map.Entry) it.next();
            Dependency dependency = (Dependency) entry.getKey();
            ArtifactUpdatesDetails details = (ArtifactUpdatesDetails) entry.getValue();
            renderDependencyDetail( dependency, details );
        }
        sink.section1_();
    }

    private void renderDependencyDetail( Dependency dependency, ArtifactUpdatesDetails details )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( ArtifactUtils.versionlessKey( dependency.getGroupId(), dependency.getArtifactId() ) );
        sink.sectionTitle2_();
        ArtifactVersion[] versions = details.getAll();
        if ( versions.length == 0 )
        {
            sink.paragraph();
            sink.text( getText( "report.noUpdatesAvailable" ) );
            sink.paragraph_();
        }
        else
        {
            if ( details.getNextIncremental() != null )
            {
                sink.paragraph();
                sink.text( getText( "report.incrementalUpdatesAvailable" ) );
                sink.paragraph_();
            }
            if ( details.getNextMinor() != null )
            {
                sink.paragraph();
                sink.text( getText( "report.minorUpdatesAvailable" ) );
                sink.paragraph_();
            }
            if ( details.getNextMajor() != null )
            {
                sink.paragraph();
                sink.text( getText( "report.majorUpdatesAvailable" ) );
                sink.paragraph_();
            }
        }
        sink.table();
        sink.tableRow();
        sink.tableHeaderCell( "20%" );
        sink.text( getText( "report.groupId" ) );
        sink.tableHeaderCell_();
        sink.tableCell();
        sink.text( dependency.getGroupId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( "20%" );
        sink.text( getText( "report.artifactId" ) );
        sink.tableHeaderCell_();
        sink.tableCell();
        sink.text( dependency.getArtifactId() );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableHeaderCell( "20%" );
        sink.text( getText( "report.currentVersion" ) );
        sink.tableHeaderCell_();
        sink.tableCell();
        sink.text( dependency.getVersion() );
        sink.tableCell_();
        sink.tableRow_();
        if ( versions.length > 0 )
        {
            sink.tableRow();
            sink.tableHeaderCell( "20%" );
            sink.text( getText( "report.updateVersions" ) );
            sink.tableHeaderCell_();
            sink.tableCell();
            for ( int i = 0; i < versions.length; i++ )
            {
                if ( i > 0 )
                {
                    sink.text( ", " );
                }
                boolean bold = equals( versions[i], details.getNextIncremental() )
                    || equals( versions[i], details.getLatestIncremental() )
                    || equals( versions[i], details.getNextMinor() ) || equals( versions[i], details.getLatestMinor() )
                    || equals( versions[i], details.getNextMajor() ) || equals( versions[i], details.getLatestMajor() );
                if ( bold )
                {
                    sink.bold();
                }
                sink.text( versions[i].toString() );
                if ( bold )
                {
                    sink.bold_();
                }
            }
            sink.tableCell_();
            sink.tableRow_();
        }
        sink.table_();
        sink.section2_();
    }

    private void renderDependencySummary( Dependency dependency, ArtifactUpdatesDetails details )
    {
        sink.tableRow();
        sink.tableCell();
        sink.text( dependency.getGroupId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( dependency.getArtifactId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( dependency.getVersion() );
        sink.tableCell_();

        sink.tableCell();
        ArtifactVersion prev = null;
        if ( details.getNextIncremental() != null )
        {
            sink.bold();
            sink.text( details.getNextIncremental().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getLatestIncremental() != null )
        {
            if ( !equals( details.getLatestIncremental(), details.getNextIncremental() ) )
            {
                sink.bold();
            }
            sink.text( details.getLatestIncremental().toString() );
            if ( !equals( details.getLatestIncremental(), details.getNextIncremental() ) )
            {
                sink.bold_();
            }
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getNextMinor() != null )
        {
            sink.bold();
            sink.text( details.getNextMinor().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getLatestMinor() != null )
        {
            if ( !equals( details.getLatestMinor(), details.getNextMinor() ) )
            {
                sink.bold();
            }
            sink.text( details.getLatestMinor().toString() );
            if ( !equals( details.getLatestMinor(), details.getNextMinor() ) )
            {
                sink.bold_();
            }
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getNextMajor() != null )
        {
            sink.bold();
            sink.text( details.getNextMajor().toString() );
            sink.bold_();
        }
        sink.tableCell_();

        sink.tableCell();
        if ( details.getLatestMajor() != null )
        {
            if ( !equals( details.getLatestMajor(), details.getNextMajor() ) )
            {
                sink.bold();
            }
            sink.text( details.getLatestMajor().toString() );
            if ( !equals( details.getLatestMajor(), details.getNextMajor() ) )
            {
                sink.bold_();
            }
        }
        sink.tableCell_();

        sink.tableRow_();
    }

    private boolean equals( ArtifactVersion v1, ArtifactVersion v2 )
    {
        return v1 == v2 || ( v1 != null && v1.equals( v2 ) ) || ( v1 != null && v2 != null && v1.toString().equals(
            v2.toString() ) );
    }

    private void renderHeader()
    {
        sink.tableRow();
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
        sink.text( getText( "report.nextIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestIncremental" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextMinor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMinor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.nextMajor" ) );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText( "report.latestMajor" ) );
        sink.tableHeaderCell_();
        sink.tableRow_();
    }


}
