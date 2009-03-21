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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.codehaus.mojo.versions.DisplayDependencyUpdatesReport.MultiVersionSummary;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;

/**
 * This class renders the version information gathered by DisplayDependencyUpdatesReport.
 *
 * @author Matthew Beermann <matthew.beermann@cerner.com>
 */
public class DisplayDependencyUpdatesRenderer
    extends AbstractMavenReportRenderer
{
    private final Map availableVersions;

    private final Comparator comparator;

    /**
     * Create a new renderer with the given sink, version list, and comparator.
     */
    public DisplayDependencyUpdatesRenderer( Sink sink, Map availableVersions, Comparator comparator )
    {
        super( (org.codehaus.doxia.sink.Sink) sink );
        this.availableVersions = availableVersions;
        this.comparator = comparator;
    }

    public String getTitle()
    {
        return "Dependency ArtifactVersions";
    }

    protected void renderBody()
    {
        sink.section1();
        sink.sectionTitle1();
        sink.text( "Overview" );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text(
            "This report summarizes newer versions that may be available for your project's various dependencies. " );
        sink.text(
            "Incremental updates (typically passive) are noted in green, minor updates (sometimes passive) are noted in yellow, and major updates (rarely passive) are noted in red. " );
        sink.text(
            "Dependencies whose available updates could not be reliably determined appear as \"?\", and should be checked manually." );
        sink.paragraph_();

        // Loop over all the artifacts (they're already sorted), printing each
        sink.table();
        renderHeader();
        for ( Iterator it = availableVersions.values().iterator(); it.hasNext(); )
        {
            MultiVersionSummary summary = (MultiVersionSummary) it.next();
            renderArtifact( summary );
        }
        renderHeader();
        sink.table_();
        sink.section1_();
    }

    private void renderHeader()
    {
        sink.tableRow();
        sink.tableHeaderCell();
        sink.text( "Group ID" );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( "Artifact ID" );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( "Version" );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( "Latest Incremental" );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( "Latest Minor" );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( "Latest Major" );
        sink.tableHeaderCell_();
        sink.tableRow_();
    }

    private void renderArtifact( MultiVersionSummary summary )
    {
        Artifact artifact = summary.getArtifact();
        sink.tableRow();
        sink.tableCell();
        sink.text( artifact.getGroupId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( artifact.getArtifactId() );
        sink.tableCell_();
        sink.tableCell();
        sink.text( artifact.getVersion() );
        sink.tableCell_();

        // Cut down on excessive coloring by comparing each column to the next
        renderVersion( summary.getCurrentVersion(), summary.getLatestIncremental(), "lightgreen" );
        renderVersion( summary.getLatestIncremental(), summary.getLatestMinor(), "yellow" );
        renderVersion( summary.getLatestMinor(), summary.getLatestMajor(), "red" );
        sink.tableRow_();
    }

    private void renderVersion( ArtifactVersion currentVersion, ArtifactVersion comparisonVersion, String color )
    {
        // Ignore this whole cell if the left or right side of the comparison is null,
        // or if it's a version number that Maven couldn't parse properly anyway
        if ( currentVersion == null || comparisonVersion == null || comparisonVersion.toString().equals(
            comparisonVersion.getQualifier() ) )
        {
            sink.tableCell();
            sink.text( "?" );
            sink.tableCell_();
        }
        // Print the version in plain text if it's not newer
        else if ( comparator.compare( currentVersion, comparisonVersion ) >= 0 )
        {
            sink.tableCell();
            sink.text( comparisonVersion.toString() );
            sink.tableCell_();
        }
        // If we got this far, print the cell with a highlighted color
        else
        {
            sink.rawText( "<td style=\"background-color: " + color + "\">" );
            sink.text( comparisonVersion.toString() );
            sink.rawText( "</td>" );
        }
    }
}
