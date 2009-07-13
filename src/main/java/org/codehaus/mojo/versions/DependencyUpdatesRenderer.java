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

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.plexus.i18n.I18N;

import java.util.Locale;
import java.util.Map;
import java.util.Iterator;
import java.util.List;

/**
 * @since 1.0-beta-1
 */
public class DependencyUpdatesRenderer
    extends AbstractVersionsReportRenderer
{

    private final Map/*<Dependency,List<ArtifactVersion>*/ dependencyUpdates;
    
    public DependencyUpdatesRenderer( Sink sink, I18N i18n, String bundleName, Locale locale,
                                      Map/*<Dependency,List<ArtifactVersion>*/ dependencyUpdates )
    {
        super( sink, bundleName, i18n, locale );
        this.dependencyUpdates = dependencyUpdates;
    }

    protected void renderBody()
    {
        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ));
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText("report.overview.text"));
        sink.paragraph_();

        // Loop over all the artifacts (they're already sorted), printing each
        sink.table();
        renderHeader();
        for ( Iterator it = dependencyUpdates.entrySet().iterator(); it.hasNext(); )
        {
            final Map.Entry/*<Dependency,List<ArtifactVersion>*/ entry = (Map.Entry) it.next();
            Dependency dependency = (Dependency) entry.getKey();
            List/*<ArtifactVersion>*/ versions = (List) entry.getValue();
            renderDependency( dependency, versions );
        }
        renderHeader();
        sink.table_();
        sink.section1_();
    }

    private void renderDependency( Dependency dependency, List versions )
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

        // Cut down on excessive coloring by comparing each column to the next
        sink.tableCell();
        boolean first = true;
        for (Iterator i = versions.iterator(); i.hasNext(); ) {
            ArtifactVersion version = (ArtifactVersion) i.next();
            if (first)  {
                first = false;
            } else {
                sink.text( ", " );
            }
            sink.text( version.toString() );
        }
        sink.tableCell_();
        sink.tableRow_();
        //To change body of created methods use File | Settings | File Templates.
    }

    private void renderHeader()
    {
        sink.tableRow();
        sink.tableHeaderCell();
        sink.text( getText("report.groupId" ));
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText("report.artifactId") );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText("report.currentVersion") );
        sink.tableHeaderCell_();
        sink.tableHeaderCell();
        sink.text( getText("report.newerVersions") );
        sink.tableHeaderCell_();
        sink.tableRow_();
    }


}
