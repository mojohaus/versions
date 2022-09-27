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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.reporting.model.DependencyUpdatesModel;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * @param <K> type of the model
 * @since 1.0-beta-1
 */
public class DependencyUpdatesReportRenderer<K extends DependencyUpdatesModel> extends AbstractVersionsReportRenderer<K>
{
    public DependencyUpdatesReportRenderer( I18N i18n, Sink sink, Locale locale, String bundleName, K model )
    {
        super( i18n, sink, locale, bundleName, model );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails()
    {
        model.getAllUpdates().forEach( this::renderDependencyDetail );
    }

    @Override
    protected void renderSummaryTable()
    {
        renderTable( "report.overview.dependency", model.getArtifactUpdates(),
                "report.overview.noDependency" );
    }

    @Override
    protected void renderManagementSummaryTable()
    {
        renderTable( "report.overview.dependencyManagement",
                model.getArtifactManagementUpdates(), "report.overview.noDependencyManagement" );
    }

    protected void renderTable( String titleKey, Map<Dependency, ArtifactVersions> contents, String emptyKey )
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
            renderSummaryTable( contents, true );
        }
        sink.section2_();
    }

    @Override
    protected OverviewStats computeOverviewStats()
    {
        OverviewStats stats = new OverviewStats();
        model.getAllUpdates().values().forEach( details ->
        {
            if ( details.getOldestUpdate( of( SUBINCREMENTAL ) ) != null )
            {
                stats.incrementAny();
            }
            else if ( details.getOldestUpdate( of( INCREMENTAL ) ) != null )
            {
                stats.incrementIncremental();
            }
            else if ( details.getOldestUpdate( of( MINOR ) ) != null )
            {
                stats.incrementMinor();
            }
            else if ( details.getOldestUpdate( of( MAJOR ) ) != null )
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

    protected void renderDependencyDetail( Dependency artifact, ArtifactVersions details )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( ArtifactUtils.versionlessKey( artifact.getGroupId(), artifact.getArtifactId() ) );
        sink.sectionTitle2_();
        renderDependencyDetailTable( artifact, details, true );
        sink.section2_();
    }
}
