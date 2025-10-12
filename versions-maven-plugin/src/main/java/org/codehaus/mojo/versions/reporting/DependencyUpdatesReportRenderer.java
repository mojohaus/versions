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

/**
 * A renderer for Dependency updates reports, using a {@link DependencyUpdatesModel}.
 * @param <K> type of the model
 * @since 1.0-beta-1
 */
public class DependencyUpdatesReportRenderer<K extends DependencyUpdatesModel>
        extends AbstractVersionsReportRenderer<K> {
    /**
     * Creates a new instance.
     * @param i18n an {@link I18N} instance
     * @param sink the {@link Sink} to render to
     * @param locale the locale to render in
     * @param bundleName the resource bundle name to use
     * @param model object containing the updates model
     * @param allowSnapshots whether snapshots should be included
     */
    public DependencyUpdatesReportRenderer(
            I18N i18n, Sink sink, Locale locale, String bundleName, K model, boolean allowSnapshots) {
        super(i18n, sink, locale, bundleName, model, allowSnapshots);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails() {
        model.getAllUpdates().forEach(this::renderDependencyDetail);
    }

    @Override
    protected void renderSummaryTable() {
        renderTable("report.overview.dependency", model.getArtifactUpdates(), "report.overview.noDependency");
    }

    @Override
    protected void renderManagementSummaryTable() {
        renderTable(
                "report.overview.dependencyManagement",
                model.getArtifactManagementUpdates(),
                "report.overview.noDependencyManagement");
    }

    /**
     * Renders a table with the given title and contents. If the contents are empty, a message is rendered instead.
     *
     * @param titleKey the resource bundle key for the title of the table
     * @param contents the contents of the table
     * @param emptyKey the resource bundle key for the message to render if the contents are empty
     */
    protected void renderTable(String titleKey, Map<Dependency, ArtifactVersions> contents, String emptyKey) {
        startSection(getText(titleKey));

        if (contents.isEmpty()) {
            paragraph(getText(emptyKey));
        } else {
            renderSummaryTable(contents, true);
        }
        endSection();
    }

    @Override
    protected OverviewStats computeOverviewStats() {
        return OverviewStats.fromUpdates(model.getAllUpdates().values(), newestUpdateCache, isAllowSnapshots());
    }

    /**
     * Renders the details for a given dependency. If there are no updates, nothing is rendered.
     *
     * @param artifact the dependency
     * @param details  the details of the dependency
     */
    protected void renderDependencyDetail(Dependency artifact, ArtifactVersions details) {
        startSection(ArtifactUtils.versionlessKey(artifact.getGroupId(), artifact.getArtifactId()));
        renderDependencyDetailTable(artifact, details, true);
        endSection();
    }
}
