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

import java.text.MessageFormat;
import java.util.Locale;
import java.util.Map;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.empty;

/**
 * @since 1.0-beta-1
 */
public class PluginUpdatesReportRenderer extends AbstractVersionsReportRenderer<PluginUpdatesModel> {

    public PluginUpdatesReportRenderer(
            I18N i18n, Sink sink, Locale locale, String bundleName, PluginUpdatesModel model, boolean allowSnapshots) {
        super(i18n, sink, locale, bundleName, model, allowSnapshots);
    }

    @Override
    protected void renderSummaryTable() {
        renderTable("report.overview.plugin", model.getArtifactUpdates(), "report.overview.noPlugin");
    }

    protected void renderManagementSummaryTable() {
        renderTable(
                "report.overview.pluginManagement",
                model.getArtifactManagementUpdates(),
                "report.overview.noPluginManagement");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails() {
        model.getAllUpdates().forEach(this::renderPluginDetail);
    }

    private void renderDependencyDetail(Dependency dependency, ArtifactVersions details) {
        startSection(MessageFormat.format(
                getText("report.pluginDependency"),
                ArtifactUtils.versionlessKey(dependency.getGroupId(), dependency.getArtifactId())));
        renderDependencyDetailTable(dependency, details, false);
        endSection();
    }

    private void renderTable(String titleKey, Map<Dependency, PluginUpdatesDetails> contents, String emptyKey) {
        startSection(getText(titleKey));

        if (contents.isEmpty()) {
            paragraph(getText(emptyKey));
        } else {
            renderSummaryTable(contents);
        }
        endSection();
    }

    protected void renderSummaryTable(Map<Dependency, PluginUpdatesDetails> contents) {
        startTable();

        sink.tableRow();
        renderSummaryTableHeader(false, false);
        sink.tableRow_();

        contents.forEach(this::renderSummaryTableRow);

        sink.tableRow();
        renderSummaryTableHeader(false, false);
        sink.tableRow_();

        endTable();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PluginOverviewStats computeOverviewStats() {
        return PluginOverviewStats.fromUpdates(model.getAllUpdates().values(), newestUpdateCache, isAllowSnapshots());
    }

    @Override
    protected void renderSummaryTableHeader(boolean hasScope, boolean hasType) {
        super.renderSummaryTableHeader(hasScope, hasType);
        renderTableHeaderCells("report.dependencyStatus");
    }

    @Override
    protected <T extends OverviewStats> void renderOverviewTableRow(T stats) {
        super.renderOverviewTableRow(stats);
        super.renderStatRow(
                "report.overview.numNewerDependenciesAvailable",
                ((PluginOverviewStats) stats).getDependencies(),
                false);
    }

    protected void renderSummaryTableRow(Dependency artifact, PluginUpdatesDetails details) {
        boolean upToDate = !details.isUpdateAvailable();

        sink.tableRow();

        sink.tableCell();
        renderIcon(upToDate);
        sink.tableCell_();

        renderCells(artifact.getGroupId(), artifact.getArtifactId());
        renderBoldCell(upToDate, artifact.getVersion());
        renderNewestVersions(details);

        sink.tableCell();
        renderIcon(!details.isDependencyUpdateAvailable());
        sink.tableCell_();

        sink.tableRow_();
    }

    private void renderPluginDetail(Dependency artifact, PluginUpdatesDetails details) {
        startSection(MessageFormat.format(
                getText("report.plugin"), ArtifactUtils.versionlessKey(details.getGroupId(), details.getArtifactId())));
        renderPluginDetailTable(details);

        if (!details.getDependencyVersions().isEmpty()) {
            startSection(MessageFormat.format(
                    getText("report.pluginDependencies"),
                    ArtifactUtils.versionlessKey(details.getGroupId(), details.getArtifactId())));

            renderSummaryTable(details.getDependencyVersions(), false);
            endSection();

            details.getDependencyVersions().forEach(this::renderDependencyDetail);
        }
        endSection();
    }

    private void renderPluginDetailTable(PluginUpdatesDetails details) {
        // warning: using caches here might break plugin report
        ArtifactVersion[] allUpdates = details.getAllUpdates(empty(), isAllowSnapshots());
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        startTable(new int[] {Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT}, false);

        renderTwoCellsRow("report.status", () -> renderStatus(details));
        renderTwoCellsRow("report.groupId", details.getGroupId());
        renderTwoCellsRow("report.artifactId", details.getArtifactId());
        renderTwoCellsRow("report.currentVersion", details.getVersion());
        if (!upToDate) {
            renderTwoCellsRow("report.updateVersions", () -> renderVersions(allUpdates, details));
        }

        endTable();
    }
}
