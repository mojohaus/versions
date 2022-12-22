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
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.reporting.model.PropertyUpdatesModel;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.empty;

/**
 * @since 1.0-beta-1
 */
public class PropertyUpdatesReportRenderer extends AbstractVersionsReportRenderer<PropertyUpdatesModel> {
    public PropertyUpdatesReportRenderer(
            I18N i18n,
            Sink sink,
            Locale locale,
            String bundleName,
            PropertyUpdatesModel model,
            boolean allowSnapshots) {
        super(i18n, sink, locale, bundleName, model, allowSnapshots);
    }

    @Override
    protected void renderManagementSummaryTable() {}

    @Override
    protected void renderSummaryTable() {
        renderTable("report.overview.property", model.getAllUpdates(), "report.overview.noProperty");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void renderDetails() {
        model.getAllUpdates().forEach(this::renderPropertyDetail);
    }

    protected void renderTable(String titleKey, Map<Property, PropertyVersions> contents, String emptyKey) {
        sink.section2();
        sink.sectionTitle2();
        sink.text(getText(titleKey));
        sink.sectionTitle2_();

        if (contents.isEmpty()) {
            sink.paragraph();
            sink.text(getText(emptyKey));
            sink.paragraph_();
        } else {
            renderSummaryTable(contents);
        }
        sink.section2_();
    }

    protected void renderSummaryTable(Map<Property, PropertyVersions> contents) {
        sink.table();

        sink.tableRow();
        renderSummaryTableHeader(false, false);
        sink.tableRow_();

        contents.forEach(this::renderPropertySummaryTableRow);

        sink.tableRow();
        renderSummaryTableHeader(false, false);
        sink.tableRow_();

        sink.table_();
    }

    private void renderPropertySummaryTableRow(Property property, PropertyVersions details) {
        ArtifactVersion[] allUpdates = allUpdatesCache.get(details, empty(), isAllowSnapshots());
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        sink.tableRow();

        sink.tableCell();
        renderIcon(upToDate);
        sink.tableCell_();

        renderCells("${" + property.getName() + "}", details.getCurrentVersion());
        renderNewestVersions(details);

        sink.tableRow_();
    }

    protected void renderPropertyDetailTable(Property property, PropertyVersions details) {
        ArtifactVersion[] allUpdates = allUpdatesCache.get(details, empty(), isAllowSnapshots());
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        sink.table();
        sink.tableRows(new int[] {Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT}, false);

        renderTwoCellsRow("report.status", () -> renderStatus(details));
        renderTwoCellsRow("report.property", "${" + property.getName() + "}");
        renderTwoCellsRow("report.associations", () -> renderAssociations(details));
        renderTwoCellsRow("report.currentVersion", details.getCurrentVersion().toString());
        if (!upToDate) {
            renderTwoCellsRow("report.updateVersions", () -> renderVersions(allUpdates, details));
        }
        renderTwoCellsRow("report.versionRange", details.getCurrentVersion().toString());
        renderTwoCellsRow("report.autoLinkDependencies", property.isAutoLinkDependencies());
        renderTwoCellsRow("report.banSnapshots", property.isBanSnapshots());
        renderTwoCellsRow("report.searchReactor", property.isSearchReactor());
        renderTwoCellsRow("report.preferReactor", property.isPreferReactor());

        sink.tableRows_();
        sink.table_();
    }

    /**
     * Renders a row of two cells, the first cell being an header and the second cell being a non-header cell.
     * @param textKey the key of the text to be rendered in the header cell.
     * @param b a yes/no value to be rendered in the non-header cell.
     */
    private void renderTwoCellsRow(String textKey, boolean b) {
        renderTwoCellsRow(textKey, getText(b ? "report.yes" : "report.no"));
    }

    private void renderAssociations(PropertyVersions details) {
        ArtifactAssociation[] associations = details.getAssociations();
        for (int i = 0; i < associations.length; i++) {
            if (i > 0) {
                sink.lineBreak();
            }
            sink.text(ArtifactUtils.versionlessKey(associations[i].getArtifact()));
        }
    }

    @Override
    protected void renderSummaryTableHeader(boolean hasScope, boolean hasType) {
        renderTableHeaderCells(
                "report.status",
                "report.property",
                "report.currentVersion",
                "report.latestSubIncremental",
                "report.latestIncremental",
                "report.latestMinor",
                "report.latestMajor");
    }

    @Override
    protected OverviewStats computeOverviewStats() {
        return OverviewStats.fromUpdates(model.getAllUpdates().values(), newestUpdateCache, isAllowSnapshots());
    }

    private void renderPropertyDetail(Property property, PropertyVersions details) {
        sink.section2();
        sink.sectionTitle2();
        sink.text("${" + property.getName() + "}");
        sink.sectionTitle2_();
        renderPropertyDetailTable(property, details);
        sink.section2_();
    }
}
