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

import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.doxia.sink.SinkEventAttributes;
import org.apache.maven.doxia.sink.impl.SinkEventAttributeSet;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Base class for report renderers.
 * @param <T> modelled report object
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionsReportRenderer<T> extends VersionsReportRendererBase implements ReportRenderer {

    /**
     * Model of the object being rendered
     *
     * @since 2.13.0
     */
    protected T model;

    protected final ArtifactVersionsCache newestUpdateCache =
            new ArtifactVersionsCache(AbstractVersionDetails::getReportNewestUpdate);

    protected final ArtifactVersionsCache allUpdatesCache =
            new ArtifactVersionsCache(AbstractVersionDetails::getReportUpdates);

    protected final SinkEventAttributes headerAttributes = new SinkEventAttributeSet(SinkEventAttributes.WIDTH, "30%");

    protected AbstractVersionsReportRenderer(
            I18N i18n, Sink sink, Locale locale, String bundleName, T model, boolean allowSnapshots) {
        super(sink, i18n, locale, bundleName, allowSnapshots);
        this.model = model;
    }

    /**
     * {@inheritDoc}
     */
    protected void renderBody() {
        sink.section1();
        sink.sectionTitle1();
        sink.text(getText("report.overview.title"));
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text(getText("report.overview.text"));
        sink.paragraph_();

        renderOverview();

        renderManagementSummaryTable();
        renderSummaryTable();

        sink.section1_();

        sink.section1();
        sink.sectionTitle1();
        sink.text(getText("report.detail.title"));
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text(getText("report.detail.text"));
        sink.paragraph_();

        renderDetails();

        sink.section1_();
    }

    /**
     * Renders the "Overview" table
     */
    protected void renderOverview() {
        sink.table();
        sink.tableRow();
        renderOverviewTableRow(computeOverviewStats());
        sink.tableRow_();
        sink.table_();
    }

    /**
     * Renders the "management" (dependencyManagement, pluginManagement, etc.) summary table
     */
    protected abstract void renderManagementSummaryTable();

    /**
     * Renders the regular ("dependencies", "plugins", etc.) summary table
     */
    protected abstract void renderSummaryTable();

    /**
     * Renders the singular summary table row
     * @param stats summary statistics object to render
     * @param <Q> concrete {@linkplain OverviewStats} class
     */
    protected <Q extends OverviewStats> void renderOverviewTableRow(Q stats) {
        renderStatRow("report.overview.numUpToDate", stats.getUpToDate(), true);
        renderStatRow("report.overview.numNewerVersionAvailable", stats.getAny(), false);
        renderStatRow("report.overview.numNewerIncrementalAvailable", stats.getIncremental(), false);
        renderStatRow("report.overview.numNewerMinorAvailable", stats.getMinor(), false);
        renderStatRow("report.overview.numNewerMajorAvailable", stats.getMajor(), false);
    }

    /**
     * Renders one table row for the given statistics.
     * @param textKey the key of the text to be rendered.
     * @param statCount the number of artifacts with the given stat.
     * @param forceSuccessIcon if true, the success icon will be rendered regardless.
     */
    protected void renderStatRow(String textKey, int statCount, boolean forceSuccessIcon) {
        sink.tableRow();
        sink.tableCell();
        renderIcon(statCount == 0 || forceSuccessIcon);
        sink.tableCell_();
        sink.tableCell();
        sink.text(getText(textKey));
        sink.tableCell_();
        sink.tableCell();
        sink.text(Integer.toString(statCount));
        sink.tableCell_();
        sink.tableRow_();
    }

    /**
     * Renders the success or warning icon.
     * @param success if true, the success icon will be rendered, otherwise the warning icon will be rendered.
     */
    protected void renderIcon(boolean success) {
        if (success) {
            renderSuccessIcon();
        } else {
            renderWarningIcon();
        }
    }

    /**
     * Computes the {@linkplain OverviewStats} object needed to render the summary table row
     * @param <Q> concrete {@linkplain OverviewStats} class
     * @return stats object
     */
    protected abstract <Q extends OverviewStats> Q computeOverviewStats();

    /**
     * Renders the details table
     */
    protected abstract void renderDetails();

    protected void renderSummaryTable(Map<Dependency, ArtifactVersions> contents, boolean hasScope) {
        sink.table();

        sink.tableRow();
        renderSummaryTableHeader(hasScope, true);
        sink.tableRow_();

        contents.forEach((artifact, artifactVersions) -> renderSummaryTableRow(artifact, artifactVersions, hasScope));

        sink.tableRow();
        renderSummaryTableHeader(hasScope, true);
        sink.tableRow_();

        sink.table_();
    }

    protected void renderSummaryTableHeader(boolean hasScope, boolean hasType) {
        renderTableHeaderCells("report.status", "report.groupId", "report.artifactId", "report.currentVersion");
        if (hasScope) {
            renderTableHeaderCells("report.scope");
        }
        if (hasType) {
            renderTableHeaderCells("report.classifier", "report.type");
        }
        renderTableHeaderCells(
                "report.latestSubIncremental", "report.latestIncremental", "report.latestMinor", "report.latestMajor");
    }

    protected void renderSummaryTableRow(Dependency artifact, ArtifactVersions details, boolean includeScope) {
        details.setCurrentVersion(artifact.getVersion());
        ArtifactVersion[] allUpdates = allUpdatesCache.get(details, empty(), isAllowSnapshots());
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        sink.tableRow();

        sink.tableCell();
        renderIcon(upToDate);
        sink.tableCell_();

        renderCells(artifact.getGroupId(), artifact.getArtifactId(), artifact.getVersion());
        if (includeScope) {
            renderCell(artifact.getScope());
        }
        renderCells(artifact.getClassifier(), artifact.getType());
        renderNewestVersions(details);

        sink.tableRow_();
    }

    /**
     * Renders the newest versions for the given artifact.
     * @param details the artifact for which to render the newest versions.
     */
    protected void renderNewestVersions(AbstractVersionDetails details) {
        renderBoldCell(newestUpdateCache.get(details, of(SUBINCREMENTAL), isAllowSnapshots()));
        renderBoldCell(newestUpdateCache.get(details, of(INCREMENTAL), isAllowSnapshots()));
        renderBoldCell(newestUpdateCache.get(details, of(MINOR), isAllowSnapshots()));
        renderBoldCell(newestUpdateCache.get(details, of(MAJOR), isAllowSnapshots()));
    }

    protected void renderDependencyDetailTable(Dependency artifact, ArtifactVersions details, boolean includeScope) {
        ArtifactVersion[] allUpdates = allUpdatesCache.get(details, empty(), isAllowSnapshots());
        boolean upToDate = allUpdates == null || allUpdates.length == 0;

        sink.table();
        sink.tableRows(new int[] {Sink.JUSTIFY_RIGHT, Sink.JUSTIFY_LEFT}, false);

        renderTwoCellsRow("report.status", () -> renderStatus(details));
        renderTwoCellsRow("report.groupId", artifact.getGroupId());
        renderTwoCellsRow("report.artifactId", artifact.getArtifactId());
        renderTwoCellsRow("report.currentVersion", artifact.getVersion());
        if (includeScope) {
            renderTwoCellsRow("report.scope", artifact.getScope());
        }
        renderTwoCellsRow("report.classifier", artifact.getClassifier());
        renderTwoCellsRow("report.type", artifact.getType());
        if (!upToDate) {
            renderTwoCellsRow("report.updateVersions", () -> renderVersions(allUpdates, details));
        }

        sink.tableRows_();
        sink.table_();
    }

    /**
     * Renders a row of two cells, the first cell being an header and the second cell being a non-header cell.
     * @param textKey the key of the text to be rendered.
     * @param textValue the value of the text to be rendered.
     */
    protected void renderTwoCellsRow(String textKey, String textValue) {
        sink.tableRow();
        sink.tableHeaderCell(headerAttributes);
        sink.text(getText(textKey));
        sink.tableHeaderCell_();
        sink.tableCell();
        sink.text(textValue);
        sink.tableCell_();
        sink.tableRow_();
    }

    /**
     * Renders a row of two cells, the first cell being an header and the second cell being a non-header cell.
     * @param textKey the key of the text to be rendered.
     * @param runnable the runnable to be executed to render the second cell content.
     */
    protected void renderTwoCellsRow(String textKey, Runnable runnable) {
        sink.tableRow();
        sink.tableHeaderCell(headerAttributes);
        sink.text(getText(textKey));
        sink.tableHeaderCell_();
        sink.tableCell();
        runnable.run();
        sink.tableCell_();
        sink.tableRow_();
    }

    /**
     * Renders the status of the given artifact.
     * @param details the artifact for which to render the status.
     */
    protected void renderStatus(AbstractVersionDetails details) {
        if (newestUpdateCache.get(details, of(SUBINCREMENTAL), isAllowSnapshots()) != null) {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text(getText("report.otherUpdatesAvailable"));
        } else if (newestUpdateCache.get(details, of(INCREMENTAL), isAllowSnapshots()) != null) {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text(getText("report.incrementalUpdatesAvailable"));
        } else if (newestUpdateCache.get(details, of(MINOR), isAllowSnapshots()) != null) {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text(getText("report.minorUpdatesAvailable"));
        } else if (newestUpdateCache.get(details, of(MAJOR), isAllowSnapshots()) != null) {
            renderWarningIcon();
            sink.nonBreakingSpace();
            sink.text(getText("report.majorUpdatesAvailable"));
        } else {
            renderSuccessIcon();
            sink.nonBreakingSpace();
            sink.text(getText("report.noUpdatesAvailable"));
        }
    }

    /**
     * Builds the list of restrictions for the given artifact or property, based on its version range.
     * used to determine if a candidate version is outside the range, and if it should be displayed with a star.
     * @param details the artifact or property for which to render the versions.
     * @return the list of restrictions for the spec versions range.
     */
    private List<Restriction> getArtifactVersionRange(AbstractVersionDetails details) {
        try {
            String spec = details.getCurrentVersion().toString();
            VersionRange range = VersionRange.createFromVersionSpec(spec);
            return range.getRestrictions();
        } catch (InvalidVersionSpecificationException ignored) {
            ignored.printStackTrace(System.err);
        }
        return Collections.EMPTY_LIST;
    }

    /**
     * Renders the list of versions that are available for the given artifact or property.
     * @param allUpdates the list of all updates available.
     * @param details the versions details for the given artifact or property.
     */
    protected void renderVersions(ArtifactVersion[] allUpdates, AbstractVersionDetails details) {
        List<Restriction> versionRange = getArtifactVersionRange(details);
        boolean someNotAllowed = false;
        for (int i = 0; i < allUpdates.length; i++) {
            if (i > 0) {
                sink.lineBreak();
            }
            // if candidate version in range, display no star.
            ArtifactVersion candidate = allUpdates[i];
            boolean allowed = versionRange.stream()
                    .anyMatch(restriction -> details.isVersionInRestriction(restriction, candidate));
            String label = getLabel(allUpdates[i], details);
            if (!allowed) {
                sink.text("* ");
                someNotAllowed = true;
            }
            if (allowed && label != null) {
                safeBold();
            }
            sink.text(allUpdates[i].toString());
            if (label != null) {
                if (allowed) {
                    safeBold_();
                }
                sink.nonBreakingSpace();
                safeItalic();
                sink.text(label);
                safeItalic_();
            }
        }
        if (someNotAllowed) {
            sink.lineBreak();
            sink.lineBreak();
            sink.text("* ");
            safeItalic();
            sink.text(getText("report.excludedVersion"));
            safeItalic_();
        }
    }

    /**
     * Returns a text label to describe if the given version is a major, minor, incremental or subincremental update.
     * @param version the version to describe.
     * @param details the artifact for which to render the versions.
     * @return a text label to describe if the given version is a major, minor, incremental or subincremental update.
     */
    protected String getLabel(ArtifactVersion version, AbstractVersionDetails details) {

        if (equals(version, newestUpdateCache.get(details, of(SUBINCREMENTAL), isAllowSnapshots()))) {
            return getText("report.latestSubIncremental");
        }

        if (equals(version, newestUpdateCache.get(details, of(INCREMENTAL), isAllowSnapshots()))) {
            return getText("report.latestIncremental");
        }

        if (equals(version, newestUpdateCache.get(details, of(MINOR), isAllowSnapshots()))) {
            return getText("report.latestMinor");
        }

        if (equals(version, newestUpdateCache.get(details, of(MAJOR), isAllowSnapshots()))) {
            return getText("report.latestMajor");
        }

        return null;
    }
}
