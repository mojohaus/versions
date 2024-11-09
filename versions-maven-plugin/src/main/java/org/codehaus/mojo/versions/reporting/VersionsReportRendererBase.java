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

import java.util.Arrays;
import java.util.Locale;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;
import org.codehaus.plexus.i18n.I18N;

/**
 * Base class over AbstractVersionsReportRenderer providing base
 * utility methods
 */
public abstract class VersionsReportRendererBase extends AbstractMavenReportRenderer implements ReportRenderer {
    /**
     * Internationalization component.
     *
     * @since 1.0-beta-1
     */
    protected final I18N i18n;

    private final boolean allowSnapshots;

    /**
     * The locale we are rendering for.
     *
     * @since 1.0-beta-1
     */
    protected Locale locale;
    /**
     * The name of the bundle containing our I18n resources.
     *
     * @since 1.0-beta-1
     */
    protected String bundleName;

    protected VersionsReportRendererBase(
            Sink sink, I18N i18n, Locale locale, String bundleName, boolean allowSnapshots) {
        super(sink);
        this.i18n = i18n;
        this.locale = locale;
        this.bundleName = bundleName;
        this.allowSnapshots = allowSnapshots;
    }

    public String getTitle() {
        return getText("report.title");
    }

    @Override
    public boolean isAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * Gets the localized message for this report.
     *
     * @param key the message key.
     * @return the message.
     */
    public String getText(String key) {
        return i18n.getString(bundleName, locale, key);
    }

    protected void renderWarningIcon() {
        sink.figureGraphics("images/icon_warning_sml.gif");
    }

    protected void renderSuccessIcon() {
        sink.figureGraphics("images/icon_success_sml.gif");
    }

    protected boolean equals(ArtifactVersion v1, ArtifactVersion v2) {
        return v1 == v2
                || (v1 != null && v1.equals(v2))
                || (v1 != null && v2 != null && v1.toString().equals(v2.toString()));
    }

    /**
     * Renders a table header containing elements denoted by the given keys
     * @param keys variable argument list containing keys of the property file to retrieve the
     *             headers from
     */
    protected void renderTableHeaderCells(String... keys) {
        Arrays.stream(keys).map(this::getText).forEachOrdered(this::tableHeaderCell);
    }

    /**
     * Renders a bold table cell containing the given text.
     * @param object the text to be rendered, or null for an empty cell.
     */
    protected void renderBoldCell(Object object) {
        renderBoldCell(true, object);
    }
    /**
     * Renders a table cell containing the given text.
     * @param object the text to be rendered, or null for an empty cell.
     */
    protected void renderCell(Object object) {
        renderBoldCell(false, object);
    }
    /**
     * Renders multiple cells containing the given texts.
     * @param objects the texts to be rendered, null for empty cells.
     */
    protected void renderCells(Object... objects) {
        for (Object object : objects) {
            renderBoldCell(false, object);
        }
    }

    /**
     * Renders a bold table cell containing the given text.
     * @param bold true to render the cell in bold, false otherwise.
     * @param object the text to be rendered, or null for an empty cell.
     */
    protected void renderBoldCell(boolean bold, Object object) {
        sink.tableCell();
        renderBoldText(bold, object);
        sink.tableCell_();
    }

    /**
     * Renders a bold text.
     * @param bold true to render the text in bold, false otherwise.
     * @param object the text to be rendered, or null for an empty cell.
     */
    protected void renderBoldText(boolean bold, Object object) {
        if (object != null) {
            String text = object.toString();
            if (!text.isEmpty()) {
                if (bold) {
                    sink.bold();
                }
                sink.text(text);
                if (bold) {
                    sink.bold_();
                }
            }
        }
    }
}
