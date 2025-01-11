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

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import java.util.Locale;

import org.apache.maven.doxia.sink.Sink;
import org.codehaus.mojo.versions.reporting.model.DependencyUpdatesModel;
import org.codehaus.mojo.versions.reporting.model.ParentUpdatesModel;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.reporting.model.PropertyUpdatesModel;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;
import org.codehaus.plexus.i18n.I18N;

/**
 * Factory for report renderers
 */
@Named
@Singleton
public class ReportRendererFactoryImpl implements ReportRendererFactory {
    public static final String DEPENDENCY_UPDATES_REPORT = "dependency-updates-report";
    public static final String DEPENDENCY_UPDATES_AGGREGATE_REPORT = "dependency-updates-aggregate-report";
    public static final String PLUGIN_UPDATES_REPORT = "plugin-updates-report";
    public static final String PLUGIN_UPDATES_AGGREGATE_REPORT = "plugin-updates-aggregate-report";
    public static final String PROPERTY_UPDATES_REPORT = "property-updates-report";
    public static final String PROPERTY_UPDATES_AGGREGATE_REPORT = "property-updates-aggregate-report";
    public static final String PARENT_UPDATES_REPORT = "parent-updates-report";
    private final I18N i18N;

    @Inject
    public ReportRendererFactoryImpl(I18N i18N) {
        this.i18N = i18N;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    public <T extends ReportRenderer, U> T createReportRenderer(
            String reportName, Sink sink, Locale locale, U model, boolean allowSnapshots)
            throws IllegalArgumentException {
        switch (reportName) {
            case DEPENDENCY_UPDATES_REPORT:
            case DEPENDENCY_UPDATES_AGGREGATE_REPORT:
                return (T) new DependencyUpdatesReportRenderer<>(
                        i18N, sink, locale, reportName, (DependencyUpdatesModel) model, allowSnapshots);
            case PLUGIN_UPDATES_REPORT:
            case PLUGIN_UPDATES_AGGREGATE_REPORT:
                return (T) new PluginUpdatesReportRenderer(
                        i18N, sink, locale, reportName, (PluginUpdatesModel) model, allowSnapshots);
            case PROPERTY_UPDATES_REPORT:
            case PROPERTY_UPDATES_AGGREGATE_REPORT:
                return (T) new PropertyUpdatesReportRenderer(
                        i18N, sink, locale, reportName, (PropertyUpdatesModel) model, allowSnapshots);
            case PARENT_UPDATES_REPORT:
                return (T) new ParentUpdatesReportRenderer(
                        i18N, sink, locale, reportName, (ParentUpdatesModel) model, allowSnapshots);
            default:
                throw new IllegalArgumentException("Invalid report name: " + reportName);
        }
    }
}
