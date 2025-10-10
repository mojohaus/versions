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

import org.apache.maven.doxia.sink.Sink;
import org.codehaus.mojo.versions.reporting.model.ParentUpdatesModel;
import org.codehaus.plexus.i18n.I18N;

/**
 * A renderer for Parent updates reports, using {@link ParentUpdatesModel} as model.
 * @since 1.0-beta-1
 */
public class ParentUpdatesReportRenderer extends DependencyUpdatesReportRenderer<ParentUpdatesModel> {
    /**
     * Creates a new instance.
     *
     * @param i18n an {@link I18N} instance
     * @param sink the {@link Sink} to render to
     * @param locale the locale to render in
     * @param bundleName the resource bundle name to use
     * @param model object containing the updates model
     * @param allowSnapshots whether snapshots should be included
     */
    public ParentUpdatesReportRenderer(
            I18N i18n, Sink sink, Locale locale, String bundleName, ParentUpdatesModel model, boolean allowSnapshots) {
        super(i18n, sink, locale, bundleName, model, allowSnapshots);
    }

    @Override
    protected void renderOverview() {}

    @Override
    protected void renderManagementSummaryTable() {}
}
