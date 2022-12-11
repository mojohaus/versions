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
 * @since 1.0-beta-1
 */
public class ParentUpdatesReportRenderer extends DependencyUpdatesReportRenderer<ParentUpdatesModel> {
    public ParentUpdatesReportRenderer(
            I18N i18n, Sink sink, Locale locale, String bundleName, ParentUpdatesModel model) {
        super(i18n, sink, locale, bundleName, model);
    }

    @Override
    protected void renderOverview() {}

    @Override
    protected void renderManagementSummaryTable() {}
}
