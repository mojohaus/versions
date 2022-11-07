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
import org.codehaus.mojo.versions.api.ReportRenderer;

/**
 * Factory for report renderers
 */
public interface ReportRendererFactory
{
    /**
     * Creates a new {@linkplain ReportRenderer} instance for the given report renderer name.
     *
     * @param <U> class of the model being rendered
     * @param <T> concrete class of the report renderer
     * @param reportName name of the report to generate
     * @param sink sink to use for rendering
     * @param locale locale to use for rendering
     * @param model data to render
     * @return new report renderer
     * @throws IllegalArgumentException thrown if the report with the given name could not be found
     */
    <T extends ReportRenderer, U> T createReportRenderer( String reportName, Sink sink, Locale locale, U model )
            throws IllegalArgumentException;
}
