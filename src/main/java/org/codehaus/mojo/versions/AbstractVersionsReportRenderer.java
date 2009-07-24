package org.codehaus.mojo.versions;

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

import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.plexus.i18n.I18N;

import java.util.Locale;

/**
 * Base class for report renderers.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionsReportRenderer
    extends AbstractMavenReportRenderer
{
    /**
     * Internationalization component.
     *
     * @since 1.0-beta-1
     */
    protected final I18N i18n;

    /**
     * The locale we are rendering for.
     *
     * @since 1.0-beta-1
     */
    protected final Locale locale;

    /**
     * The name of the bundle containing our I18n resources.
     *
     * @since 1.0-beta-1
     */
    protected final String bundleName;

    public AbstractVersionsReportRenderer( org.apache.maven.doxia.sink.Sink sink, String bundleName, I18N i18n,
                                           Locale locale )
    {
        super( sink );
        this.bundleName = bundleName;
        this.i18n = i18n;
        this.locale = locale;
    }

    public String getTitle()
    {
        return getText( "report.title" );
    }

    /**
     * Gets the localized message for this report.
     *
     * @param key the message key.
     * @return the message.
     */
    public String getText( String key )
    {
        return i18n.getString( bundleName, locale, key );
    }

    protected void renderWarningIcon()
    {
        sink.figure();
        sink.figureGraphics( "images/icon_warning_sml.gif" );
        sink.figure_();
    }

    protected void renderSuccessIcon()
    {
        sink.figure();
        sink.figureGraphics( "images/icon_success_sml.gif" );
        sink.figure_();
    }

    protected boolean equals( ArtifactVersion v1, ArtifactVersion v2 )
    {
        return v1 == v2 || ( v1 != null && v1.equals( v2 ) ) || ( v1 != null && v2 != null && v1.toString().equals(
            v2.toString() ) );
    }
}
