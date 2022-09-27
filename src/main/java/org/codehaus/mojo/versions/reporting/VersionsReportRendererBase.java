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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.reporting.AbstractMavenReportRenderer;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Base class over AbstractVersionsReportRenderer providing base
 * utility methods
 */
public abstract class VersionsReportRendererBase extends AbstractMavenReportRenderer
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
    protected Locale locale;
    /**
     * The name of the bundle containing our I18n resources.
     *
     * @since 1.0-beta-1
     */
    protected String bundleName;

    public VersionsReportRendererBase( Sink sink, I18N i18n, Locale locale, String bundleName )
    {
        super( sink );
        this.i18n = i18n;
        this.locale = locale;
        this.bundleName = bundleName;
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
        sink.figureGraphics( "images/icon_warning_sml.gif" );
    }

    protected void renderSuccessIcon()
    {
        sink.figureGraphics( "images/icon_success_sml.gif" );
    }

    protected boolean equals( ArtifactVersion v1, ArtifactVersion v2 )
    {
        return v1 == v2 || ( v1 != null && v1.equals( v2 ) )
                || ( v1 != null && v2 != null && v1.toString().equals( v2.toString() ) );
    }

    protected void safeBold()
    {
        try
        {
            sink.bold();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    @SuppressWarnings( "checkstyle:MethodName" )
    protected void safeBold_()
    {
        try
        {
            sink.bold_();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    protected void safeItalic()
    {
        try
        {
            sink.italic();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    @SuppressWarnings( "checkstyle:MethodName" )
    protected void safeItalic_()
    {
        try
        {
            sink.italic_();
        }
        catch ( NoSuchMethodError e )
        {
            // ignore Maven 2.1.0
        }
    }

    protected String getLabel( ArtifactVersion version, AbstractVersionDetails versions )
    {
        if ( equals( version, versions.getNewestUpdate( of( SUBINCREMENTAL ) ) ) )
        {
            return getText( "report.latestSubIncremental" );
        }

        if ( equals( version, versions.getOldestUpdate( of( SUBINCREMENTAL ) ) ) )
        {
            return getText( "report.nextVersion" );
        }

        if ( equals( version, versions.getOldestUpdate( of( INCREMENTAL ) ) ) )
        {
            return getText( "report.nextIncremental" );
        }

        if ( equals( version, versions.getNewestUpdate( of( INCREMENTAL ) ) ) )
        {
            return getText( "report.latestIncremental" );
        }

        if ( equals( version, versions.getOldestUpdate( of( MINOR ) ) ) )
        {
            return getText( "report.nextMinor" );
        }

        if ( equals( version, versions.getNewestUpdate( of( MINOR ) ) ) )
        {
            return getText( "report.latestMinor" );
        }

        if ( equals( version, versions.getOldestUpdate( of( MAJOR ) ) ) )
        {
            return getText( "report.nextMajor" );
        }

        if ( equals( version, versions.getNewestUpdate( of( MAJOR ) ) ) )
        {
            return getText( "report.latestMajor" );
        }

        return "";
    }
}
