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

import org.apache.maven.doxia.sink.Sink;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.utils.PropertyComparator;
import org.codehaus.plexus.i18n.I18N;

import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * @since 1.0-beta-1
 */
public class PropertyUpdatesRenderer
    extends AbstractVersionsReportRenderer
{

    private final Map<Property, PropertyVersions> propertyUpdates;

    public PropertyUpdatesRenderer( Sink sink, I18N i18n, String bundleName, Locale locale,
                                    Map<Property, PropertyVersions> propertyUpdates )
    {
        super( sink, bundleName, i18n, locale );
        this.propertyUpdates = propertyUpdates;
    }


    protected void renderBody()
    {
        Map<Property, PropertyVersions> allUpdates =
            new TreeMap<Property, PropertyVersions>( new PropertyComparator() );
        allUpdates.putAll( propertyUpdates );

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.overview.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.overview.text" ) );
        sink.paragraph_();

        renderSummaryTotalsTable( allUpdates );

        renderSummaryTable( "report.overview.property", propertyUpdates, "report.overview.noProperty" );

        sink.section1_();

        sink.section1();
        sink.sectionTitle1();
        sink.text( getText( "report.detail.title" ) );
        sink.sectionTitle1_();
        sink.paragraph();
        sink.text( getText( "report.detail.text" ) );
        sink.paragraph_();

        for ( final Map.Entry<Property, PropertyVersions> entry : allUpdates.entrySet() )
        {
            renderPropertyDetail( entry.getKey(), entry.getValue() );
        }
        sink.section1_();
    }

    private void renderSummaryTable( String titleKey, Map contents, String emptyKey )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( getText( titleKey ) );
        sink.sectionTitle2_();

        if ( contents.isEmpty() )
        {
            sink.paragraph();
            sink.text( getText( emptyKey ) );
            sink.paragraph_();
        }
        else
        {
            renderPropertySummaryTable( contents );
        }
        sink.section2_();
    }

    private void renderSummaryTotalsTable( Map allUpdates )
    {
        int numInc = 0;
        int numMin = 0;
        int numMaj = 0;
        int numAny = 0;
        int numCur = 0;
        for ( Iterator iterator = allUpdates.values().iterator(); iterator.hasNext(); )
        {
            PropertyVersions details = (PropertyVersions) iterator.next();
            if ( details.getOldestUpdate( UpdateScope.SUBINCREMENTAL ) != null )
            {
                numAny++;
            }
            else if ( details.getOldestUpdate( UpdateScope.INCREMENTAL ) != null )
            {
                numInc++;
            }
            else if ( details.getOldestUpdate( UpdateScope.MINOR ) != null )
            {
                numMin++;
            }
            else if ( details.getOldestUpdate( UpdateScope.MAJOR ) != null )
            {
                numMaj++;
            }
            else
            {
                numCur++;
            }
        }
        sink.table();
        sink.tableRow();
        sink.tableCell();
        renderSuccessIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numUpToDate" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numCur ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerVersionAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numAny ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerIncrementalAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numInc ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerMinorAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numMin ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.tableRow();
        sink.tableCell();
        renderWarningIcon();
        sink.tableCell_();
        sink.tableCell();
        sink.text( getText( "report.overview.numNewerMajorAvailable" ) );
        sink.tableCell_();
        sink.tableCell();
        sink.text( Integer.toString( numMaj ) );
        sink.tableCell_();
        sink.tableRow_();
        sink.table_();
    }

    private void renderPropertyDetail( Property property, PropertyVersions versions )
    {
        sink.section2();
        sink.sectionTitle2();
        sink.text( "${" + property.getName() + "}" );
        sink.sectionTitle2_();
        renderPropertyDetailTable( property, versions );
        sink.section2_();
    }

}