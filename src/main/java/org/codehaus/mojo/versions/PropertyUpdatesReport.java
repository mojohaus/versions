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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Generates a report of available updates for properties of a project which are linked to the dependencies and/or
 * plugins of a project.
 *
 * @author Stephen Connolly
 * @goal property-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 1.0-beta-1
 */
public class PropertyUpdatesReport
    extends AbstractVersionsReport
{


    /**
     * Any restrictions that apply to specific properties.
     *
     * @parameter
     * @since 1.0-beta-1
     */
    private Property[] properties;

    /**
     * A comma separated list of properties to include in the report.
     *
     * @parameter expression="${includeProperties}"
     * @since 1.0-beta-1
     */
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not include in the report.
     *
     * @parameter expression="${excludeProperties}"
     * @since 1.0-beta-1
     */
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @parameter expression="${autoLinkItems}" defaultValue="true"
     * @since 1.0-beta-1
     */
    private Boolean autoLinkItems;


    /**
     * {@inheritDoc}
     */
    public boolean isExternalReport()
    {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public boolean canGenerateReport()
    {
        return haveBuildProperties();
    }

    private boolean haveBuildProperties()
    {
        return getProject().getProperties() != null && !getProject().getProperties().isEmpty();
    }

    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        final Map updateSet;
        try
        {
            updateSet =
                getHelper().getVersionPropertiesMap( getProject(), properties, includeProperties, excludeProperties,
                                                     !Boolean.FALSE.equals( autoLinkItems ) );
            List propertyUpdateDetails = new ArrayList();
            Iterator i = updateSet.entrySet().iterator();
            while ( i.hasNext() )
            {
                Map.Entry entry = (Map.Entry) i.next();
                Property property = (Property) entry.getKey();
                PropertyVersions version = (PropertyVersions) entry.getValue();

                ArtifactVersion[] artifactVersions =
                    version.getVersions( !property.isBanSnapshots() && Boolean.TRUE.equals( allowSnapshots ) );
                ArtifactVersion current = new DefaultArtifactVersion( property.getVersion() );

                VersionComparator versionComparator = version.getVersionComparator();
                int segmentCount = versionComparator.getSegmentCount( current );
                ArtifactVersion nextVersion = segmentCount < 3
                    ? null
                    : version.getOldestVersion( current, versionComparator.incrementSegment( current, 2 ),
                                                Boolean.TRUE.equals( getAllowSnapshots() ), false, false );
                ArtifactVersion nextIncremental = segmentCount < 3
                    ? null
                    : version.getOldestVersion( versionComparator.incrementSegment( current, 2 ),
                                                versionComparator.incrementSegment( current, 1 ),
                                                Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
                ArtifactVersion latestIncremental = segmentCount < 3
                    ? null
                    : version.getNewestVersion( versionComparator.incrementSegment( current, 2 ),
                                                versionComparator.incrementSegment( current, 1 ),
                                                Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
                ArtifactVersion nextMinor = segmentCount < 2
                    ? null
                    : version.getOldestVersion( versionComparator.incrementSegment( current, 1 ),
                                                versionComparator.incrementSegment( current, 0 ),
                                                Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
                ArtifactVersion latestMinor = segmentCount < 2
                    ? null
                    : version.getNewestVersion( versionComparator.incrementSegment( current, 1 ),
                                                versionComparator.incrementSegment( current, 0 ),
                                                Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
                ArtifactVersion nextMajor =
                    version.getOldestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                              Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
                ArtifactVersion latestMajor =
                    version.getNewestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                              Boolean.TRUE.equals( getAllowSnapshots() ), true, false );

                getLog().debug(
                    "Property ${" + property.getName() + "}: Set of valid available versions is " + Arrays.asList(
                        artifactVersions ) );

            }
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
//        Set plugins = new TreeSet( new PluginComparator() );
//        if ( haveBuildPlugins() )
//        {
//            plugins.addAll( getProject().getBuild().getPlugins() );
//        }
//        plugins.addAll( getProject().getDependencies() );
//        PluginUpdatesRenderer renderer =
//            new PluginUpdatesRenderer( sink, getI18n(), getOutputName(), locale, pluginUpdates,
//                                       pluginManagementUpdates );
//        renderer.render();
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "property-updates-report";
    }
}