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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.utils.DependencyComparator;

import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @goal dependency-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 31-Jan-2009 10:32:10
 */
public class DependencyUpdatesReport
    extends AbstractVersionsReport
{
    public boolean isExternalReport()
    {
        return false;
    }

    public boolean canGenerateReport()
    {
        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException, MojoExecutionException
    {
        Map/*<Dependency,DependencyUpdateDetails>*/ dependencyUpdates = new TreeMap( new DependencyComparator() );
        Set dependencies = new TreeSet( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );
        Iterator i = dependencies.iterator();
        while ( i.hasNext() )
        {
            Dependency dependency = (Dependency) i.next();
            String groupId = dependency.getGroupId();
            String artifactId = dependency.getArtifactId();
            String version = dependency.getVersion();
            getLog().debug( "Checking " + groupId + ":" + artifactId + " for updates newer than " + version );

            VersionRange versionRange = null;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( version );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MavenReportException( "Invalid version range specification: " + version, e );
            }

            Artifact artifact =
                artifactFactory.createDependencyArtifact( groupId, artifactId, versionRange, dependency.getType(),
                                                          dependency.getClassifier(), dependency.getScope() );

            ArtifactVersions artifactVersions =
                getHelper().lookupArtifactVersions( artifact, Boolean.TRUE.equals( getAllowSnapshots() ) );

            ArtifactVersion current = getHelper().createArtifactVersion( dependency.getVersion() );
            VersionComparator versionComparator = artifactVersions.getVersionComparator();
            int segmentCount = versionComparator.getSegmentCount( current );
            ArtifactVersion nextIncremental = segmentCount < 3
                ? null
                : artifactVersions.getOldestVersion( current, versionComparator.incrementSegment( current, 1 ),
                                                     Boolean.TRUE.equals( getAllowSnapshots() ), false, false );
            ArtifactVersion latestIncremental = segmentCount < 3
                ? null
                : artifactVersions.getLatestVersion( current, versionComparator.incrementSegment( current, 1 ),
                                                     Boolean.TRUE.equals( getAllowSnapshots() ), false, false );
            ArtifactVersion nextMinor = segmentCount < 2
                ? null
                : artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 1 ),
                                                     versionComparator.incrementSegment( current, 0 ),
                                                     Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
            ArtifactVersion latestMinor = segmentCount < 2
                ? null
                : artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 1 ),
                                                     versionComparator.incrementSegment( current, 0 ),
                                                     Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
            ArtifactVersion nextMajor =
                artifactVersions.getOldestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                                   Boolean.TRUE.equals( getAllowSnapshots() ), true, false );
            ArtifactVersion latestMajor =
                artifactVersions.getLatestVersion( versionComparator.incrementSegment( current, 0 ), null,
                                                   Boolean.TRUE.equals( getAllowSnapshots() ), true, false );

            dependencyUpdates.put( dependency,
                                   new ArtifactUpdatesDetails( artifact, nextIncremental, latestIncremental, nextMinor,
                                                               latestMinor, nextMajor, latestMajor,
                                                               artifactVersions.getNewerVersions( current ) ) );


        }
        DependencyUpdatesRenderer renderer =
            new DependencyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, dependencyUpdates );
        renderer.render();
    }

    public String getOutputName()
    {
        return "dependency-updates-report";
    }

}