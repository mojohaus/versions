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
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.reporting.MavenReport;
import org.codehaus.mojo.versions.ordering.VersionComparators;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * This report summarizes all project dependencies for which newer versions may exist. For convenience, the new versions
 * are segregated by incremental, minor, and major changes, since each tends to have a different level of effort (and
 * risk) involved when upgrading.
 *
 * @author <a href="mailto:matthew.beermann@cerner.com">Matthew Beermann</a>
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal display-dependency-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 1.0-alpha-3
 */
public class DisplayDependencyUpdatesReport
    extends AbstractVersionsReport
{

    /**
     * A list of groupId:artifactId keys, which indicate that the corresponding artifact(s) should be omitted from the
     * report (even when showAll is true). For example:<br/>
     * &lt;excludes&gt;<br/>
     * &nbsp;&nbsp;&nbsp;&nbsp;&lt;exclude&gt;com.oracle:ojdbc14&lt;/exclude&gt;<br/>
     * &lt;/excludes&gt;
     *
     * @parameter expression="${excludes}"
     */
    protected ArrayList excludes;

    /**
     * If true, show <i>all</i> unexcluded dependencies in the report - even those that have no updates available.
     *
     * @parameter expression="${showAll}" default-value="false"
     * @required
     */
    protected Boolean showAll;

    public boolean canGenerateReport()
    {
        return true;
    }

    /**
     * generates the report.
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool.
     */
    protected void doGenerateReport( Locale locale, org.apache.maven.doxia.sink.Sink sink )
    {
        Map artifacts = new TreeMap();
        for ( Iterator it = getProject().getArtifacts().iterator(); it.hasNext(); )
        {
            Artifact artifact = (Artifact) it.next();
            ArtifactVersion currentVersion = new DefaultArtifactVersion( artifact.getVersion() );
            if ( isExcluded( artifact ) )
            {
                continue;
            }

            try
            {
                // Find the latest version, accepting major changes
                // Range: [current,)
                ArtifactVersion latestMajor = findLatestVersion( artifact, VersionRange.createFromVersionSpec(
                    "[" + artifact.getVersion() + ",)" ), getAllowSnapshots(), false );

                // Find the latest version, accepting minor changes
                // Range: [current, (currentMajor+1).0.0)
                ArtifactVersion latestMinor = findLatestVersion( artifact, VersionRange.createFromVersionSpec(
                    "[" + artifact.getVersion() + "," + ( currentVersion.getMajorVersion() + 1 ) + ".0.0)" ),
                                                                 getAllowSnapshots(), false );

                // Find the latest version, accepting incremental changes
                // Range: [current, (currentMajor).(currentMinor+1).0)
                ArtifactVersion latestIncremental = findLatestVersion( artifact, VersionRange.createFromVersionSpec(
                    "[" + artifact.getVersion() + "," + currentVersion.getMajorVersion() + "."
                        + ( currentVersion.getMinorVersion() + 1 ) + ".0)" ), getAllowSnapshots(), false );

                // Add the results of our search to the collection
                MultiVersionSummary summary =
                    new MultiVersionSummary( artifact, currentVersion, latestMajor, latestMinor, latestIncremental );
                if ( hasUpdates( summary ) )
                {
                    artifacts.put( artifact.getId(), summary );
                }
            }
            catch ( Exception e )
            {
                // If something goes haywire, warn, but continue on our merry way
                getLog().warn( "Problem encountered while searching for newer versions:", e );
            }
        }

        // Last but not least, send everything we've gathered off to be rendered
        DisplayDependencyUpdatesRenderer renderer = new DisplayDependencyUpdatesRenderer( sink, artifacts,
                                                                                          VersionComparators.getVersionComparator(
                                                                                              getComparisonMethod() ) );
        renderer.render();
    }

    public String getCategoryName()
    {
        return MavenReport.CATEGORY_PROJECT_INFORMATION;
    }

    public String getDescription( Locale locale )
    {
        return "A report summarizing newer versions of the project's dependencies that may be available.";
    }

    public String getName( Locale locale )
    {
        return "Dependency ArtifactVersions";
    }

    public String getOutputName()
    {
        return "dependency-updates-report";
    }

    protected boolean hasUpdates( MultiVersionSummary summary )
    {
        // If we've been told to show them all, well, show them all
        if ( Boolean.TRUE.equals( showAll ) )
        {
            return true;
        }

        // Err on the side of caution for dependencies we don't understand
        ArtifactVersion currentVersion = summary.getCurrentVersion();
        if ( currentVersion.toString().equals( currentVersion.getQualifier() ) )
        {
            return true;
        }

        // Are any of the versions we found larger than the one we have now?
        Comparator comparator = VersionComparators.getVersionComparator( getComparisonMethod() );
        if ( summary.getLatestIncremental() != null
            && comparator.compare( currentVersion, summary.getLatestIncremental() ) < 0 )
        {
            return true;
        }
        else if ( summary.getLatestMinor() != null
            && comparator.compare( currentVersion, summary.getLatestMinor() ) < 0 )
        {
            return true;
        }
        else if ( summary.getLatestMajor() != null
            && comparator.compare( currentVersion, summary.getLatestMajor() ) < 0 )
        {
            return true;
        }

        // If we made it this far, then the answer is apparently "no"...
        return false;
    }

    protected boolean isExcluded( Artifact artifact )
    {
        if ( excludes == null || excludes.size() == 0 )
        {
            return false;
        }

        String candidate = artifact.getGroupId() + ":" + artifact.getArtifactId();
        if ( excludes.contains( candidate ) )
        {
            return true;
        }

        return false;
    }

    public boolean isExternalReport()
    {
        return false;
    }

    protected static class MultiVersionSummary
    {
        private final Artifact artifact;

        private final ArtifactVersion currentVersion, latestMajor, latestMinor, latestIncremental;

        public MultiVersionSummary( Artifact artifact, ArtifactVersion currentVersion, ArtifactVersion latestMajor,
                                    ArtifactVersion latestMinor, ArtifactVersion latestIncremental )
        {
            this.artifact = artifact;
            this.currentVersion = currentVersion;
            this.latestMajor = latestMajor;
            this.latestMinor = latestMinor;
            this.latestIncremental = latestIncremental;
        }

        /**
         * @return the artifact
         */
        public Artifact getArtifact()
        {
            return artifact;
        }

        /**
         * @return the currentVersion
         */
        public ArtifactVersion getCurrentVersion()
        {
            return currentVersion;
        }

        /**
         * @return the latestMajor
         */
        public ArtifactVersion getLatestMajor()
        {
            return latestMajor;
        }

        /**
         * @return the latestMinor
         */
        public ArtifactVersion getLatestMinor()
        {
            return latestMinor;
        }

        /**
         * @return the latestIncremental
         */
        public ArtifactVersion getLatestIncremental()
        {
            return latestIncremental;
        }
    }
}
