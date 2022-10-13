package org.codehaus.mojo.versions.xml;

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

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.utils.DependencyComparator;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * XML renderer for DependencyUpdatesReport creates an xml file in target directory and writes report about available
 * dependency/dependency management updates.
 *
 * @author Illia Dubinin
 * @since 2.4
 */
public class DependencyUpdatesXmlRenderer
{

    private static final String GROUP_ID = "groupId";

    private static final String ARTIFACT_ID = "artifactId";

    private static final String SCOPE = "scope";

    private static final String CLASSIFIER = "classifier";

    private static final String TYPE = "type";

    private static final String CURRENT_VERSION = "currentVersion";

    private static final String NEXT_VERSION = "nextVersion";

    private static final String STATUS = "status";

    private static final String OPEN_TAG = "<";

    private static final String CLOSE_TAG = ">";

    private static final String OPEN_CLOSING_TAG = "</";

    private static final String NL = "\n";

    private static final String TAB = "\t";

    private final Map<Dependency, ArtifactVersions> dependencyUpdates;

    private final Map<Dependency, ArtifactVersions> dependencyManagementUpdates;

    private final String outputFileName;

    public DependencyUpdatesXmlRenderer( Map<Dependency, ArtifactVersions> dependencyUpdates,
                                         Map<Dependency, ArtifactVersions> dependencyManagementUpdates,
                                         String outputFileName )
    {
        this.dependencyUpdates = dependencyUpdates;
        this.dependencyManagementUpdates = dependencyManagementUpdates;
        this.outputFileName = outputFileName;
    }

    /**
     * Makes report file with given name in target directory.
     *
     * @throws MavenReportException if something went wrong
     */
    public void render()
        throws MavenReportException
    {
        StringBuilder sb = new StringBuilder();
        sb.append( "<DependencyUpdatesReport>" ).append( NL );
        Map<Dependency, ArtifactVersions> allUpdates = new TreeMap<>( DependencyComparator.INSTANCE );
        allUpdates.putAll( dependencyManagementUpdates );
        allUpdates.putAll( dependencyUpdates );
        sb.append( getSummaryBlock( allUpdates.values() ) );
        sb.append( getDependencyInfoBlock( dependencyManagementUpdates, "dependencyManagements",
                                           "dependencyManagement" ) );
        sb.append( getDependencyInfoBlock( dependencyUpdates, "dependencies", "dependency" ) );
        sb.append( "</DependencyUpdatesReport>" ).append( NL );
        PrintWriter pw;
        try
        {
            pw = new PrintWriter( outputFileName, "UTF8" );
            pw.print( sb );
            pw.close();
        }
        catch ( IOException e )
        {
            throw new MavenReportException( "Cannot create xml report.", e );
        }
    }

    /**
     * Method wraps value in xml tag. In ex: to wrap foo in tag bar you have to pass foo as value and bar as tag. As a
     * result you will get: &lt;bar&gt;foo&lt;/bar&gt;
     *
     * @param value - string to wrap
     * @param tag   - name of tag
     * @return value wrapped in xml tag
     */

    public static String wrapElement( String value, String tag )
    {
        return OPEN_TAG + tag + CLOSE_TAG + value + OPEN_CLOSING_TAG + tag + CLOSE_TAG;
    }

    /**
     * Returns summary of dependency analysis result in xml format: current version, next available, next incremental,
     * next minor and next major versions.
     *
     * @param allUpdates all dependencies versions
     * @return summary in xml format
     */
    public static String getSummaryBlock( Collection<? extends ArtifactVersions> allUpdates )
    {
        int numInc = 0;
        int numMin = 0;
        int numMaj = 0;
        int numAny = 0;
        int numCur = 0;
        for ( ArtifactVersions details : allUpdates )
        {
            if ( details.getOldestUpdate( of( SUBINCREMENTAL ) ) != null )
            {
                numAny++;
            }
            else if ( details.getOldestUpdate( of( INCREMENTAL ) ) != null )
            {
                numInc++;
            }
            else if ( details.getOldestUpdate( of( MINOR ) ) != null )
            {
                numMin++;
            }
            else if ( details.getOldestUpdate( of( MAJOR ) ) != null )
            {
                numMaj++;
            }
            else
            {
                numCur++;
            }
        }

        String result = "\t<summary>%n" + "\t\t<usingLastVersion>%d</usingLastVersion>%n"
            + "\t\t<nextVersionAvailable>%d</nextVersionAvailable>%n"
            + "\t\t<nextIncrementalAvailable>%d</nextIncrementalAvailable>%n"
            + "\t\t<nextMinorAvailable>%d</nextMinorAvailable>%n" + "\t\t<nextMajorAvailable>%d</nextMajorAvailable>%n"
            + "\t</summary>%n";

        return String.format( result, numCur, numAny, numInc, numMin, numMaj );
    }

    /**
     * Returns xml report for current dependency state with following info: current version, next available version,
     * next incremental/minor/major if available and status ('incremental available', 'minor available', 'major
     * available' or 'no new available')
     *
     * @param versions version info for dependency
     * @return xml reports about current possible updates.
     */
    public static String getVersionsBlocks( ArtifactVersions versions )
    {
        StringBuilder sBuilder = new StringBuilder();
        sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( versions.isCurrentVersionDefined()
                        ? versions.getCurrentVersion().toString() : versions.getArtifact().getVersionRange().toString(),
                CURRENT_VERSION ) ).append( NL );
        ArtifactVersion nextVersion = versions.getOldestUpdate( empty() );
        if ( nextVersion != null )
        {
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( nextVersion.toString(),
                                                                                    NEXT_VERSION ) ).append( NL );

            String incrementalsBlock = getVersionsInScopeBlock( versions, of( INCREMENTAL ) );
            sBuilder.append( incrementalsBlock );
            String minorsBlock = getVersionsInScopeBlock( versions, of( MINOR ) );
            sBuilder.append( minorsBlock );
            String majorsBlock = getVersionsInScopeBlock( versions, of( MAJOR ) );
            sBuilder.append( majorsBlock );

            String status = null;
            if ( incrementalsBlock.length() > 0 )
            {
                status = "incremental available";
            }
            else if ( minorsBlock.length() > 0 )
            {
                status = "minor available";
            }
            else if ( majorsBlock.length() > 0 )
            {
                status = "major available";
            }

            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( status, STATUS ) ).append( NL );
        }
        else
        {
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( "no new available",
                                                                                    STATUS ) ).append( NL );
        }

        return sBuilder.toString();
    }

    private static String getDependencyInfoBlock( Map<Dependency, ArtifactVersions> dependencyUpdates, String blockName,
                                                  String subblockName )
    {
        StringBuilder sBuilder = new StringBuilder();
        sBuilder.append( TAB ).append( OPEN_TAG ).append( blockName ).append( CLOSE_TAG ).append( NL );
        dependencyUpdates.forEach( ( dep, value ) ->
        {
            sBuilder.append( TAB ).append( TAB ).append( OPEN_TAG ).append( subblockName ).append( CLOSE_TAG )
                    .append( NL );

            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( dep.getGroupId(),
                    GROUP_ID ) ).append( NL );
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( dep.getArtifactId(),
                    ARTIFACT_ID ) ).append( NL );
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( dep.getScope(),
                    SCOPE ) ).append( NL );
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( dep.getClassifier(),
                    CLASSIFIER ) ).append( NL );
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( dep.getType(),
                    TYPE ) ).append( NL );

            sBuilder.append( getVersionsBlocks( value ) );

            sBuilder.append( TAB ).append( TAB ).append( OPEN_CLOSING_TAG ).append( subblockName ).append( CLOSE_TAG )
                    .append( NL );
        } );
        sBuilder.append( TAB ).append( OPEN_CLOSING_TAG ).append( blockName ).append( CLOSE_TAG ).append( NL );
        return sBuilder.toString();
    }

    private static String getVersionsInScopeBlock( ArtifactVersions av, Optional<Segment> scope )
    {
        String versionsTag = scope.map( s -> s.name().toLowerCase() + "s" ).orElse( "any" );
        StringBuilder sBuilder = new StringBuilder();

        ArtifactVersion nextVersion = av.getOldestUpdate( scope );
        if ( nextVersion != null )
        {
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( OPEN_TAG ).append( versionsTag )
                .append( CLOSE_TAG ).append( NL );
            ArtifactVersion[] versions = av.getAllUpdates( scope );
            for ( ArtifactVersion version : versions )
            {
                sBuilder.append( TAB ).append( TAB ).append( TAB ).append( TAB )
                    .append( wrapElement( version.toString(),
                                          scope.map( s -> s.toString().toLowerCase() ).orElse( "any" ) ) )
                            .append( NL );
            }
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( OPEN_CLOSING_TAG ).append( versionsTag )
                .append( CLOSE_TAG ).append( NL );
        }

        return sBuilder.toString();
    }

}
