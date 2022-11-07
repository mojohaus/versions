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

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.SegmentUtils;

import static java.util.Collections.singletonList;
import static java.util.Optional.empty;

/**
 * Replaces any <em>release</em> versions (i.e. versions that are not snapshots and do not
 * have a year-month-day suffix) with the latest <em>release</em> version. This goal
 * will <em>not</em> replace versions of dependencies which use snapshots
 * or versions with a year-month-day suffix.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-latest-releases", threadSafe = true )
public class UseLatestReleasesMojo
    extends UseLatestVersionsMojoBase
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.2
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "true" )
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 1.2
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "true" )
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 1.2
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    protected boolean allowIncrementalUpdates = true;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseLatestReleasesMojo( RepositorySystem repositorySystem,
                                     MavenProjectBuilder projectBuilder,
                                     ArtifactMetadataSource artifactMetadataSource,
                                     WagonManager wagonManager,
                                     ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useLatestReleases( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useLatestReleases( pom, getProject().getDependencies() );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useLatestReleases( pom, singletonList( getParentDependency() ) );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useLatestReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
            throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment( allowMajorUpdates,
                allowMinorUpdates, allowIncrementalUpdates, getLog() );

        useLatestVersions( pom, dependencies,
                ( dep, versions ) ->
                {
                    try
                    {
                        return getLastFiltered( versions.getNewerVersions( dep.getVersion(), unchangedSegment,
                                false, false ), dep );
                    }
                    catch ( InvalidSegmentException e )
                    {
                        getLog().warn( String.format( "Skipping the processing of %s:%s:%s due to: %s",
                                dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), e.getMessage() ) );
                    }
                    return empty();
                }, "useLatestReleases",
                dep -> !SNAPSHOT_REGEX.matcher( dep.getVersion() ).matches() );
    }

    /**
     * Returns the last element of the given {@link ArtifactVersion} array such as every version of the array
     * is included in the {@code includes} and excluded by the {@code excludes} filters
     *
     * @param newer array of {@link ArtifactVersion} with newer versions
     * @param dependency dependency prototype to create the artifacts from
     * @return the newest version fulfilling the criteria
     */
    private Optional<ArtifactVersion> getLastFiltered( ArtifactVersion[] newer, Dependency dependency )
    {
        return Arrays.stream( newer )
                .filter( version ->
                {
                    Artifact artefactWithNewVersion =
                            new DefaultArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                                    VersionRange.createFromVersion( version.toString() ), dependency.getScope(),
                                    dependency.getType(), null, new DefaultArtifactHandler(), false );
                    return isIncluded( artefactWithNewVersion );
                } )
                .reduce( ( v1, v2 ) -> v2 );
    }

}
