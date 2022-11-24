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

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecord;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.SegmentUtils;

import static java.util.Collections.singletonList;
import static java.util.Optional.empty;

/**
 * Replaces any release versions with the next snapshot version (if it has been deployed).
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "use-next-snapshots", threadSafe = true )
public class UseNextSnapshotsMojo
    extends UseLatestVersionsMojoBase
{

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "false" )
    private boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "false" )
    private boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     * @since 1.0-beta-1
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    private boolean allowIncrementalUpdates;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseNextSnapshotsMojo( RepositorySystem repositorySystem,
                                 org.eclipse.aether.RepositorySystem aetherRepositorySystem,
                                 MavenProjectBuilder projectBuilder,
                                 WagonManager wagonManager,
                                 Map<String, ChangeRecorder> changeRecorders )
    {
        super( repositorySystem, aetherRepositorySystem, projectBuilder, wagonManager, changeRecorders );
        // the below is necessary for UseLatestVersionsMojoBase.useLatestVersions to select snapshots
        allowSnapshots = true;
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException
    {
        try
        {
            DependencyManagement dependencyManagement =
                    PomHelper.getRawModel( getProject() ).getDependencyManagement();
            if ( dependencyManagement != null )
            {
                useNextSnapshots( pom, dependencyManagement.getDependencies(),
                                  ChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useNextSnapshots( pom, getProject().getDependencies(), ChangeRecord.ChangeKind.DEPENDENCY );
            }
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useNextSnapshots( pom, singletonList( getParentDependency() ),
                                  ChangeRecord.ChangeKind.PARENT );
            }
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useNextSnapshots( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies,
                                   ChangeRecord.ChangeKind changeKind )
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException
    {
        Optional<Segment>
                unchangedSegment = SegmentUtils.determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates,
                allowIncrementalUpdates, getLog() );

        useLatestVersions( pom, dependencies,
                           ( dep, versions ) ->
                {
                    try
                    {
                        return Arrays.stream( versions.getNewerVersions( dep.getVersion(), unchangedSegment,
                                        true, false ) )
                                .filter( v -> SNAPSHOT_REGEX.matcher( v.toString() ).matches() )
                                .findFirst();
                    }
                    catch ( InvalidSegmentException e )
                    {
                        getLog().info( "Ignoring " + toString( dep ) + " as the version number is too short" );
                        return empty();
                    }
                }, changeKind, dep -> !SNAPSHOT_REGEX.matcher( dep.getVersion() ).matches() );
    }
}
