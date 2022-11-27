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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.codehaus.mojo.versions.api.PomHelper;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

/**
 * Restores the pom from the initial backup.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "revert", threadSafe = true )
public class RevertMojo extends AbstractMojo
{
    /**
     * The {@link MavenSession} instance
     *
     * @since 2.14.0
     */
    @Parameter( defaultValue = "${session}", required = true, readonly = true )
    private MavenSession session;

    /**
     * Whether to start processing at the local aggregation root (which might be a parent module
     * of that module where Maven is executed in, and the version change may affect parent and sibling modules).
     * Setting to false makes sure only the module (and it's submodules) where Maven is executed for is affected.
     *
     * @since 2.13.0
     */
    @Parameter( property = "processFromLocalAggregationRoot", defaultValue = "true" )
    private boolean processFromLocalAggregationRoot;

    /**
     * The (injected) {@link ProjectBuilder} instance
     *
     * @since 2.14.0
     */
    protected final ProjectBuilder projectBuilder;

    @Inject
    protected RevertMojo( ProjectBuilder projectBuilder )
    {
        this.projectBuilder = projectBuilder;
    }

    public void execute() throws MojoExecutionException, MojoFailureException
    {
        final MavenProject projectToProcess = !processFromLocalAggregationRoot
                ? PomHelper.getLocalRoot( projectBuilder, session, getLog() )
                : session.getCurrentProject();

        getLog().info( "Local aggregation root: " + projectToProcess.getBasedir() );
        Set<String> reactor = PomHelper.getAllChildModules( projectToProcess, getLog() );
        reactor.add( "." );

        reactor.forEach( entry ->
        {
            Path pomFile = projectToProcess.getBasedir().toPath().resolve( entry ).resolve( "pom.xml" ).normalize();
            getLog().debug( "Processing:" + pomFile );
            Path backupFile = Paths.get( pomFile + ".versionsBackup" );
            if ( Files.exists( backupFile ) )
            {
                getLog().info( "Restoring " + pomFile + " from " + backupFile );
                try
                {
                    Files.copy( backupFile, pomFile, REPLACE_EXISTING );
                    try
                    {
                        Files.delete( backupFile );
                    }
                    catch ( IOException e )
                    {
                        getLog().warn( "Error deleting " + backupFile );
                    }
                }
                catch ( IOException e )
                {
                    getLog().warn( "Error copying " + backupFile + " onto " + pomFile );
                }
            }
        } );
    }
}
