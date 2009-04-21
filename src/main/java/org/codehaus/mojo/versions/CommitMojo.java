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


import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.FileUtils;

import java.io.File;
import java.io.IOException;

/**
 * Removes the initial backup of the pom, thereby accepting the changes.
 *
 * @author Stephen Connolly
 * @goal commit
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class CommitMojo
    extends AbstractMojo
{
    /**
     * The Maven Project.
     *
     * @parameter expression="${project}"
     * @required
     * @readonly
     * @since 1.0-alpha-1
     */
    private MavenProject project;


    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        File outFile = project.getFile();
        File backupFile = new File( outFile.getParentFile(), outFile.getName() + ".versionsBackup" );

        if ( backupFile.exists() )
        {
            getLog().info( "Accepting all changes to " + outFile );
            try
            {
                FileUtils.forceDelete( backupFile );
            }
            catch ( IOException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
        }
    }
}
