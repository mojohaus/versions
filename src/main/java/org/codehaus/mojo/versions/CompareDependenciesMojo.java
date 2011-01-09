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
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Compare dependency versions of the current project to dependencies or dependency management of a remote repository
 * project.
 * 
 * @author Paul Gier
 * @goal compare-dependencies
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.3
 */
public class CompareDependenciesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    /**
     * The width to pad info messages.
     * 
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 68;

    /**
     * The groupId, artifactId, and version of the remote project (POM) to which we are comparing.  This
     * should be in the form "groupId:artifactId:version"
     * 
     * @parameter expression="${remotePom}"
     * @required true
     */
    protected String remotePom;

    /**
     * Ignore the list of remote dependencies and only compare the remote dependencyManagement
     * 
     * @parameter defaul-value="false" expression="${ignoreRemoteDependencies}"
     */
    protected boolean ignoreRemoteDependencies;

    /**
     * Ignore the remote dependency management and only check against the actual dependencies of the remote project
     * 
     * @parameter default-value="false" expression="${ignoreRemoteDependencyManagement}"
     */
    protected boolean ignoreRemoteDependencyManagement;

    /**
     * Update dependency versions in the current POM.
     * 
     * @parameter default-value="false" expression="${updateDependencies}"
     */
    protected boolean updateDependencies;

    /**
     * Update dependency versions stored in properties
     * 
     * @parameter default-value="false" expression="${updatePropertyVersions}"
     */
    protected boolean updatePropertyVersions;

    /**
     * Display the dependency version differences on the command line, but do not update the versions in the current
     * pom. If updateDependencies is set to "true" this will automatically be set to false.
     * 
     * @parameter default-value="true" expression="${reportMode}"
     */
    protected boolean reportMode;

    /**
     * If the output file is set, the diff report will be written to this file.
     * 
     * @parameter default-value="null" expression="${reportOutputFile}"
     */
    protected File reportOutputFile;

    /**
     * The project builder used to initialize the remote project.
     * 
     * @component
     */
    protected MavenProjectBuilder mavenProjectBuilder;

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException Something wrong with the plugin itself
     * @throws org.apache.maven.plugin.MojoFailureException The plugin detected an error in the build
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( this.ignoreRemoteDependencies && this.ignoreRemoteDependencyManagement )
        {
            throw new MojoFailureException( " ignoreRemoteDependencies and ignoreRemoteDependencyManagement"
                + "are both set to true.  At least one of these needs to be false " );
        }

        if ( updateDependencies )
        {
            reportMode = false;
        }

        String [] remotePomParts = this.remotePom.split( ":" );
        if ( remotePomParts.length != 3 )
        {
            throw new MojoFailureException(" Invalid format for remotePom: " + remotePom );
        }
        String rGroupId = remotePomParts[0];
        String rArtifactId = remotePomParts[1];
        String rVersion = remotePomParts[2];

        Dependency remoteDependency = new Dependency();
        remoteDependency.setGroupId( rGroupId );
        remoteDependency.setArtifactId( rArtifactId );
        remoteDependency.setVersion( rVersion );

        Artifact remoteArtifact = this.toArtifact( remoteDependency );
        MavenProject remoteMavenProject = null;
        try
        {
            remoteMavenProject =
                mavenProjectBuilder.buildFromRepository( remoteArtifact, remoteArtifactRepositories, localRepository );
        }
        catch ( ProjectBuildingException e )
        {
            throw new MojoExecutionException( "Unable to build remote project " + remoteArtifact, e );
        }

        Map remoteDepsMap = new HashMap();
        if ( !ignoreRemoteDependencyManagement )
        {
            List remoteProjectDepMgmtDeps = remoteMavenProject.getDependencyManagement().getDependencies();
            mapDependencies( remoteDepsMap, remoteProjectDepMgmtDeps );
        }
        if ( !ignoreRemoteDependencies )
        {
            List remoteProjectDeps = remoteMavenProject.getDependencies();
            mapDependencies( remoteDepsMap, remoteProjectDeps );
        }

        List totalDiffs = new ArrayList();
        if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
        {
            List depManDiffs = compareVersions( pom, getProject().getDependencyManagement().getDependencies(), remoteDepsMap );
            totalDiffs.addAll( depManDiffs );
        }
        if ( isProcessingDependencies() )
        {
            List depDiffs = compareVersions( pom, getProject().getDependencies(), remoteDepsMap );
            totalDiffs.addAll( depDiffs );
        }
        
        if ( reportMode )
        {
            getLog().info( "The following differences were found:" );
            Iterator iter = totalDiffs.iterator();
            while ( iter.hasNext() )
            {
                getLog().info( "  " + iter.next() );
            }
        }

        if ( reportOutputFile != null )
        {
            writeReportFile( totalDiffs );
        }
        
    }

    /**
     * Compare the dependency versions of the current project with the dependency versions of a remote project
     * 
     * @throws XMLStreamException
     */
    private List compareVersions( ModifiedPomXMLEventReader pom, List dependencies, Map remoteDependencies )
        throws MojoExecutionException, XMLStreamException
    {
        List updates = new ArrayList();
        Iterator iter = dependencies.iterator();
        while ( iter.hasNext() )
        {
            Dependency dep = (Dependency) iter.next();

            Artifact artifact = this.toArtifact( dep );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            Dependency remoteDep = (Dependency) remoteDependencies.get( dep.getManagementKey() );
            if ( remoteDep != null )
            {
                String remoteVersion = remoteDep.getVersion();

                if ( !dep.getVersion().equals( remoteVersion ) )
                {
                    StringBuffer buf = writeDependencyDiffMessage( dep, remoteVersion );
                    updates.add( buf.toString() );
                    if ( !reportMode )
                    {
                        if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(),
                                                             dep.getVersion(), remoteVersion ) )
                        {
                            getLog().info( "Updated " + toString( dep ) + " to version " + remoteVersion );
                        }
                    }

                }

            }
        }
        
        return updates;

    }

    private void writeReportFile( List updates )
        throws MojoExecutionException
    {
        if ( !reportOutputFile.getParentFile().exists() )
        {
            reportOutputFile.getParentFile().mkdirs();
        }

        FileWriter fw = null;
        PrintWriter pw = null;
        try
        {
            fw = new FileWriter( reportOutputFile );
            pw = new PrintWriter( fw );
            Iterator iter = updates.iterator();
            pw.println( "The following differences were found:" );
            pw.println();
            while ( iter.hasNext() )
            {
                pw.println( "  " + iter.next() );
            }
            pw.close();
            fw.close();
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( "Unable to write report file. ", e );
        }
        finally
        {
            if ( pw != null )
            {
                pw.close();
            }
            if ( fw != null )
            {
                try
                {
                    fw.close();
                }
                catch ( IOException io )
                {
                    // Ignore
                }
            }
        }

    }

    /**
     * Create a simple message describing the version diff
     * 
     * @param dep
     * @param remoteVersion
     * @return The message
     */
    private StringBuffer writeDependencyDiffMessage( Dependency dep, String remoteVersion )
    {
        StringBuffer buf = new StringBuffer();
        buf.append( dep.getGroupId() ).append( ':' );
        buf.append( dep.getArtifactId() );
        buf.append( ' ' );
        int padding = INFO_PAD_SIZE - dep.getVersion().length() - remoteVersion.length() - 4;
        while ( buf.length() < padding )
        {
            buf.append( '.' );
        }
        buf.append( ' ' );
        buf.append( dep.getVersion() );
        buf.append( " -> " );
        buf.append( remoteVersion );
        return buf;
    }

    /**
     * Add a list of dependencies to a Map for easy access
     * 
     * @param map
     * @param deps
     */
    private void mapDependencies( Map map, List deps )
    {
        Iterator iter = deps.iterator();
        while ( iter.hasNext() )
        {
            Dependency nextDep = (Dependency) iter.next();
            map.put( nextDep.getManagementKey(), nextDep );
        }
    }
}