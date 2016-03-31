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
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Compare dependency versions of the current project to dependencies or dependency management of a remote repository
 * project. Can optionally update locally the project instead of reporting the comparison
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
     * The groupId, artifactId, and version of the remote project (POM) to which we are comparing. This should be in the
     * form "groupId:artifactId:version"
     *
     * @parameter property="remotePom"
     * @required true
     */
    protected String remotePom;

    /**
     * Ignore the list of remote dependencies and only compare the remote dependencyManagement
     *
     * @parameter property="ignoreRemoteDependencies" default-value="false"
     */
    protected boolean ignoreRemoteDependencies;

    /**
     * Ignore the remote dependency management and only check against the actual dependencies of the remote project
     *
     * @parameter property="ignoreRemoteDependencyManagement" default-value="false"
     */
    protected boolean ignoreRemoteDependencyManagement;

    /**
     * Update dependency versions in the current POM.
     *
     * @parameter property="updateDependencies" default-value="false"
     */
    protected boolean updateDependencies;

    /**
     * Update dependency versions stored in properties
     *
     * @parameter property="updatePropertyVersions" default-value="false"
     */
    protected boolean updatePropertyVersions;

    /**
     * Display the dependency version differences on the command line, but do not update the versions in the current
     * pom. If updateDependencies is set to "true" this will automatically be set to false.
     *
     * @parameter property="reportMode" default-value="true"
     */
    protected boolean reportMode;

    /**
     * If the output file is set, the diff report will be written to this file.
     *
     * @parameter property="reportOutputFile"
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

        String[] remotePomParts = this.remotePom.split( ":" );
        if ( remotePomParts.length != 3 )
        {
            throw new MojoFailureException( " Invalid format for remotePom: " + remotePom );
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

        Map<String, Dependency> remoteDepsMap = new HashMap<String, Dependency>();
        if ( !ignoreRemoteDependencyManagement )
        {
            List<Dependency> remoteProjectDepMgmtDeps = ( remoteMavenProject.getDependencyManagement() == null ) ? null
                            : remoteMavenProject.getDependencyManagement().getDependencies();
            mapDependencies( remoteDepsMap, remoteProjectDepMgmtDeps );
        }
        if ( !ignoreRemoteDependencies )
        {
            List<Dependency> remoteProjectDeps = remoteMavenProject.getDependencies();
            mapDependencies( remoteDepsMap, remoteProjectDeps );
        }

        List<String> totalDiffs = new ArrayList<String>();
        List<String> propertyDiffs = new ArrayList<String>();
        if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
        {
            List<String> depManDiffs =
                compareVersions( pom, getProject().getDependencyManagement().getDependencies(), remoteDepsMap );
            totalDiffs.addAll( depManDiffs );
        }
        if ( isProcessingDependencies() )
        {
            List<String> depDiffs = compareVersions( pom, getProject().getDependencies(), remoteDepsMap );
            totalDiffs.addAll( depDiffs );
        }
        if ( updatePropertyVersions )
        {
            Map<Property, PropertyVersions> versionProperties =
                this.getHelper().getVersionPropertiesMap( getProject(), null, null, null, true );
            List<String> diff = updatePropertyVersions( pom, versionProperties, remoteDepsMap );
            propertyDiffs.addAll( diff );
        }

        if ( reportMode )
        {
            getLog().info( "The following differences were found:" );
            if ( totalDiffs.size() == 0 )
            {
                getLog().info( "  none" );
            }
            else
            {
                for ( String totalDiff : totalDiffs )
                {
                    getLog().info( "  " + totalDiff );
                }
            }
            getLog().info( "The following property differences were found:" );
            if ( propertyDiffs.size() == 0 )
            {
                getLog().info( "  none" );
            }
            else
            {
                for ( String propertyDiff : propertyDiffs )
                {
                    getLog().info( "  " + propertyDiff );
                }
            }
        }

        if ( reportOutputFile != null )
        {
            writeReportFile( totalDiffs, propertyDiffs );
        }

    }

    /**
     * Compare the dependency versions of the current project with the dependency versions of a remote project
     *
     * @throws XMLStreamException
     */
    private List<String> compareVersions( ModifiedPomXMLEventReader pom, List<Dependency> dependencies,
                                          Map<String, Dependency> remoteDependencies )
                                              throws MojoExecutionException, XMLStreamException
    {
        List<String> updates = new ArrayList<String>();
        for ( Dependency dep : dependencies )
        {
            Artifact artifact = this.toArtifact( dep );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            Dependency remoteDep = remoteDependencies.get( dep.getManagementKey() );
            if ( remoteDep != null )
            {
                String remoteVersion = remoteDep.getVersion();

                if ( !dep.getVersion().equals( remoteVersion ) )
                {
                    StringBuilder buf = writeDependencyDiffMessage( dep, remoteVersion );
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

    /**
     * Updates the properties holding a version if necessary.
     */
    private List<String> updatePropertyVersions( ModifiedPomXMLEventReader pom,
                                                 Map<Property, PropertyVersions> versionProperties,
                                                 Map<String, Dependency> remoteDependencies )
                                                     throws XMLStreamException
    {
        List<String> result = new ArrayList<String>();
        for ( Map.Entry<Property, PropertyVersions> entry : versionProperties.entrySet() )
        {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            String candidateVersion = computeCandidateVersion( remoteDependencies, property, version );
            if ( candidateVersion != null )
            {
                String originalVersion = version.getAssociations()[0].getArtifact().getVersion(); // Yekes
                if ( !candidateVersion.equals( originalVersion ) ) // Update needed
                {
                    result.add( writeDiffMessage( property.getName(), originalVersion, candidateVersion ).toString() );
                    if ( !reportMode
                        && PomHelper.setPropertyVersion( pom, null, property.getName(), candidateVersion ) )
                    {
                        getLog().info( "Updated ${" + property.getName() + "} from " + originalVersion + " to "
                            + candidateVersion );
                    }
                }
            }
        }
        return result;
    }

    /**
     * Returns the candidate version to use for the specified property.
     * <p/>
     * The dependencies currently linked to the property must all be defined by the remote POM and they should refer to
     * the same version. If that's the case, that same version is returned. Otherwise, <tt>null</tt> is returned
     * indicating that there is no candidate.
     *
     * @param remoteDependencies the remote dependencies
     * @param property the property to update
     * @param propertyVersions the association
     * @return the candidate version or <tt>null</tt> if there isn't any
     */
    private String computeCandidateVersion( Map<String, Dependency> remoteDependencies, Property property,
                                            PropertyVersions propertyVersions )
    {
        String candidateVersion = null;
        for ( ArtifactAssociation artifactAssociation : propertyVersions.getAssociations() )
        {
            String id = generateId( artifactAssociation.getArtifact() );
            Dependency dependency = remoteDependencies.get( id );
            if ( dependency == null )
            {
                getLog().info( "Not updating ${" + property.getName() + "}: no info for " + id );
                return null;
            }
            else
            {
                if ( candidateVersion == null )
                {
                    candidateVersion = dependency.getVersion();
                }
                else if ( !candidateVersion.equals( dependency.getVersion() ) )
                {
                    getLog().warn( "Could not update ${" + property.getName() + "}: version mismatch" );
                    return null;
                }
            }
        }
        return candidateVersion;
    }

    private void writeReportFile( List<String> dependenciesUpdate, List<String> propertiesUpdate )
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
            pw.println( "The following differences were found:" );
            pw.println();
            if ( dependenciesUpdate.size() == 0 )
            {
                pw.println( "  none" );
            }
            else
            {
                for ( String dependencyUpdate : dependenciesUpdate )
                {
                    pw.println( "  " + dependencyUpdate );
                }
            }
            pw.println();
            pw.println( "The following property differences were found:" );
            pw.println();
            if ( propertiesUpdate.size() == 0 )
            {
                pw.println( "  none" );
            }
            else
            {
                for ( String propertyUpdate : propertiesUpdate )
                {
                    pw.println( "  " + propertyUpdate );
                }
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
    private StringBuilder writeDependencyDiffMessage( Dependency dep, String remoteVersion )
    {
        String id = dep.getGroupId() + ":" + dep.getArtifactId();
        return writeDiffMessage( id, dep.getVersion(), remoteVersion );
    }

    private StringBuilder writeDiffMessage( String id, String originalVersion, String targetVersion )
    {
        StringBuilder buf = new StringBuilder();
        buf.append( id );
        buf.append( ' ' );
        int padding = INFO_PAD_SIZE - originalVersion.length() - targetVersion.length() - 4;
        while ( buf.length() < padding )
        {
            buf.append( '.' );
        }
        buf.append( ' ' );
        buf.append( originalVersion );
        buf.append( " -> " );
        buf.append( targetVersion );
        return buf;
    }

    /**
     * Add a list of dependencies to a Map for easy access
     *
     * @param map
     * @param deps
     */
    private void mapDependencies( Map<String, Dependency> map, List<Dependency> deps )
    {
        if ( deps != null )
        {
            for ( Dependency nextDep : deps )
            {
                map.put( nextDep.getManagementKey(), nextDep );
            }
        }
    }

    /**
     * Creates a key that is similar to what {@link Dependency#getManagementKey()} generates for a dependency.
     */
    private static String generateId( Artifact artifact )
    {
        StringBuilder sb = new StringBuilder();
        sb.append( artifact.getGroupId() ).append( ":" ).append( artifact.getArtifactId() ).append( ":" ).append( artifact.getType() );
        if ( artifact.getClassifier() != null )
        {
            sb.append( ":" ).append( artifact.getClassifier() );
        }
        return sb.toString();
    }

}