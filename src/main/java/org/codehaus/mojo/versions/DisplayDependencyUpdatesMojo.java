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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Displays all dependencies that have newer versions available.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo( name = "display-dependency-updates", requiresProject = true, requiresDirectInvocation = false )
public class DisplayDependencyUpdatesMojo
    extends AbstractVersionsDisplayMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 72;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.2
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    protected boolean processDependencyManagement;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.2
     */
    @Parameter( property = "processDependencies", defaultValue = "true" )
    protected boolean processDependencies;

    /**
     * Whether to show additional information such as dependencies that do not need updating.
     *
     * @since 2.1
     */
    @Parameter( property = "verbose", defaultValue = "false" )
    protected boolean verbose;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param dependencies The set of dependencies.
     * @param dependencyManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set removeDependencyManagment( Set dependencies, Set dependencyManagement )
    {
        Set result = new TreeSet( new DependencyComparator() );
        for ( Iterator i = dependencies.iterator(); i.hasNext(); )
        {
            Dependency c = (Dependency) i.next();
            boolean matched = false;
            Iterator j = dependencyManagement.iterator();
            while ( !matched && j.hasNext() )
            {
                Dependency t = (Dependency) j.next();
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() )
                    && ( t.getScope() == null || StringUtils.equals( t.getScope(), c.getScope() ) )
                    && ( t.getClassifier() == null || StringUtils.equals( t.getClassifier(), c.getClassifier() ) )
                    && ( c.getVersion() == null || t.getVersion() == null
                        || StringUtils.equals( t.getVersion(), c.getVersion() ) ) )
                {
                    matched = true;
                    break;
                }
            }
            if ( !matched )
            {
                result.add( c );
            }
        }
        return result;
    }

    public boolean isProcessingDependencyManagement()
    {
        // true if true or null
        return processDependencyManagement;
    }

    public boolean isProcessingDependencies()
    {
        return processDependencies;
    }

    public boolean isVerbose()
    {
        return verbose;
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();

        Set dependencyManagement = new TreeSet( new DependencyComparator() );
        if ( getProject().getDependencyManagement() != null )
        {

            List<Dependency> dependenciesFromPom = getProject().getDependencyManagement().getDependencies();
            for ( Dependency dependency : dependenciesFromPom )
            {
                getLog().debug( "dependency from pom: " + dependency.getGroupId() + ":" + dependency.getArtifactId()
                    + ":" + dependency.getVersion() );
                if ( dependency.getVersion() == null )
                {
                    // get parent and get the information from there.
                    if ( getProject().hasParent() )
                    {
                        getLog().debug( "Reading parent dependencyManagement information" );
                        if ( getProject().getParent().getDependencyManagement() != null )
                        {
                            List<Dependency> parentDeps =
                                getProject().getParent().getDependencyManagement().getDependencies();
                            for ( Dependency parentDep : parentDeps )
                            {
                                // only groupId && artifactId needed cause version is null
                                if ( dependency.getGroupId().equals( parentDep.getGroupId() )
                                    && dependency.getArtifactId().equals( parentDep.getArtifactId() )
                                    && dependency.getType().equals( parentDep.getType() ) )
                                {
                                    dependencyManagement.add( parentDep );
                                }
                            }
                        }
                    }
                    else
                    {
                        String message = "We can't get the version for the dependency " + dependency.getGroupId() + ":"
                            + dependency.getArtifactId() + " cause there does not exist a parent.";
                        getLog().error( message );
                        // Throw error cause we will not able to get a version for a dependency.
                        throw new MojoExecutionException( message );
                    }
                }
                else
                {
                    dependencyManagement.add( dependency );
                }
            }
        }

        Set dependencies = new TreeSet( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );

        if ( isProcessingDependencyManagement() )
        {
            dependencies = removeDependencyManagment( dependencies, dependencyManagement );
        }

        try
        {
            if ( isProcessingDependencyManagement() )
            {
                logUpdates( getHelper().lookupDependenciesUpdates( dependencyManagement, false ),
                            "Dependency Management" );
            }
            if ( isProcessingDependencies() )
            {
                logUpdates( getHelper().lookupDependenciesUpdates( dependencies, false ), "Dependencies" );
            }
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void logUpdates( Map updates, String section )
    {
        List withUpdates = new ArrayList();
        List usingCurrent = new ArrayList();
        Iterator i = updates.values().iterator();
        while ( i.hasNext() )
        {
            ArtifactVersions versions = (ArtifactVersions) i.next();
            String left = "  " + ArtifactUtils.versionlessKey( versions.getArtifact() ) + " ";
            final String current;
            ArtifactVersion latest;
            if ( versions.isCurrentVersionDefined() )
            {
                current = versions.getCurrentVersion().toString();
                latest = versions.getNewestUpdate( UpdateScope.ANY, allowSnapshots );
            }
            else
            {
                ArtifactVersion newestVersion =
                    versions.getNewestVersion( versions.getArtifact().getVersionRange(), allowSnapshots );
                current = versions.getArtifact().getVersionRange().toString();
                latest = newestVersion == null ? null
                                : versions.getNewestUpdate( newestVersion, UpdateScope.ANY, allowSnapshots );
                if ( latest != null
                    && ArtifactVersions.isVersionInRange( latest, versions.getArtifact().getVersionRange() ) )
                {
                    latest = null;
                }
            }
            String right = " " + ( latest == null ? current : current + " -> " + latest.toString() );
            List t = latest == null ? usingCurrent : withUpdates;
            if ( right.length() + left.length() + 3 > INFO_PAD_SIZE )
            {
                t.add( left + "..." );
                t.add( StringUtils.leftPad( right, INFO_PAD_SIZE ) );

            }
            else
            {
                t.add( StringUtils.rightPad( left, INFO_PAD_SIZE - right.length(), "." ) + right );
            }
        }
        if ( isVerbose() && usingCurrent.isEmpty() && !withUpdates.isEmpty() )
        {
            logLine( false, "No dependencies in " + section + " are using the newest version." );
            logLine( false, "" );
        }
        else if ( isVerbose() && !usingCurrent.isEmpty() )
        {
            logLine( false, "The following dependencies in " + section + " are using the newest version:" );
            i = usingCurrent.iterator();
            while ( i.hasNext() )
            {
                logLine( false, (String) i.next() );
            }
            logLine( false, "" );
        }
        if ( withUpdates.isEmpty() && !usingCurrent.isEmpty() )
        {
            logLine( false, "No dependencies in " + section + " have newer versions." );
            logLine( false, "" );
        }
        else if ( !withUpdates.isEmpty() )
        {
            logLine( false, "The following dependencies in " + section + " have newer versions:" );
            i = withUpdates.iterator();
            while ( i.hasNext() )
            {
                logLine( false, (String) i.next() );
            }
            logLine( false, "" );
        }
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }

}
