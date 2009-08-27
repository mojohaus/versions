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
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Displays all dependencies that have newer versions available.
 *
 * @author Stephen Connolly
 * @goal display-dependency-updates
 * @requiresProject true
 * @requiresDirectInvocation false
 * @since 1.0-alpha-1
 */
public class DisplayDependencyUpdatesMojo
    extends AbstractVersionsUpdaterMojo
{

// ------------------------------ FIELDS ------------------------------

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 72;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param dependencies         The set of dependencies.
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
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() ) &&
                    StringUtils.equals( t.getArtifactId(), c.getArtifactId() ) &&
                    ( t.getScope() == null || StringUtils.equals( t.getScope(), c.getScope() ) ) &&
                    ( t.getClassifier() == null || StringUtils.equals( t.getClassifier(), c.getClassifier() ) ) &&
                    ( c.getVersion() == null || t.getVersion() == null ||
                        StringUtils.equals( t.getVersion(), c.getVersion() ) ) )
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

// ------------------------ INTERFACE METHODS ------------------------

// --------------------- Interface Mojo ---------------------

    /**
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong in a very bad way
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        Set dependencyManagement = new TreeSet( new DependencyComparator() );
        dependencyManagement.addAll( getProject().getDependencyManagement() == null
            ? Collections.EMPTY_LIST
            : getProject().getDependencyManagement().getDependencies() );

        Set dependencies = new TreeSet( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );
        dependencies = removeDependencyManagment( dependencies, dependencyManagement );

        try
        {
            logUpdates( getHelper().lookupDependenciesUpdates( dependencyManagement, false ), "Dependency Management" );
            logUpdates( getHelper().lookupDependenciesUpdates( dependencies, false ), "Dependencies" );
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
            final ArtifactVersion current = versions.getCurrentVersion();
            final ArtifactVersion latest = versions.getNewestUpdate( UpdateScope.ANY );
            String right =
                " " + ( latest == null ? current.toString() : current.toString() + " -> " + latest.toString() );
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
        if ( usingCurrent.isEmpty() && !withUpdates.isEmpty() )
        {
            getLog().info( "No dependencies in " + section + " are using the newest version." );
            getLog().info( "" );
        }
        else if ( !usingCurrent.isEmpty() )
        {
            getLog().info( "The following dependencies in " + section + " are using the newest version:" );
            i = usingCurrent.iterator();
            while ( i.hasNext() )
            {
                getLog().info( (String) i.next() );
            }
            getLog().info( "" );
        }
        if ( withUpdates.isEmpty() && !usingCurrent.isEmpty() )
        {
            getLog().info( "No dependencies in " + section + " have newer versions." );
            getLog().info( "" );
        }
        else if ( !withUpdates.isEmpty() )
        {
            getLog().info( "The following dependencies in " + section + " have newer versions:" );
            i = withUpdates.iterator();
            while ( i.hasNext() )
            {
                getLog().info( (String) i.next() );
            }
            getLog().info( "" );
        }
    }


    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException
     *          when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // do nothing
    }


}