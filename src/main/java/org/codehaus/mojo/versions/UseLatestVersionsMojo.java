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
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @goal use-latest-versions
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UseLatestVersionsMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException
     *          when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException
     *          when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
        {
            useLatestVersions( pom, getProject().getDependencyManagement().getDependencies() );
        }
        if ( isProcessingDependencies() )
        {
            useLatestVersions( pom, getProject().getDependencies() );
        }
    }

    private void useLatestVersions( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException
    {
        Iterator i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = (Dependency) i.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            String version = dep.getVersion();
            Artifact artifact = this.toArtifact( dep );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            getLog().debug( "Looking for newer versions of " + toString( dep ) );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            ArtifactVersion[] newer = versions.getNewerVersions( version, Boolean.TRUE.equals( allowSnapshots ) );
            if ( newer.length > 0 )
            {
                String newVersion = newer[newer.length - 1].toString();
                if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version, newVersion ) )
                {
                    getLog().info( "Updated " + toString( dep ) + " to version " + newVersion );
                }
            }
        }
    }

}