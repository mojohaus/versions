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
import org.apache.maven.artifact.versioning.OverConstrainedVersionException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any release versions with the next release version (if it has been released).
 *
 * @author Stephen Connolly
 * @goal use-next-releases
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UseNextReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

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
            useNextReleases( pom, getProject().getDependencyManagement().getDependencies() );
        }
        if ( isProcessingDependencies() )
        {
            useNextReleases( pom, getProject().getDependencies() );
        }
    }

    private void useNextReleases( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException
    {
        Iterator i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = (Dependency) i.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( !versionMatcher.matches() )
            {
                Artifact artifact = this.findArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                try
                {
                    ArtifactVersion[] newer = versions.getNewerVersions( artifact.getSelectedVersion(), false );
                    if ( newer.length > 0 )
                    {
                        if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                             newer[0].toString() ) )
                        {
                            getLog().debug( "Version set to " + newer[0].toString() + " for dependnecy: " + dep );
                        }
                    }
                }
                catch ( OverConstrainedVersionException e )
                {
                    getLog().warn( "This should never happen as your build should not work at all if this is thrown",
                                   e );
                }
            }
        }
    }

}