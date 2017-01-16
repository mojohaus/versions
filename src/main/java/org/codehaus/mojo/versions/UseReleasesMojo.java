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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
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
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any -SNAPSHOT versions with the corresponding release version (if it has been released).
 *
 * @author Stephen Connolly
 * @goal use-releases
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UseReleasesMojo extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * Whether to check for releases within the range.
     *
     * @parameter property="allowRangeMatching" default-value="false"
     * @since 2.3
     */
    private Boolean allowRangeMatching;

    /**
     * Whether to fail if a SNAPSHOT could not be replaced
     *
     * @parameter property="failIfNotReplaced" default-value="false"
     * @since 2.3
     */
    private Boolean failIfNotReplaced;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useReleases( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( isProcessingDependencies() )
            {
                useReleases( pom, getProject().getDependencies() );
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    private void useReleases( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
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
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.matches() )
            {
                String releaseVersion = versionMatcher.group( 1 );
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                getLog().debug( "Looking for a release of " + toString( dep ) );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                if ( !allowRangeMatching ) // standard behaviour
                {
                    if (versions.containsVersion(releaseVersion)) {
                        if (PomHelper.setDependencyVersion(pom, dep.getGroupId(), dep.getArtifactId(), version,
                                releaseVersion)) {
                            getLog().info("Updated " + toString(dep) + " to version " + releaseVersion);
                        }
                    } else if ( failIfNotReplaced ) {
                        throw new NoSuchElementException("No matching release of " + toString(dep) + " found for update.");
                    }
                }
                else
                {
                    ArtifactVersion finalVersion = null;
                    for (ArtifactVersion proposedVersion : versions.getVersions( false )) {
                        if (proposedVersion.toString().startsWith( releaseVersion )) {
                            getLog().debug("Found matching version for " + toString(dep) + " to version " + releaseVersion);
                            finalVersion = proposedVersion;
                        }
                    }

                    if ( finalVersion != null )
                    {
                        if (PomHelper.setDependencyVersion(pom, dep.getGroupId(), dep.getArtifactId(), version,
                                finalVersion.toString()))
                        {
                            getLog().info("Updated " + toString(dep) + " to version " + finalVersion.toString());
                        }
                    }
                    else
                    {
                        getLog().info("No matching release of " + toString(dep) + " to update via rangeMatching.");
                        if ( failIfNotReplaced ) {
                            throw new NoSuchElementException("No matching release of " + toString(dep) + " found for update via rangeMatching.");
                        }
                    }

                }
                else if ( allowRangeMatching )
                {

                    ArtifactVersion finalVersion = null;
                    for (ArtifactVersion proposedVersion : versions.getVersions( false )) {
                        if (proposedVersion.toString().startsWith( releaseVersion )) {
                            getLog().debug("Found matching version for " + toString(dep) + " to version " + releaseVersion);
                            finalVersion = proposedVersion;
                        }
                    }

                    if ( finalVersion != null )
                    {
                        if (PomHelper.setDependencyVersion(pom, dep.getGroupId(), dep.getArtifactId(), version,
                                finalVersion.toString()))
                        {
                            getLog().info("Updated " + toString(dep) + " to version " + finalVersion.toString());
                        }
                    } else
                    {
                        getLog().info("No matching release of " + toString(dep) + " to force.");
                        if ( failIfNotReplaced ) {
                            throw new NoSuchElementException("No matching release of " + toString(dep) + " to found for update.");
                        }
                    }

                }
            }
        }
    }


}
