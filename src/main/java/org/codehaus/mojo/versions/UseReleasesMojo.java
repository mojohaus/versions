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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
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
 * Replaces any -SNAPSHOT versions with the corresponding release version (if it has been released).
 *
 * @author Stephen Connolly
 * @goal use-releases
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-3
 */
public class UseReleasesMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * A comma separated list of group ids to update.
     *
     * @parameter expression="${includeGroupIds}"
     * @since 1.0-alpha-3
     */
    private String includeGroupIds = null;

    /**
     * A comma separated list of artifact ids to update.
     *
     * @parameter expression="${includeArtifactIds}"
     * @since 1.0-alpha-3
     */
    private String includeArtifactIds = null;

    /**
     * A comma separated list of group ids to not update.
     *
     * @parameter expression="${excludeGroupIds}"
     * @since 1.0-alpha-3
     */
    private String excludeGroupIds = null;

    /**
     * A comma separated list of artifact ids to not update.
     *
     * @parameter expression="${excludeArtifactIds}"
     * @since 1.0-alpha-3
     */
    private String excludeArtifactIds = null;

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
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( getProject().getDependencyManagement() != null )
        {
            useReleases( pom, getProject().getDependencyManagement().getDependencies() );
        }
        useReleases( pom, getProject().getDependencies() );
    }

    private void useReleases( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException
    {
        Iterator i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = (Dependency) i.next();
            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.matches() )
            {
                String releaseVersion = versionMatcher.group( 1 );
                try
                {
                    Artifact artifact = getHelper().createDependencyArtifact( dep );
                    if ( !getHelper().isIncluded( artifact, includeGroupIds, includeArtifactIds, excludeGroupIds,
                                                  excludeArtifactIds ) )
                    {
                        continue;
                    }
                    ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                    if ( versions.containsVersion( releaseVersion ) )
                    {
                        if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                             releaseVersion ) )
                        {
                            getLog().debug( "Version set to " + releaseVersion + " for dependnecy: " + dep );
                        }
                    }
                }
                catch ( InvalidVersionSpecificationException e )
                {
                    throw new MojoExecutionException( e.getMessage(), e );
                }
            }
        }
    }

}
