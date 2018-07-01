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
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Replaces any -SNAPSHOT versions with a release version, older if necessary (if there has been a release).
 *
 * @author Stephen Connolly
 * @since 2.2
 */
@Mojo( name = "force-releases", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class ForceReleasesMojo
    extends ParentUpdatingDependencyUpdateMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------


    @Override
    protected void setVersions(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        useReleases(pom, dependencies);
    }

    private void useReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        Iterator<Dependency> i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = i.next();

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
                if ( versions.containsVersion( releaseVersion ) )
                {
                    setVersion(pom, dep, version, artifact, new DefaultArtifactVersion(releaseVersion));
                }
                else
                {
                    ArtifactVersion[] v = versions.getVersions( false );
                    if ( v.length == 0 )
                    {
                        getLog().info( "No release of " + toString( dep ) + " to force." );
                    }
                    else {
                        setVersion(pom, dep, version, artifact, v[v.length - 1]);
                    }
                }
            }
        }
    }

}
