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
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.change.VersionChange;

import java.util.ArrayList;
import java.util.Collection;
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
    Collection<VersionChange> getVersionChanges(Collection<ArtifactIdentifier> artifacts)
        throws MojoExecutionException, ArtifactMetadataRetrievalException
    {
        final Collection<VersionChange> versionsToChange = new ArrayList<>();

        for ( ArtifactIdentifier dep : artifacts )
        {
            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + dep );
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.matches() )
            {
                String releaseVersion = versionMatcher.group( 1 );
                Artifact artifact = dep.getArtifact( getProject(), getHelper() );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                getLog().debug( "Looking for a release of " + dep );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                if ( versions.containsVersion( releaseVersion ) )
                {
                    final VersionChange versionChange = new VersionChange( artifact.getGroupId(), artifact.getArtifactId(), version, new DefaultArtifactVersion( releaseVersion ).toString() );
                    versionsToChange.add( versionChange );
                }
                else
                {
                    ArtifactVersion[] v = versions.getVersions( false );
                    if ( v.length == 0 )
                    {
                        getLog().info( "No release of " + dep + " to force." );
                    }
                    else
                    {
                        final VersionChange versionChange = new VersionChange( artifact.getGroupId(), artifact.getArtifactId(), version, v[v.length - 1].toString() );
                        versionsToChange.add( versionChange );
                    }
                }
            }
        }
        return versionsToChange;
    }

}
