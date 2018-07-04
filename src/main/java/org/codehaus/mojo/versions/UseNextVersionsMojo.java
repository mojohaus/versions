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
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.change.VersionChange;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-next-versions", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseNextVersionsMojo
    extends ParentUpdatingDependencyUpdateMojo
{

    // ------------------------------ METHODS --------------------------

    @Override
    Collection<VersionChange> getVersionChanges(Collection<ArtifactIdentifier> artifacts) throws MojoExecutionException, ArtifactMetadataRetrievalException
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
            Artifact artifact = dep.getArtifact( getProject(), getHelper() );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            getLog().debug( "Looking for newer versions of " + dep );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            ArtifactVersion[] newer = versions.getNewerVersions( version, Boolean.TRUE.equals( allowSnapshots ) );
            if ( newer.length > 0 )
            {
                final VersionChange versionChange = new VersionChange( artifact.getGroupId(), artifact.getArtifactId(), version, newer[0].toString() );
                versionsToChange.add( versionChange );
            }
        }
        return versionsToChange;
    }

}