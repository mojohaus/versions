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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Updates the parent to the latest version.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-parent
 * @requires-project
 * @description Sets the parent version to the latest parent version.
 */
public class UpdateParentMojo
    extends AbstractVersionsUpdaterMojo
{

// -------------------------- OTHER METHODS --------------------------

    /**
     * {@inheritDoc}
     */
    protected boolean update( StringBuffer pom )
        throws MojoExecutionException, MojoFailureException
    {
        if ( getProject().getParent() == null )
        {
            getLog().info( "Project does not have a parent" );
            return false;
        }

        if ( reactorProjects.contains( getProject().getParent() ) )
        {
            getLog().info( "Project's parent is part of the reactor" );
            return false;
        }

        String currentVersion = getProject().getParent().getVersion();
        String version = currentVersion;

        if ( parentVersion != null )
        {
            version = parentVersion;
        }

        VersionRange versionRange = null;
        try
        {
            versionRange = VersionRange.createFromVersionSpec( version );
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( "Invalid version range specification: " + version, e );
        }

        Artifact artifact = artifactFactory.createDependencyArtifact( getProject().getParent().getGroupId(),
                                                                      getProject().getParent().getArtifactId(),
                                                                      versionRange, "pom", null, null );

        ArtifactVersion artifactVersion = findLatestVersion( artifact, versionRange );

        if ( !shouldApplyUpdate( artifact, currentVersion, artifactVersion ) )
        {
            return false;
        }

        getLog().info( "Updating parent from " + currentVersion + " to " + artifactVersion.toString() );

        boolean madeReplacement = false;
        int startIndex = 0;
        int endIndex;
        do
        {
            // do we have a parent section?
            startIndex = pom.indexOf( "<parent>", startIndex );
            if ( startIndex < 0 )
            {
                break;
            }
            endIndex = pom.indexOf( "</parent>", startIndex );

            getLog().debug( "Found a parent section" );

            int i1, i2, i3;

            // is it for our groupId?
            i1 = pom.indexOf( "<groupId>", startIndex );
            i2 = i1 >= 0 ? pom.indexOf( getProject().getParent().getGroupId(), i1 ) : -1;
            i3 = i2 >= 0 ? pom.indexOf( "</groupId>", i2 ) : -1;
            if ( i1 <= startIndex || i2 <= i1 || i3 <= i2 || i3 >= endIndex )
            {
                startIndex = endIndex;
                continue;
            }

            getLog().debug( "The parent section matches our parent's groupId" );

            // is it for our artifactId?
            i1 = pom.indexOf( "<artifactId>", startIndex );
            i2 = i1 >= 0 ? pom.indexOf( getProject().getParent().getArtifactId(), i1 ) : -1;
            i3 = i2 >= 0 ? pom.indexOf( "</artifactId>", i2 ) : -1;
            if ( i1 <= startIndex || i2 <= i1 || i3 <= i2 || i3 >= endIndex )
            {
                startIndex = endIndex;
                continue;
            }

            getLog().debug( "The parent section matches our parent's artifactId" );

            // is it for our version?
            i1 = pom.indexOf( "<version>", startIndex );
            i2 = i1 >= 0 ? pom.indexOf( getProject().getParent().getVersion(), i1 ) : -1;
            i3 = i2 >= 0 ? pom.indexOf( "</version>", i2 ) : -1;
            if ( i1 <= startIndex || i2 <= i1 || i3 <= i2 || i3 >= endIndex )
            {
                startIndex = endIndex;
                getLog().info( getProject().getParent().getVersion() + " " + i1 + " " + i2 + " " + i3 );
                continue;
            }

            getLog().debug( "The parent section matches our parent's version" );

            // ok, well update the version so
            pom.replace( i1, i3 + "</version>".length(), "<version>" + artifactVersion.toString() + "</version>" );
            getLog().debug( "Made an update from " + currentVersion + " to " + artifactVersion.toString() );
            madeReplacement = true;
            startIndex = endIndex;
        }
        while ( startIndex >= 0 );

        return madeReplacement;
    }

}