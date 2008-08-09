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
 * Updates properties to refer to the latest versions.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-properties
 * @requires-project
 * @description Sets properties to the latest versions of specific artifacts.
 */
public class UpdatePropertiesMojo
    extends AbstractVersionsUpdaterMojo
{

// -------------------------- OTHER METHODS --------------------------

    /**
     * {@inheritDoc}
     */
    protected boolean update( StringBuffer pom )
        throws MojoExecutionException, MojoFailureException
    {
        if ( linkItems == null || linkItems.length == 0 )
        {
            getLog().info( "Nothing to link" );
            return false;
        }
        boolean madeReplacement = false;
        for ( int i = 0; i < linkItems.length; i++ )
        {
            LinkItem item = linkItems[i];

            if ( includeProperties != null )
            {
                if ( includeProperties.indexOf( item.getProperty() ) < 0 )
                {
                    getLog().debug( "Skipping update of property \"" + item.getProperty() + "\"" );
                    continue;
                }
            }

            if ( excludeProperties != null )
            {
                if ( excludeProperties.indexOf( item.getProperty() ) >= 0 )
                {
                    getLog().debug( "Ignoring update of property \"" + item.getProperty() + "\"" );
                    continue;
                }
            }

            String itemCoordinates = item.getGroupId() + ":" + item.getArtifactId();
            String currentVersion = getPropertyValue( pom, item.getProperty() );
            String version = currentVersion;

            if ( version == null )
            {
                getLog().warn( "This project does not define the property \"" + item.getProperty() + "\"" );
                continue;
            }

            if ( item.getVersion() != null )
            {
                version = item.getVersion();
            }

            VersionRange versionRange = null;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( version );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( "Invalid version range specification for " + item.toString(), e );
            }

            Artifact artifact = artifactFactory.createDependencyArtifact( item.getGroupId(), item.getArtifactId(),
                                                                          versionRange, "pom", null, null );

            ArtifactVersion updateVersion = findLatestVersion( artifact, versionRange );

            if ( !shouldApplyUpdate( artifact, currentVersion, updateVersion ) )
            {
                return false;
            }

            String searchStr = new StringBuffer()
                .append( "<" )
                .append( item.getProperty() )
                .append( ">" )
                .append( currentVersion )
                .append( "</" )
                .append( item.getProperty() )
                .append( ">" )
                .toString();
            String replaceStr = new StringBuffer()
                .append( "<" )
                .append( item.getProperty() )
                .append( ">" )
                .append( updateVersion.toString() )
                .append( "</" )
                .append( item.getProperty() )
                .append( ">" )
                .toString();

            if ( searchStr.equals( replaceStr ) )
            {
                getLog().info( itemCoordinates + " is already up to date." );
                continue;
            }

            getLog().info( "Updating " + itemCoordinates + " from version " + currentVersion + " to " + updateVersion );

            int index = pom.indexOf( searchStr );
            while ( index < pom.length() && ( index > 0 ) )
            {
                pom.replace( index, index + searchStr.length(), replaceStr );
                index = pom.indexOf( searchStr, index + replaceStr.length() );
                madeReplacement = true;
            }
        }
        return madeReplacement;
    }

}