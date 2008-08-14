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
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;
import java.util.Stack;
import java.util.regex.Pattern;

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
     *
     * @param pom
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( linkItems == null || linkItems.length == 0 )
        {
            getLog().info( "Nothing to link" );
            return;
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
            String currentVersion = getPropertyValue( pom.asStringBuffer(), item.getProperty() );
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

            VersionRange versionRange;
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
                return;
            }

            Stack stack = new Stack();
            String path = "";

            Pattern pathRegex =
                Pattern.compile( "/project/properties(?:/profiles/profile)?/" + Pattern.quote( item.getProperty() ) );

            while ( pom.hasNext() )
            {
                XMLEvent event = pom.nextEvent();
                if ( event.isStartElement() )
                {
                    stack.push( path );
                    path += "/" + event.asStartElement().getName().getLocalPart();

                    if ( pathRegex.matcher( path ).matches() )
                    {
                        pom.mark( 0 );
                    }
                }
                if ( event.isEndElement() )
                {
                    if ( pathRegex.matcher( path ).matches() )
                    {
                        pom.mark( 1 );
                        if ( pom.hasMark( 0 ) )
                        {
                            pom.replaceBetween( 0, 1, updateVersion.toString() );
                            getLog().info( "Updating " + itemCoordinates + " from version " + currentVersion + " to " +
                                updateVersion );
                            pom.clearMark( 0 );
                            pom.clearMark( 1 );
                        }
                    }
                    path = (String) stack.pop();
                }
            }
        }
    }

}