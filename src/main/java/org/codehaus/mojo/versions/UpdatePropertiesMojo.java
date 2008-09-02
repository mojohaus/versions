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
import org.apache.maven.artifact.ArtifactUtils;
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
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-properties
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0
 */
public class UpdatePropertiesMojo
    extends AbstractVersionsUpdaterMojo
{

// ------------------------------ FIELDS ------------------------------

    /**
     * The end of a regex literal sequence.
     *
     * @since 1.0
     */
    private static final String REGEX_QUOTE_END = "\\E";

    /**
     * The start of a regex literal sequence.
     *
     * @since 1.0
     */
    private static final String REGEX_QUOTE_START = "\\Q";

    /**
     * Escape the escapes.
     *
     * @since 1.0
     */
    private static final String REGEX_QUOTE_END_ESCAPED = REGEX_QUOTE_END + '\\' + REGEX_QUOTE_END + REGEX_QUOTE_START;

    /**
     * The properties to update and the artifact coordinates that they are to be updated from.
     *
     * @parameter
     * @since 1.0
     */
    protected LinkItem[] linkItems;

    /**
     * A comma separated list of properties to update.
     *
     * @parameter expression="${includeProperties}"
     * @since 1.0
     */
    protected String includeProperties = null;

    /**
     * A comma separated list of properties to not update.
     *
     * @parameter expression="${excludeProperties}"
     * @since 1.0
     */
    protected String excludeProperties = null;

// -------------------------- STATIC METHODS --------------------------

    /**
     * Takes a string and returns the regex that will match that string exactly.
     *
     * @param s The string to match.
     * @return The regex that will match the string exactly.
     * @since 1.0
     */
    private static String quote( String s )
    {
        int i = s.indexOf( REGEX_QUOTE_END );
        if ( i == -1 )
        {
            // we're safe as nobody has a crazy \E in the string
            return REGEX_QUOTE_START + s + REGEX_QUOTE_END;
        }

        // damn there's at least one \E in the string
        StringBuffer sb = new StringBuffer( s.length() + 32 );
        // each escape-escape takes 10 chars...
        // hope there's less than 4 of them

        sb.append( REGEX_QUOTE_START );
        int pos = 0;
        do
        {
            // we are safe from pos to i
            sb.append( s.substring( pos, i ) );
            // now escape-escape
            sb.append( REGEX_QUOTE_END_ESCAPED );
            // move the working start
            pos = i + REGEX_QUOTE_END.length();
            i = s.indexOf( REGEX_QUOTE_END, pos );
        }
        while ( i != -1 );

        sb.append( s.substring( pos, s.length() ) );
        sb.append( REGEX_QUOTE_END );

        return sb.toString();
    }

// -------------------------- OTHER METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        if ( linkItems == null || linkItems.length == 0 )
        {
            getLog().info( "Nothing to link" );
            return;
        }
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

            String itemCoords = ArtifactUtils.versionlessKey( item.getGroupId(), item.getArtifactId() );
            String curVer = getPropertyValue( pom.asStringBuffer(), item.getProperty() );
            String version = curVer;

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

            ArtifactVersion newVer = findLatestVersion( artifact, versionRange, item.getAllowSnapshots() );

            if ( !shouldApplyUpdate( artifact, curVer, newVer ) )
            {
                continue;
            }

            Stack stack = new Stack();
            String path = "";

            Pattern pathRegex = Pattern.compile( new StringBuffer()
                .append( "/project/properties(?:/profiles/profile)?/" )
                .append( quote( item.getProperty() ) )
                .toString() );

            while ( pom.hasNext() )
            {
                XMLEvent event = pom.nextEvent();
                if ( event.isStartElement() )
                {
                    stack.push( path );
                    path = new StringBuffer()
                        .append( path )
                        .append( "/" )
                        .append( event.asStartElement().getName().getLocalPart() )
                        .toString();

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
                            pom.replaceBetween( 0, 1, newVer.toString() );
                            getLog().info( new StringBuffer()
                                .append( "Updating " )
                                .append( itemCoords )
                                .append( " from version " )
                                .append( curVer )
                                .append( " to " )
                                .append( newVer )
                                .toString() );
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