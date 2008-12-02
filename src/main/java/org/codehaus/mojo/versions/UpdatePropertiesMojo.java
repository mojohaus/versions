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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.regex.Pattern;

/**
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-properties
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-1
 */
public class UpdatePropertiesMojo
    extends AbstractVersionsUpdaterMojo
{

// ------------------------------ FIELDS ------------------------------

    /**
     * The end of a regex literal sequence.
     *
     * @since 1.0-alpha-1
     */
    private static final String REGEX_QUOTE_END = "\\E";

    /**
     * The start of a regex literal sequence.
     *
     * @since 1.0-alpha-1
     */
    private static final String REGEX_QUOTE_START = "\\Q";

    /**
     * Escape the escapes.
     *
     * @since 1.0-alpha-1
     */
    private static final String REGEX_QUOTE_END_ESCAPED = REGEX_QUOTE_END + '\\' + REGEX_QUOTE_END + REGEX_QUOTE_START;

    /**
     * The properties to update and the artifact coordinates that they are to be updated from.
     *
     * @parameter
     * @since 1.0-alpha-1
     */
    private LinkItem[] linkItems;

    /**
     * A comma separated list of properties to update.
     *
     * @parameter expression="${includeProperties}"
     * @since 1.0-alpha-1
     */
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not update.
     *
     * @parameter expression="${excludeProperties}"
     * @since 1.0-alpha-1
     */
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @parameter expression="${autoLinkItems}" defaultValue="true"
     * @since 1.0-alpha-2
     */
    private Boolean autoLinkItems;

// -------------------------- STATIC METHODS --------------------------

    /**
     * Takes a string and returns the regex that will match that string exactly.
     *
     * @param s The string to match.
     * @return The regex that will match the string exactly.
     * @since 1.0-alpha-1
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
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        LinkItem[] linkItems;
        if ( autoLinkItems == null || Boolean.TRUE.equals(autoLinkItems) )
        {
            Map properties = new HashMap();
            if ( this.linkItems != null )
            {
                for ( int i = 0; i < this.linkItems.length; i++ )
                {
                    properties.put( this.linkItems[i].getProperty(), this.linkItems[i] );
                }
            }
            getLog().info( "Searching for properties to update" );
            Map localDependencies = new HashMap();
            Set localProperties = new HashSet();
            Stack stack = new Stack();
            String path = "";
            String groupId = null;
            String artifactId = null;
            String property = null;
            String versions = null;
            Pattern dependencyPathMatcher = Pattern.compile(
                "/project(/profiles/profile)?((/dependencyManagement)?|(/build(/pluginManagement)?/plugins/plugin))/dependencies/dependency/(groupId|artifactId|version)" );
            Pattern propertyPathMatcher = Pattern.compile( "/project(/profiles/profile)?/properties/[^/]*" );
            while ( pom.hasNext() )
            {
                XMLEvent event = pom.nextEvent();
                if ( event.isStartDocument() )
                {
                    path = "";
                    stack.clear();
                }
                else if ( event.isStartElement() )
                {
                    stack.push( path );

                    path = path + "/" + event.asStartElement().getName().getLocalPart();

                    if ( dependencyPathMatcher.matcher( path ).matches() )
                    {
                        String text = pom.getElementText().trim();
                        if ( path.endsWith( "groupId" ) )
                        {
                            groupId = text;
                        }
                        else if ( path.endsWith( "artifactId" ) )
                        {
                            artifactId = text;
                        }
                        else if ( path.endsWith( "version" ) )
                        {
                            int start = text.indexOf( "${" );
                            int middle = text.indexOf( "${", start + 1 );
                            int end = text.lastIndexOf( "}" );
                            if ( start != -1 && end != -1 && end > start && middle == -1 )
                            {
                                property = text.substring( start + 2, end ).trim();
                                if ( start > 0 && end < text.length() )
                                {
                                    String prefix = text.substring( 0, start ).trim();
                                    String postfix = text.substring( end + 1, text.length() ).trim();
                                    if ( ( prefix.startsWith( "[" ) || prefix.startsWith( "(" ) ) && (
                                        postfix.endsWith( "]" ) || postfix.endsWith( ")" ) )
                                        && ( prefix.length() > 1 || postfix.length() > 1) )
                                    {
                                        versions = prefix + postfix;
                                    }

                                }
                            }
                        }
                        path = (String) stack.pop();
                    }
                    else if ( path.endsWith( "dependency" ) )
                    {
                        groupId = null;
                        artifactId = null;
                        property = null;
                        versions = null;
                    }
                    else if ( propertyPathMatcher.matcher( path ).matches() )
                    {
                        property = event.asStartElement().getName().getLocalPart();
                        if ( !properties.containsKey( property ) && !localProperties.contains( property ) )
                        {
                            LinkItem candidate = (LinkItem) localDependencies.remove( property );
                            if ( candidate == null )
                            {
                                localProperties.add( property );
                            }
                            else
                            {
                                properties.put( property, candidate );
                                getLog().info( "Associating property ${" + property + "} with "
                                    + ArtifactUtils.versionlessKey( candidate.getGroupId(), candidate.getArtifactId() )
                                    + ( candidate.getVersion() != null ? " within the version range "
                                    + candidate.getVersion() : "" ) );
                            }
                        }
                    }
                }
                else if ( event.isEndElement() )
                {
                    if ( path.endsWith( "dependency" ) )
                    {
                        if ( groupId != null && artifactId != null && property != null && !properties.containsKey(
                            property ) )
                        {
                            LinkItem candidate = new LinkItem();
                            candidate.setGroupId( groupId );
                            candidate.setArtifactId( artifactId );
                            candidate.setProperty( property );
                            if ( versions != null )
                            {
                                candidate.setVersion( versions );
                            }
                            if ( localProperties.contains( property ) )
                            {
                                properties.put( property, candidate );
                                localProperties.remove( property );
                                getLog().info( "Associating property ${" + property + "} with "
                                    + ArtifactUtils.versionlessKey( candidate.getGroupId(), candidate.getArtifactId() )
                                    + ( candidate.getVersion() != null ? " within the version range "
                                    + candidate.getVersion() : "" ) );
                            }
                            else
                            {
                                localDependencies.put( property, candidate );
                            }
                        }
                    }
                    path = (String) stack.pop();
                }
            }
            dependencyPathMatcher = Pattern.compile(
                "/project(/profiles/profile)?((/reporting/plugins/plugin)?|(/build(/pluginManagement)?/plugins/plugin))/(groupId|artifactId|version)" );
            pom.rewind();
            while ( pom.hasNext() )
            {
                XMLEvent event = pom.nextEvent();
                if ( event.isStartDocument() )
                {
                    path = "";
                    stack.clear();
                }
                else if ( event.isStartElement() )
                {
                    stack.push( path );

                    path = path + "/" + event.asStartElement().getName().getLocalPart();

                    if ( dependencyPathMatcher.matcher( path ).matches() )
                    {
                        String text = pom.getElementText().trim();
                        if ( path.endsWith( "groupId" ) )
                        {
                            groupId = text;
                        }
                        else if ( path.endsWith( "artifactId" ) )
                        {
                            artifactId = text;
                        }
                        else if ( path.endsWith( "version" ) )
                        {
                            int start = text.indexOf( "${" );
                            int middle = text.indexOf( "${", start + 1 );
                            int end = text.lastIndexOf( "}" );
                            if ( start != -1 && end != -1 && end > start && middle == -1 )
                            {
                                property = text.substring( start + 2, end ).trim();
                            }
                        }
                        path = (String) stack.pop();
                    }
                    else if ( path.endsWith( "plugin" ) )
                    {
                        groupId = null;
                        artifactId = null;
                        property = null;
                        versions = null;
                    }
                    else if ( propertyPathMatcher.matcher( path ).matches() )
                    {
                        property = event.asStartElement().getName().getLocalPart();
                        if ( !properties.containsKey( property ) && !localProperties.contains( property ) )
                        {
                            LinkItem candidate = (LinkItem) localDependencies.remove( property );
                            if ( candidate == null )
                            {
                                localProperties.add( property );
                            }
                            else
                            {
                                properties.put( property, candidate );
                                getLog().info( "Associating property ${" + property + "} with "
                                    + ArtifactUtils.versionlessKey( candidate.getGroupId(), candidate.getArtifactId() )
                                    + ( candidate.getVersion() != null ? " within the version range "
                                    + candidate.getVersion() : "" ) );
                            }
                        }
                    }
                }
                else if ( event.isEndElement() )
                {
                    if ( path.endsWith( "dependency" ) )
                    {
                        if ( groupId != null && artifactId != null && property != null && !properties.containsKey(
                            property ) )
                        {
                            LinkItem candidate = new LinkItem();
                            candidate.setGroupId( groupId );
                            candidate.setArtifactId( artifactId );
                            candidate.setProperty( property );
                            if ( versions != null )
                            {
                                candidate.setVersion( versions );
                            }
                            if ( localProperties.contains( property ) )
                            {
                                properties.put( property, candidate );
                                localProperties.remove( property );
                                getLog().info( "Associating property ${" + property + "} with "
                                    + ArtifactUtils.versionlessKey( candidate.getGroupId(), candidate.getArtifactId() )
                                    + ( candidate.getVersion() != null ? " within the version range "
                                    + candidate.getVersion() : "" ) );
                            }
                            else
                            {
                                localDependencies.put( property, candidate );
                            }
                        }
                    }
                    path = (String) stack.pop();
                }
            }
            linkItems = (LinkItem[]) properties.values().toArray( new LinkItem[properties.size()] );
        }
        else
        {
            linkItems = this.linkItems;
        }
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

            pom.rewind(); // fixes MVERSIONS-8

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