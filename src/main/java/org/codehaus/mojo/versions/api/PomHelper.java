package org.codehaus.mojo.versions.api;

/*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.*;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.profiles.ProfileManager;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;
import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helper class for modifying pom files.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class PomHelper
{
    /**
     * The encoding used for the pom file.
     *
     * @since 1.0-alpha-3
     */
    public static final String POM_ENCODING = "UTF-8";

    public static final String APACHE_MAVEN_PLUGINS_GROUPID = "org.apache.maven.plugins";

    /**
     * Gets the raw model before any interpolation what-so-ever.
     *
     * @param project The project to get the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel( MavenProject project )
        throws IOException
    {
        return getRawModel( project.getFile() );
    }

    /**
     * Gets the raw model before any interpolation what-so-ever.
     *
     * @param moduleProjectFile The project file to get the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel( File moduleProjectFile )
        throws IOException
    {
        FileReader fileReader = null;
        BufferedReader bufferedReader = null;
        try
        {
            fileReader = new FileReader( moduleProjectFile );
            bufferedReader = new BufferedReader( fileReader );
            MavenXpp3Reader reader = new MavenXpp3Reader();
            return reader.read( bufferedReader );
        }
        catch ( XmlPullParserException e )
        {
            IOException ioe = new IOException( e.getMessage() );
            ioe.initCause( e );
            throw ioe;
        }
        finally
        {
            if ( bufferedReader != null )
            {
                bufferedReader.close();
            }
            if ( fileReader != null )
            {
                fileReader.close();
            }
        }
    }

    /**
     * Gets the current raw model before any interpolation what-so-ever.
     *
     * @param modifiedPomXMLEventReader The {@link ModifiedPomXMLEventReader} to get the raw model for.
     * @return The raw model.
     * @throws IOException if the file is not found or if the file does not parse.
     */
    public static Model getRawModel( ModifiedPomXMLEventReader modifiedPomXMLEventReader )
        throws IOException
    {
        StringReader stringReader = null;
        try
        {
            stringReader = new StringReader( modifiedPomXMLEventReader.asStringBuffer().toString() );
            MavenXpp3Reader reader = new MavenXpp3Reader();
            return reader.read( stringReader );
        }
        catch ( XmlPullParserException e )
        {
            IOException ioe = new IOException( e.getMessage() );
            ioe.initCause( e );
            throw ioe;
        }
        finally
        {
            if ( stringReader != null )
            {
                stringReader.close();
            }
        }
    }

    /**
     * Searches the pom re-defining the specified property to the specified version.
     *
     * @param pom       The pom to modify.
     * @param profileId The profile in which to modify the property.
     * @param property  The property to modify.
     * @param value     The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setPropertyVersion( final ModifiedPomXMLEventReader pom, final String profileId,
                                              final String property, final String value )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern propertyRegex;
        final Pattern matchScopeRegex;
        final Pattern projectProfileId;
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        if ( profileId == null )
        {
            propertyRegex = Pattern.compile( "/project/properties/" + RegexUtils.quote( property ) );
            matchScopeRegex = Pattern.compile( "/project/properties" );
            projectProfileId = null;
        }
        else
        {
            propertyRegex = Pattern.compile( "/project/profiles/profile/properties/" + RegexUtils.quote( property ) );
            matchScopeRegex = Pattern.compile( "/project/profiles/profile" );
            projectProfileId = Pattern.compile( "/project/profiles/profile/id" );
        }

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer().append( path ).append( "/" ).append(
                    event.asStartElement().getName().getLocalPart() ).toString();

                if ( propertyRegex.matcher( path ).matches() )
                {
                    pom.mark( 0 );
                }
                else if ( matchScopeRegex.matcher( path ).matches() )
                {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = profileId == null;
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                }
                else if ( profileId != null && projectProfileId.matcher( path ).matches() )
                {
                    String candidateId = pom.getElementText();
                    path = (String) stack.pop(); // since getElementText will be after the end element

                    inMatchScope = profileId.trim().equals( candidateId.trim() );
                }
            }
            if ( event.isEndElement() )
            {
                if ( propertyRegex.matcher( path ).matches() )
                {
                    pom.mark( 1 );
                }
                else if ( matchScopeRegex.matcher( path ).matches() )
                {
                    if ( inMatchScope && pom.hasMark( 0 ) && pom.hasMark( 1 ) )
                    {
                        pom.replaceBetween( 0, 1, value );
                        madeReplacement = true;
                    }
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                    inMatchScope = false;
                }
                path = (String) stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Searches the pom re-defining the project version to the specified version.
     *
     * @param pom   The pom to modify.
     * @param value The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setProjectVersion( final ModifiedPomXMLEventReader pom, final String value )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex;
        boolean madeReplacement = false;
        matchScopeRegex = Pattern.compile( "/project/version" );

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer().append( path ).append( "/" ).append(
                    event.asStartElement().getName().getLocalPart() ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 1 );
                    if ( pom.hasMark( 0 ) && pom.hasMark( 1 ) )
                    {
                        pom.replaceBetween( 0, 1, value );
                        madeReplacement = true;
                    }
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                }
                path = (String) stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Retrieves the project version from the pom.
     *
     * @param pom The pom.
     * @return the project version or <code>null</code> if the project version is not defined (i.e. inherited from parent version).
     * @throws XMLStreamException if something went wrong.
     */
    public static String getProjectVersion( final ModifiedPomXMLEventReader pom )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex = Pattern.compile( "/project/version" );

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer().append( path ).append( "/" ).append(
                    event.asStartElement().getName().getLocalPart() ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 1 );
                    if ( pom.hasMark( 0 ) && pom.hasMark( 1 ) )
                    {
                        return pom.getBetween( 0, 1 ).trim();
                    }
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                }
                path = (String) stack.pop();
            }
        }
        return null;
    }

    /**
     * Searches the pom re-defining the project version to the specified version.
     *
     * @param pom   The pom to modify.
     * @param value The new value of the property.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setProjectParentVersion( final ModifiedPomXMLEventReader pom, final String value )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex;
        boolean madeReplacement = false;
        matchScopeRegex = Pattern.compile( "/project/parent/version" );

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path = new StringBuffer().append( path ).append( "/" ).append(
                    event.asStartElement().getName().getLocalPart() ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    pom.mark( 1 );
                    if ( pom.hasMark( 0 ) && pom.hasMark( 1 ) )
                    {
                        pom.replaceBetween( 0, 1, value );
                        madeReplacement = true;
                    }
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                }
                path = (String) stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Gets the parent artifact from the pom.
     *
     * @param pom    The pom.
     * @param helper The helper (used to create the artifact).
     * @return The parent artifact or <code>null</code> if no parent is specified.
     * @throws XMLStreamException if something went wrong.
     */
    public static Artifact getProjectParent( final ModifiedPomXMLEventReader pom, VersionsHelper helper )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex = Pattern.compile( "/project/parent((/groupId)|(/artifactId)|(/version))" );
        String groupId = null;
        String artifactId = null;
        String version = null;

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                final String elementName = event.asStartElement().getName().getLocalPart();
                path = new StringBuffer().append( path ).append( "/" ).append( elementName ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    if ( "groupId".equals( elementName ) )
                    {
                        groupId = pom.getElementText().trim();
                        path = (String) stack.pop();
                    }
                    else if ( "artifactId".equals( elementName ) )
                    {
                        artifactId = pom.getElementText().trim();
                        path = (String) stack.pop();
                    }
                    else if ( "version".equals( elementName ) )
                    {
                        version = pom.getElementText().trim();
                        path = (String) stack.pop();
                    }
                }
            }
            if ( event.isEndElement() )
            {
                path = (String) stack.pop();
            }
        }
        if ( groupId == null || artifactId == null || version == null )
        {
            return null;
        }
        return helper.createDependencyArtifact( groupId, artifactId, VersionRange.createFromVersion( version ), "pom",
                                                null, null, false );
    }

    /**
     * Searches the pom re-defining the specified dependency to the specified version.
     *
     * @param pom        The pom to modify.
     * @param groupId    The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @param oldVersion The old version of the dependency.
     * @param newVersion The new version of the dependency.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setDependencyVersion( final ModifiedPomXMLEventReader pom, final String groupId,
                                                final String artifactId, final String oldVersion,
                                                final String newVersion )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex;
        final Pattern matchTargetRegex;
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        boolean haveGroupId = false;
        boolean haveArtifactId = false;
        boolean haveOldVersion = false;

        matchScopeRegex = Pattern.compile( "/project" + "(/profiles/profile)?"
            + "((/dependencyManagement)|(/build(/pluginManagement)?/plugins/plugin))?" + "/dependencies/dependency" );

        matchTargetRegex = Pattern.compile( "/project" + "(/profiles/profile)?"
            + "((/dependencyManagement)|(/build(/pluginManagement)?/plugins/plugin))?" + "/dependencies/dependency"
            + "((/groupId)|(/artifactId)|(/version))" );

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                final String elementName = event.asStartElement().getName().getLocalPart();
                path = new StringBuffer().append( path ).append( "/" ).append( elementName ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = true;
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );

                    haveGroupId = false;
                    haveArtifactId = false;
                    haveOldVersion = false;
                }
                else if ( inMatchScope && matchTargetRegex.matcher( path ).matches() )
                {
                    if ( "groupId".equals( elementName ) )
                    {
                        haveGroupId = groupId.equals( pom.getElementText().trim() );
                        path = (String) stack.pop();
                    }
                    else if ( "artifactId".equals( elementName ) )
                    {
                        haveArtifactId = artifactId.equals( pom.getElementText().trim() );
                        path = (String) stack.pop();
                    }
                    else if ( "version".equals( elementName ) )
                    {
                        pom.mark( 0 );
                    }
                }
            }
            if ( event.isEndElement() )
            {
                if ( matchTargetRegex.matcher( path ).matches() && "version".equals(
                    event.asEndElement().getName().getLocalPart() ) )
                {
                    pom.mark( 1 );
                    String compressedPomVersion = StringUtils.deleteWhitespace( pom.getBetween( 0, 1 ).trim() );
                    String compressedOldVersion = StringUtils.deleteWhitespace( oldVersion );
                    haveOldVersion = compressedOldVersion.equals( compressedPomVersion );
                }
                else if ( matchScopeRegex.matcher( path ).matches() )
                {
                    if ( inMatchScope && pom.hasMark( 0 ) && pom.hasMark( 1 ) && haveGroupId && haveArtifactId
                        && haveOldVersion )
                    {
                        pom.replaceBetween( 0, 1, newVersion );
                        madeReplacement = true;
                    }
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );
                    haveArtifactId = false;
                    haveGroupId = false;
                    haveOldVersion = false;
                    inMatchScope = false;
                }
                path = (String) stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Searches the pom re-defining the specified plugin to the specified version.
     *
     * @param pom        The pom to modify.
     * @param groupId    The groupId of the dependency.
     * @param artifactId The artifactId of the dependency.
     * @param oldVersion The old version of the dependency.
     * @param newVersion The new version of the dependency.
     * @return <code>true</code> if a replacement was made.
     * @throws XMLStreamException if somethinh went wrong.
     */
    public static boolean setPluginVersion( final ModifiedPomXMLEventReader pom, final String groupId,
                                            final String artifactId, final String oldVersion, final String newVersion )
        throws XMLStreamException
    {
        Stack stack = new Stack();
        String path = "";
        final Pattern matchScopeRegex;
        final Pattern matchTargetRegex;
        boolean inMatchScope = false;
        boolean madeReplacement = false;
        boolean haveGroupId = false;
        boolean needGroupId = groupId != null && !APACHE_MAVEN_PLUGINS_GROUPID.equals( groupId );
        boolean haveArtifactId = false;
        boolean haveOldVersion = false;

        matchScopeRegex = Pattern.compile(
            "/project" + "(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))/plugins/plugin" );

        matchTargetRegex = Pattern.compile(
            "/project" + "(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))/plugins/plugin"
                + "((/groupId)|(/artifactId)|(/version))" );

        pom.rewind();

        while ( pom.hasNext() )
        {
            XMLEvent event = pom.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                final String elementName = event.asStartElement().getName().getLocalPart();
                path = new StringBuffer().append( path ).append( "/" ).append( elementName ).toString();

                if ( matchScopeRegex.matcher( path ).matches() )
                {
                    // we're in a new match scope
                    // reset any previous partial matches
                    inMatchScope = true;
                    pom.clearMark( 0 );
                    pom.clearMark( 1 );

                    haveGroupId = false;
                    haveArtifactId = false;
                    haveOldVersion = false;
                }
                else if ( inMatchScope && matchTargetRegex.matcher( path ).matches() )
                {
                    if ( "groupId".equals( elementName ) )
                    {
                        haveGroupId = groupId.equals( pom.getElementText().trim() );
                        path = (String) stack.pop();
                    }
                    else if ( "artifactId".equals( elementName ) )
                    {
                        haveArtifactId = artifactId.equals( pom.getElementText().trim() );
                        path = (String) stack.pop();
                    }
                    else if ( "version".equals( elementName ) )
                    {
                        pom.mark( 0 );
                    }
                }
            }
            if ( event.isEndElement() )
            {
                if ( matchTargetRegex.matcher( path ).matches() && "version".equals(
                    event.asEndElement().getName().getLocalPart() ) )
                {
                    pom.mark( 1 );
                    haveOldVersion = oldVersion.equals( pom.getBetween( 0, 1 ).trim() );
                }
                else if ( matchScopeRegex.matcher( path ).matches() )
                {
                    if ( inMatchScope && pom.hasMark( 0 ) && pom.hasMark( 1 ) && ( haveGroupId || !needGroupId )
                        && haveArtifactId && haveOldVersion )
                    {
                        pom.replaceBetween( 0, 1, newVersion );
                        madeReplacement = true;
                        pom.clearMark( 0 );
                        pom.clearMark( 1 );
                        haveArtifactId = false;
                        haveGroupId = false;
                        haveOldVersion = false;
                    }
                    inMatchScope = false;
                }
                path = (String) stack.pop();
            }
        }
        return madeReplacement;
    }

    /**
     * Examines the project to find any properties which are associated with versions of artifacts in the project.
     *
     * @param helper  Our versions helper.
     * @param project The project to examine.
     * @return An array of properties that are associated within the project.
     * @throws ExpressionEvaluationException if an expression cannot be evaluated.
     * @throws IOException                   if the project's pom file cannot be parsed.
     * @since 1.0-alpha-3
     */
    public static PropertyVersionsBuilder[] getPropertyVersionsBuilders( VersionsHelper helper, MavenProject project )
        throws ExpressionEvaluationException, IOException
    {
        ExpressionEvaluator expressionEvaluator = helper.getExpressionEvaluator( project );
        Model model = getRawModel( project );
        Map/*<String,PropertyVersionsBuilder>*/ result = new TreeMap();

        Set/*<String*/ activeProfiles = new TreeSet();
        for ( Iterator i = project.getActiveProfiles().iterator(); i.hasNext(); )
        {
            Profile profile = (Profile) i.next();
            activeProfiles.add( profile.getId() );
        }

        // add any properties from profiles first (as they override properties from the project
        for ( Iterator i = model.getProfiles().iterator(); i.hasNext(); )
        {
            Profile profile = (Profile) i.next();
            if ( !activeProfiles.contains( profile.getId() ) )
            {
                continue;
            }
            addProperties( helper, result, profile.getId(), profile.getProperties() );
            if ( profile.getDependencyManagement() != null )
            {
                addDependencyAssocations( helper, expressionEvaluator, result,
                                          profile.getDependencyManagement().getDependencies(), false );
            }
            addDependencyAssocations( helper, expressionEvaluator, result, profile.getDependencies(), false );
            if ( profile.getBuild() != null )
            {
                if ( profile.getBuild().getPluginManagement() != null )
                {
                    addPluginAssociations( helper, expressionEvaluator, result,
                                           profile.getBuild().getPluginManagement().getPlugins() );
                }
                addPluginAssociations( helper, expressionEvaluator, result, profile.getBuild().getPlugins() );
            }
            if ( profile.getReporting() != null )
            {
                addReportPluginAssociations( helper, expressionEvaluator, result, profile.getReporting().getPlugins() );
            }
        }

        // second, we add all the properties in the pom
        addProperties( helper, result, null, model.getProperties() );
        if ( model.getDependencyManagement() != null )
        {
            addDependencyAssocations( helper, expressionEvaluator, result,
                                      model.getDependencyManagement().getDependencies(), false );
        }
        addDependencyAssocations( helper, expressionEvaluator, result, model.getDependencies(), false );
        if ( model.getBuild() != null )
        {
            if ( model.getBuild().getPluginManagement() != null )
            {
                addPluginAssociations( helper, expressionEvaluator, result,
                                       model.getBuild().getPluginManagement().getPlugins() );
            }
            addPluginAssociations( helper, expressionEvaluator, result, model.getBuild().getPlugins() );
        }
        if ( model.getReporting() != null )
        {
            addReportPluginAssociations( helper, expressionEvaluator, result, model.getReporting().getPlugins() );
        }

        // finally, remove any properties without associations
        purgeProperties( result );

        return (PropertyVersionsBuilder[]) result.values().toArray(
            new PropertyVersionsBuilder[result.values().size()] );
    }

    /**
     * Takes a list of {@link org.apache.maven.model.Plugin} instances and adds associations to properties used to
     * define versions of the plugin artifact or any of the plugin dependencies specified in the pom.
     *
     * @param helper              Our helper.
     * @param expressionEvaluator Our expression evaluator.
     * @param result              The map of {@link org.codehaus.mojo.versions.api.PropertyVersionsBuilder} keyed by property name.
     * @param plugins             The list of {@link org.apache.maven.model.Plugin}.
     * @throws ExpressionEvaluationException if an expression cannot be evaluated.
     */
    private static void addPluginAssociations( VersionsHelper helper, ExpressionEvaluator expressionEvaluator,
                                               Map result, List plugins )
        throws ExpressionEvaluationException
    {
        if ( plugins == null )
        {
            return;
        }
        for ( Iterator i = plugins.iterator(); i.hasNext(); )
        {
            Plugin plugin = (Plugin) i.next();
            String version = plugin.getVersion();
            if ( version != null && version.indexOf( "${" ) != -1 && version.indexOf( '}' ) != -1 )
            {
                version = StringUtils.deleteWhitespace( version );
                for ( Iterator j = result.values().iterator(); j.hasNext(); )
                {
                    // any of these could be defined by a property
                    PropertyVersionsBuilder property = (PropertyVersionsBuilder) j.next();
                    final String propertyRef = "${" + property.getName() + "}";
                    if ( version.indexOf( propertyRef ) != -1 )
                    {
                        String groupId = plugin.getGroupId();
                        if ( groupId == null || groupId.trim().length() == 0 )
                        {
                            // group Id has a special default
                            groupId = APACHE_MAVEN_PLUGINS_GROUPID;
                        }
                        else
                        {
                            groupId = (String) expressionEvaluator.evaluate( groupId );
                        }
                        String artifactId = plugin.getArtifactId();
                        if ( artifactId == null || artifactId.trim().length() == 0 )
                        {
                            // malformed pom
                            continue;
                        }
                        else
                        {
                            artifactId = (String) expressionEvaluator.evaluate( artifactId );
                        }
                        // might as well capture the current value
                        VersionRange versionRange = VersionRange.createFromVersion(
                            (String) expressionEvaluator.evaluate( plugin.getVersion() ) );
                        property.addAssociation( helper.createPluginArtifact( groupId, artifactId, versionRange ),
                                                 true );
                        if ( !propertyRef.equals( version ) )
                        {
                            addBounds( property, version, propertyRef, versionRange.toString() );
                        }
                    }
                }
            }
            addDependencyAssocations( helper, expressionEvaluator, result, plugin.getDependencies(), true );
        }
    }

    private static void addReportPluginAssociations( VersionsHelper helper, ExpressionEvaluator expressionEvaluator,
                                                     Map result, List reportPlugins )
        throws ExpressionEvaluationException
    {
        if ( reportPlugins == null )
        {
            return;
        }
        for ( Iterator i = reportPlugins.iterator(); i.hasNext(); )
        {
            ReportPlugin plugin = (ReportPlugin) i.next();
            String version = plugin.getVersion();
            if ( version != null && version.indexOf( "${" ) != -1 && version.indexOf( '}' ) != -1 )
            {
                version = StringUtils.deleteWhitespace( version );
                for ( Iterator j = result.values().iterator(); j.hasNext(); )
                {
                    PropertyVersionsBuilder property = (PropertyVersionsBuilder) j.next();
                    final String propertyRef = "${" + property.getName() + "}";
                    if ( version.indexOf( propertyRef ) != -1 )
                    {
                        // any of these could be defined by a property
                        String groupId = plugin.getGroupId();
                        if ( groupId == null || groupId.trim().length() == 0 )
                        {
                            // group Id has a special default
                            groupId = APACHE_MAVEN_PLUGINS_GROUPID;
                        }
                        else
                        {
                            groupId = (String) expressionEvaluator.evaluate( groupId );
                        }
                        String artifactId = plugin.getArtifactId();
                        if ( artifactId == null || artifactId.trim().length() == 0 )
                        {
                            // malformed pom
                            continue;
                        }
                        else
                        {
                            artifactId = (String) expressionEvaluator.evaluate( artifactId );
                        }
                        // might as well capture the current value
                        VersionRange versionRange = VersionRange.createFromVersion(
                            (String) expressionEvaluator.evaluate( plugin.getVersion() ) );
                        property.addAssociation( helper.createPluginArtifact( groupId, artifactId, versionRange ),
                                                 true );
                        if ( !propertyRef.equals( version ) )
                        {
                            addBounds( property, version, propertyRef, versionRange.toString() );
                        }
                    }
                }
            }
        }
    }

    private static void addDependencyAssocations( VersionsHelper helper, ExpressionEvaluator expressionEvaluator,
                                                  Map result, List dependencies, boolean usePluginRepositories )
        throws ExpressionEvaluationException
    {
        if ( dependencies == null )
        {
            return;
        }
        for ( Iterator i = dependencies.iterator(); i.hasNext(); )
        {
            Dependency dependency = (Dependency) i.next();
            String version = dependency.getVersion();
            if ( version != null && version.indexOf( "${" ) != -1 && version.indexOf( '}' ) != -1 )
            {
                version = StringUtils.deleteWhitespace( version );
                for ( Iterator j = result.values().iterator(); j.hasNext(); )
                {
                    PropertyVersionsBuilder property = (PropertyVersionsBuilder) j.next();
                    final String propertyRef = "${" + property.getName() + "}";
                    if ( version.indexOf( propertyRef ) != -1 )
                    {
                        // Any of these could be defined by a property
                        String groupId = dependency.getGroupId();
                        if ( groupId == null || groupId.trim().length() == 0 )
                        {
                            // malformed pom
                            continue;
                        }
                        else
                        {
                            groupId = (String) expressionEvaluator.evaluate( groupId );
                        }
                        String artifactId = dependency.getArtifactId();
                        if ( artifactId == null || artifactId.trim().length() == 0 )
                        {
                            // malformed pom
                            continue;
                        }
                        else
                        {
                            artifactId = (String) expressionEvaluator.evaluate( artifactId );
                        }
                        // might as well capture the current value
                        VersionRange versionRange = VersionRange.createFromVersion(
                            (String) expressionEvaluator.evaluate( dependency.getVersion() ) );
                        property.addAssociation(
                            helper.createDependencyArtifact( groupId, artifactId, versionRange, dependency.getType(),
                                                             dependency.getClassifier(), dependency.getScope(),
                                                             dependency.isOptional() ), usePluginRepositories );
                        if ( !propertyRef.equals( version ) )
                        {
                            addBounds( property, version, propertyRef, versionRange.toString() );
                        }
                    }
                }
            }
        }
    }

    private static void addBounds( PropertyVersionsBuilder builder, String rawVersionRange, String propertyRef,
                                   String evaluatedVersionRange )
    {
        Pattern lowerBound = Pattern.compile( "([(\\[])([^,]*)," + RegexUtils.quote( propertyRef ) + "([)\\]])" );
        Pattern upperBound = Pattern.compile( "([(\\[])" + RegexUtils.quote( propertyRef ) + ",([^,]*)([)\\]])" );
        Matcher m = lowerBound.matcher( rawVersionRange );
        if ( m.find() )
        {
            boolean includeLower = "[".equals( m.group( 1 ) );
            String lowerLimit = m.group( 2 );
            if ( StringUtils.isNotEmpty( lowerLimit ) )
            {
                builder.addLowerBound( lowerLimit, includeLower );
            }
        }
        m = upperBound.matcher( rawVersionRange );
        if ( m.find() )
        {
            boolean includeUpper = "[".equals( m.group( 3 ) );
            String upperLimit = m.group( 2 );
            if ( StringUtils.isNotEmpty( upperLimit ) )
            {
                builder.addUpperBound( upperLimit, includeUpper );
            }
        }
    }

    private static void addProperties( VersionsHelper helper, Map result, String profileId, Properties properties )
    {
        if ( properties == null )
        {
            return;
        }
        for ( Enumeration j = properties.propertyNames(); j.hasMoreElements(); )
        {
            String propertyName = (String) j.nextElement();
            if ( !result.containsKey( propertyName ) )
            {
                result.put( propertyName, new PropertyVersionsBuilder( profileId, propertyName, helper ) );
            }
        }
    }

    private static void purgeProperties( Map result )
    {
        for ( Iterator i = result.values().iterator(); i.hasNext(); )
        {
            PropertyVersionsBuilder versions = (PropertyVersionsBuilder) i.next();
            if ( versions.getAssociations().length == 0 )
            {
                i.remove();
            }
        }
    }

    /**
     * Returns a set of all child modules for a project, including any defined in profiles (ignoring profile
     * activation).
     *
     * @param project The project.
     * @param logger  The logger to use.
     * @return the set of all child modules of the project.
     */
    public static Set getAllChildModules( MavenProject project, Log logger )
    {
        return getAllChildModules( project.getOriginalModel(), logger );
    }

    /**
     * Returns a set of all child modules for a project, including any defined in profiles (ignoring profile
     * activation).
     *
     * @param model  The project model.
     * @param logger The logger to use.
     * @return the set of all child modules of the project.
     */
    public static Set getAllChildModules( Model model, Log logger )
    {
        logger.debug( "Finding child modules..." );
        Set childModules = new TreeSet();
        childModules.addAll( model.getModules() );
        Iterator i = model.getProfiles().iterator();
        while ( i.hasNext() )
        {
            Profile profile = (Profile) i.next();
            childModules.addAll( profile.getModules() );
        }
        debugModules( logger, "Child modules:", childModules );
        return childModules;
    }

    /**
     * Outputs a debug message with a list of modules.
     *
     * @param logger  The logger to log to.
     * @param message The message to display.
     * @param modules The modules to append to the message.
     */
    public static void debugModules( Log logger, String message, Collection modules )
    {
        Iterator i;
        if ( logger.isDebugEnabled() )
        {
            logger.debug( message );
            if ( modules.isEmpty() )
            {
                logger.debug( "None." );
            }
            else
            {
                i = modules.iterator();
                while ( i.hasNext() )
                {
                    logger.debug( "  " + i.next() );
                }
            }

        }
    }

    /**
     * Modifies the collection of child modules removing those which cannot be found relative to the parent.
     *
     * @param logger       The logger to log to.
     * @param project      the project.
     * @param childModules the child modules.
     */
    public static void removeMissingChildModules( Log logger, MavenProject project, Collection childModules )
    {
        removeMissingChildModules( logger, project.getBasedir(), childModules );
    }

    /**
     * Modifies the collection of child modules removing those which cannot be found relative to the parent.
     *
     * @param logger       The logger to log to.
     * @param basedir      the project basedir.
     * @param childModules the child modules.
     */
    public static void removeMissingChildModules( Log logger, File basedir, Collection childModules )
    {
        logger.debug( "Removing child modules which are missing..." );
        Iterator i = childModules.iterator();
        while ( i.hasNext() )
        {
            String modulePath = (String) i.next();
            File moduleFile = new File( basedir, modulePath );

            if ( moduleFile.isDirectory() && new File( moduleFile, "pom.xml" ).isFile() )
            {
                // it's a directory that exists
                continue;
            }

            if ( moduleFile.isFile() )
            {
                // it's the pom.xml file directly referenced and it exists.
                continue;
            }

            logger.debug( "Removing missing child module " + modulePath );
            i.remove();
        }
        debugModules( logger, "After removing missing", childModules );
    }

    /**
     * Extracts the version from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The version.
     */
    public static String getVersion( Model model )
    {
        String targetVersion = model.getVersion();
        if ( targetVersion == null && model.getParent() != null )
        {
            targetVersion = model.getParent().getVersion();
        }
        return targetVersion;
    }

    /**
     * Extracts the artifactId from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The artifactId.
     */
    public static String getArtifactId( Model model )
    {
        String sourceArtifactId = model.getArtifactId();
        if ( sourceArtifactId == null && model.getParent() != null )
        {
            sourceArtifactId = model.getParent().getArtifactId();
        }
        return sourceArtifactId;
    }

    /**
     * Extracts the groupId from a raw model, interpolating from the parent if necessary.
     *
     * @param model The model.
     * @return The groupId.
     */
    public static String getGroupId( Model model )
    {
        String targetGroupId = model.getGroupId();
        if ( targetGroupId == null && model.getParent() != null )
        {
            targetGroupId = model.getParent().getGroupId();
        }
        return targetGroupId;
    }

    /**
     * Finds the local root of the specified project.
     *
     * @param project              The project to find the local root for.
     * @param localRepository      the local repo.
     * @param globalProfileManager the global profile manager.
     * @param logger
     * @return The local root (note this may be the project passed as an argument).
     */
    public static MavenProject getLocalRoot( MavenProjectBuilder builder, MavenProject project,
                                             ArtifactRepository localRepository, ProfileManager globalProfileManager,
                                             Log logger )
    {
        logger.info( "Searching for local aggregator root..." );
        while ( true )
        {
            final File parentDir = project.getBasedir().getParentFile();
            if ( parentDir.isDirectory() )
            {
                logger.debug( "Checking to see if " + parentDir + " is an aggregator parent" );
                File parent = new File( parentDir, "pom.xml" );
                if ( parent.isFile() )
                {
                    try
                    {
                        final MavenProject parentProject =
                            builder.build( parent, localRepository, globalProfileManager );
                        if ( getAllChildModules( parentProject, logger ).contains( project.getBasedir().getName() ) )
                        {
                            logger.debug( parentDir + " is an aggregator parent" );
                            project = parentProject;
                            continue;
                        }
                        else
                        {
                            logger.debug( parentDir + " is not an aggregator parent" );
                        }
                    }
                    catch ( ProjectBuildingException e )
                    {
                        logger.warn( e );
                    }
                }
            }
            logger.debug( "Local aggregation root is " + project.getBasedir() );
            return project;
        }
    }

    /**
     * Builds a map of raw models keyed by module path.
     *
     * @param project The project to build from.
     * @param logger  The logger for logging.
     * @return A map of raw models keyed by path relative to the project's basedir.
     * @throws IOException if things go wrong.
     */
    public static Map/*<String,Model>*/ getReactorModels( MavenProject project, Log logger )
        throws IOException
    {
        Map result = new LinkedHashMap();
        final Model model = getRawModel( project );
        final String path = "";
        result.put( path, model );
        result.putAll( getReactorModels( path, model, project, logger ) );
        return result;
    }

    /**
     * Builds a sub-map of raw models keyed by module path.
     *
     * @param path    The relative path to base the sub-map on.
     * @param model   The model at the relative path.
     * @param project The project to build from.
     * @param logger  The logger for logging.
     * @return A map of raw models keyed by path relative to the project's basedir.
     * @throws IOException if things go wrong.
     */
    private static Map/*<String,Model>*/ getReactorModels( String path, Model model, MavenProject project, Log logger )
        throws IOException
    {
        if ( path.length() > 0 && !path.endsWith( "/" ) )
        {
            path += '/';
        }
        Map result = new LinkedHashMap();
        Map childResults = new LinkedHashMap();

        File baseDir = path.length() > 0 ? new File( project.getBasedir(), path ) : project.getBasedir();

        Set childModules = getAllChildModules( model, logger );

        removeMissingChildModules( logger, baseDir, childModules );

        Iterator i = childModules.iterator();

        while ( i.hasNext() )
        {
            final String moduleName = (String) i.next();
            String modulePath = path + moduleName;

            File moduleDir = new File( baseDir, moduleName );

            File moduleProjectFile;

            if ( moduleDir.isDirectory() )
            {
                moduleProjectFile = new File( moduleDir, "pom.xml" );
            }
            else
            {
                // i don't think this should ever happen... but just in case
                // the module references the file-name
                moduleProjectFile = moduleDir;
            }

            try
            {
                // the aim of this goal is to fix problems when the project cannot be parsed by Maven
                // so we have to work with the raw model and not the interpolated parsed model from maven
                Model moduleModel = getRawModel( moduleProjectFile );
                result.put( modulePath, moduleModel );
                childResults.putAll( getReactorModels( modulePath, moduleModel, project, logger ) );
            }
            catch ( IOException e )
            {
                logger.debug( "Could not parse " + moduleProjectFile.getPath(), e );
            }
        }
        result.putAll( childResults ); // more efficient update order if all children are added after siblings
        return result;
    }

    /**
     * Returns all the models that have a specified groupId and artifactId as parent.
     *
     * @param reactor    The map of models keyed by path.
     * @param groupId    The groupId of the parent.
     * @param artifactId The artifactId of the parent.
     * @return a map of models that have a specified groupId and artifactId as parent keyed by path.
     */
    public static Map/*<String,Model>*/ getChildModels( Map/*<String,Model>*/ reactor, String groupId,
                                                        String artifactId )
    {
        final Map result = new LinkedHashMap();
        final Iterator iterator = reactor.entrySet().iterator();
        while ( iterator.hasNext() )
        {
            final Map.Entry entry = (Map.Entry) iterator.next();
            final String path = (String) entry.getKey();
            final Model model = (Model) entry.getValue();
            final Parent parent = model.getParent();
            if ( parent != null && groupId.equals( parent.getGroupId() ) && artifactId.equals(
                parent.getArtifactId() ) )
            {
                result.put( path, model );
            }
        }
        return result;
    }

    /**
     * Returns the model that has the specified groupId and artifactId or <code>null</code> if no such model exists.
     *
     * @param reactor    The map of models keyed by path.
     * @param groupId    The groupId to match.
     * @param artifactId The artifactId to match.
     * @return The model or <code>null</code> if the model was not in the reactor.
     */
    public static Model getModel( Map/*<String,Model>*/ reactor, String groupId, String artifactId )
    {
        final Iterator iterator = reactor.values().iterator();
        while ( iterator.hasNext() )
        {
            final Model model = (Model) iterator.next();

            if ( groupId.equals( getGroupId( model ) ) && artifactId.equals( getArtifactId( model ) ) )
            {
                return model;
            }
        }
        return null;
    }

    /**
     * Returns a count of how many parents a model has in the reactor.
     *
     * @param reactor The map of models keyed by path.
     * @param model   The model.
     * @return The number of parents of this model in the reactor.
     */
    public static int getReactorParentCount( Map/*<String,Model>*/ reactor, Model model )
    {
        if ( model.getParent() == null )
        {
            return 0;
        }
        else
        {
            Model parentModel = getModel( reactor, model.getParent().getGroupId(), model.getParent().getArtifactId() );
            if ( parentModel != null )
            {
                return getReactorParentCount( reactor, parentModel ) + 1;
            }
            return 0;
        }
    }

    /**
     * Reads a file into a StringBuffer.
     *
     * @param outFile The file to read.
     * @return StringBuffer The contents of the file.
     * @throws java.io.IOException when things go wrong.
     */
    public static StringBuffer readFile( File outFile )
        throws IOException
    {
        StringBuffer input;
        BufferedInputStream reader;
        reader = new BufferedInputStream( new FileInputStream( outFile ) );

        byte[] content = new byte[(int) outFile.length()];
        input = new StringBuffer( content.length );
        try
        {
            int length = reader.read( content, 0, content.length );
            input.append( new String( content, 0, length, POM_ENCODING ) );
            return input;
        }
        finally
        {
            reader.close();
        }
    }

}
