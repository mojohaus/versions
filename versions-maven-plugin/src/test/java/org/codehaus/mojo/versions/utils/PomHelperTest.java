package org.codehaus.mojo.versions.utils;


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

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;

import java.io.File;
import java.io.StringReader;
import java.net.URL;
import java.util.List;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.stax2.XMLInputFactory2;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.ModifiedPomXMLEventReaderUtils.matches;
import static org.codehaus.stax2.XMLInputFactory2.P_PRESERVE_LOCATION;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

/**
 * Tests the methods of {@link PomHelper}.
 */
public class PomHelperTest
{
    private static final XMLInputFactory INPUT_FACTORY = XMLInputFactory2.newInstance();

    @BeforeClass
    public static void setUp()
    {
        INPUT_FACTORY.setProperty( P_PRESERVE_LOCATION, Boolean.TRUE );
    }

    /**
     * Tests if imported POMs are properly read from dependency management section. Such logic is required to resolve
     * <a href="https://github.com/mojohaus/versions-maven-plugin/issues/134">bug #134</a>
     *
     * @throws Exception if the test fails.
     */
    @Test
    public void testImportedPOMsRetrievedFromDependencyManagement()
        throws Exception
    {
        URL url = getClass().getResource( "PomHelperTest.dependencyManagementBOMs.pom.xml" );
        assert url != null;
        File file = new File( url.getPath() );
        StringBuilder input = PomHelper.readXmlFile( file );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( P_PRESERVE_LOCATION, Boolean.TRUE );

        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory, file.getAbsolutePath() );

        List<Dependency> dependencies = PomHelper.readImportedPOMsFromDependencyManagementSection( pom );

        assertNotNull( dependencies );
        assertEquals( 1, dependencies.size() );

        Dependency dependency = dependencies.get( 0 );
        assertEquals( "org.group1", dependency.getGroupId() );
        assertEquals( "artifact-pom", dependency.getArtifactId() );
        assertEquals( "1.0-SNAPSHOT", dependency.getVersion() );
        assertEquals( "import", dependency.getScope() );
        assertEquals( "pom", dependency.getType() );
    }

    /**
     * Tests what happens when changing a long property substitution pattern, e.g.
     * <a href="http://jira.codehaus.org/browse/MVERSIONS-44">MVERSIONS-44</a>
     *
     * @throws Exception if the test fails.
     */
    @Test
    public void testLongProperties()
        throws Exception
    {
        URL url = getClass().getResource( "PomHelperTest.testLongProperties.pom.xml" );
        assert url != null;
        File file = new File( url.getPath() );
        StringBuilder input = PomHelper.readXmlFile( file );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( P_PRESERVE_LOCATION, Boolean.TRUE );

        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory, file.getAbsolutePath() );

        String oldVersion = PomHelper.getProjectVersion( pom );

        String newVersion = "1";

        assertTrue( "The pom has been modified", PomHelper.setProjectVersion( pom, newVersion ) );

        assertEquals( newVersion, PomHelper.getProjectVersion( pom ) );

        assertNotSame( oldVersion, newVersion );
    }

    @Test
    public void testGroupIdNotOnChildPom()
        throws Exception
    {
        URL url = getClass().getResource( "PomHelperTest.noGroupIdOnChild.pom.xml" );
        assert url != null;
        StringBuilder input = PomHelper.readXmlFile( new File( url.getPath() ) );
        MavenXpp3Reader reader = new MavenXpp3Reader();
        Model model = reader.read( new StringReader( input.toString() ) );

        assertEquals( "org.myorg", PomHelper.getGroupId( model ) );
    }

    @Test
    public void testVersionVersionEqual()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "1.0.8" ) );
    }

    @Test
    public void testVersionVersionDiffer()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "1.0.0" ) );
    }

    @Test
    public void testVersionRangeIntersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "[1.0.3,1.1.0]" ) );
    }

    @Test
    public void testVersionRangeDisjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[0.0.1,1.0.0]" ) );
    }

    @Test
    public void testVersionLeftOpenRangeDisjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[,1.0.0]" ) );
    }

    @Test
    public void testVersionRightOpenRangeDisjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[1.1.0,)" ) );
    }

    @Test
    public void testEmptyRange()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "" ) );
    }

    @Test
    public void testRangeEmpty()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "" ) );
    }

    @Test
    public void testRangeRangeIntersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "[1.0.7,1.1.0]" ) );

    }

    @Test
    public void testRangeRangeDisjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "[1.0.7,1.1.0]" ) );

    }

    @Test
    public void testRangeVersionDisjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "1.0.8" ) );

    }

    @Test
    public void testRangeVersionIntersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.0,2.0.0]", "1.0.8" ) );

    }

    @Test
    public void testSetElementValueExistingValue() throws XMLStreamException
    {
        ModifiedPomXMLEventReader xmlEventReader = new ModifiedPomXMLEventReader(
                new StringBuilder( "<super-parent><parent><child>test</child></parent></super-parent>" ),
                INPUT_FACTORY, null );

        assertThat( PomHelper.setElementValue( xmlEventReader, "/super-parent/parent",
                "child", "value" ), is( true ) );
        assertThat( xmlEventReader,
                matches( "<super-parent><parent><child>value</child></parent></super-parent>" ) );
    }

    @Test
    public void testSetElementValueEmptyChild() throws XMLStreamException
    {
        ModifiedPomXMLEventReader xmlEventReader = new ModifiedPomXMLEventReader(
                new StringBuilder( "<super-parent><parent><child/></parent></super-parent>" ), INPUT_FACTORY, null );

        assertThat( PomHelper.setElementValue( xmlEventReader, "/super-parent/parent",
                "child", "value" ), is( true ) );
        assertThat( xmlEventReader,
                matches( "<super-parent><parent><child>value</child></parent></super-parent>" ) );
    }

    @Test
    public void testSetElementValueNewValueEmptyParent() throws XMLStreamException
    {
        ModifiedPomXMLEventReader xmlEventReader = new ModifiedPomXMLEventReader(
                new StringBuilder( "<super-parent><parent/></super-parent>" ), INPUT_FACTORY, null );

        assertThat( PomHelper.setElementValue( xmlEventReader, "/super-parent/parent",
                "child", "value" ), is( true ) );
        assertThat( xmlEventReader,
                matches( "<super-parent><parent><child>value</child></parent></super-parent>" ) );
    }

    @Test
    public void testSetElementValueNewValueNoChild() throws XMLStreamException
    {
        ModifiedPomXMLEventReader xmlEventReader = new ModifiedPomXMLEventReader(
                new StringBuilder( "<super-parent><parent><child2/></parent></super-parent>" ), INPUT_FACTORY, null );

        assertThat( PomHelper.setElementValue( xmlEventReader, "/super-parent/parent",
                "child", "value" ), is( true ) );
        assertThat( xmlEventReader,
                matches( "<super-parent><parent><child2/><child>value</child></parent></super-parent>" ) );
    }

    @Test
    public void testSetProjectValueNewValueNonEmptyParent() throws XMLStreamException
    {
        ModifiedPomXMLEventReader xmlEventReader = new ModifiedPomXMLEventReader(
                new StringBuilder( "<super-parent><parent><child>test</child></parent></super-parent>" ), INPUT_FACTORY,
                null );

        assertThat( PomHelper.setElementValue( xmlEventReader, "/super-parent/parent",
                "child", "value" ), is( true ) );
        assertThat( xmlEventReader,
                matches( "<super-parent><parent><child>value</child></parent></super-parent>" ) );
    }
}
