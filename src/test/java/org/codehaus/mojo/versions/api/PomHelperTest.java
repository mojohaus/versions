package org.codehaus.mojo.versions.api;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.stax2.XMLInputFactory2;
import org.junit.jupiter.api.Test;

import javax.xml.stream.XMLInputFactory;
import java.io.File;
import java.io.StringReader;
import java.net.URL;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the methods of {@link PomHelper}.
 */
public class PomHelperTest
{
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
        StringBuilder input = PomHelper.readXmlFile( new File( url.getPath() ) );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );

        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory );

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
        StringBuilder input = PomHelper.readXmlFile( new File( url.getPath() ) );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );

        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory );

        String oldVersion = PomHelper.getProjectVersion( pom );

        String newVersion = "1";

        assertTrue( PomHelper.setProjectVersion( pom, newVersion ), "The pom has been modified" );

        assertEquals( newVersion, PomHelper.getProjectVersion( pom ) );

        assertNotSame( oldVersion, newVersion );
    }

    @Test
    public void testGroupIdNotOnChildPom()
            throws Exception
    {
        URL url = getClass().getResource( "PomHelperTest.noGroupIdOnChild.pom.xml" );
        StringBuilder input = PomHelper.readXmlFile( new File( url.getPath() ) );
        MavenXpp3Reader reader = new MavenXpp3Reader();
        Model model = reader.read(new StringReader(input.toString()));

        assertEquals("org.myorg", PomHelper.getGroupId(model));
    }

    @Test
    public void test_Version_Version_Equal()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "1.0.8" ) );
    }

    @Test
    public void test_Version_Version_Differ()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "1.0.0" ) );
    }

    @Test
    public void test_Version_Range_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "[1.0.3,1.1.0]" ) );
    }

    @Test
    public void test_Version_Range_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[0.0.1,1.0.0]" ) );
    }

    @Test
    public void test_Version_LeftOpenRange_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[,1.0.0]" ) );
    }

    @Test
    public void test_Version_RightOpenRange_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[1.1.0,)" ) );
    }

    @Test
    public void test_Empty_Range()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "" ) );
    }

    @Test
    public void test_Range_Empty()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "" ) );
    }

    @Test
    public void test_Range_Range_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "[1.0.7,1.1.0]" ) );

    }

    @Test
    public void test_Range_Range_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "[1.0.7,1.1.0]" ) );

    }

    @Test
    public void test_Range_Version_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "1.0.8" ) );

    }

    @Test
    public void test_Range_Version_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.0,2.0.0]", "1.0.8" ) );

    }

}
