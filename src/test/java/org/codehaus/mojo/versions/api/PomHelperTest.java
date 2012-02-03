package org.codehaus.mojo.versions.api;

import junit.framework.TestCase;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.stax2.XMLInputFactory2;

import javax.xml.stream.XMLInputFactory;
import java.io.File;
import java.net.URL;

/**
 * Tets the methods of {@link PomHelper}.
 */
public class PomHelperTest
    extends TestCase
{
    /**
     * Tests what happens when changing a long property substitution pattern, e.g.
     * <a href="http://jira.codehaus.org/browse/MVERSIONS-44">MVERSIONS-44</a>
     *
     * @throws Exception if the test fails.
     */
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

        assertTrue( "The pom has been modified", PomHelper.setProjectVersion( pom, newVersion ) );

        assertEquals( newVersion, PomHelper.getProjectVersion( pom ) );

        assertNotSame( oldVersion, newVersion );
    }

    public void test_Version_Version_Equal()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "1.0.8" ) );
    }

    public void test_Version_Version_Differ()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "1.0.0" ) );
    }

    public void test_Version_Range_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "[1.0.3,1.1.0]" ) );
    }

    public void test_Version_Range_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[0.0.1,1.0.0]" ) );
    }

    public void test_Version_LeftOpenRange_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[,1.0.0]" ) );
    }

    public void test_Version_RightOpenRange_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "1.0.8", "[1.1.0,)" ) );
    }

    public void test_Empty_Range()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "1.0.8", "" ) );
    }

    public void test_Range_Empty()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "" ) );
    }

    public void test_Range_Range_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.5,1.0.8]", "[1.0.7,1.1.0]" ) );

    }

    public void test_Range_Range_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "[1.0.7,1.1.0]" ) );

    }

    public void test_Range_Version_Disjoint()
        throws Exception
    {
        assertFalse( PomHelper.isVersionOverlap( "[1.0.5,1.0.6]", "1.0.8" ) );

    }

    public void test_Range_Version_Intersect()
        throws Exception
    {
        assertTrue( PomHelper.isVersionOverlap( "[1.0.0,2.0.0]", "1.0.8" ) );

    }

}
