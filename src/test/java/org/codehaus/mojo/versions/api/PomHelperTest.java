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
        StringBuffer input = PomHelper.readFile( new File( url.getPath() ) );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );

        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory );

        PomHelper.setProjectVersion( pom, "1" );
        System.out.println( pom.asStringBuffer().toString() );
        PomHelper.setDependencyVersion( pom, "localhost", "it-set-004", "${V1234567890123456789012}", "1" );

        System.out.println( pom.asStringBuffer().toString() );
    }

}
