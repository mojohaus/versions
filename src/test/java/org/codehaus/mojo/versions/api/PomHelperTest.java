package org.codehaus.mojo.versions.api;

import junit.framework.TestCase;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.stax2.XMLInputFactory2;

import javax.xml.stream.XMLInputFactory;
import java.io.File;
import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.net.URL;

/**
 * Created by IntelliJ IDEA.
 * User: user
 * Date: 17-Jun-2009
 * Time: 18:40:49
 * To change this template use File | Settings | File Templates.
 */
public class PomHelperTest
    extends TestCase
{
    public void testLongProperties() throws Exception {
        URL url = getClass().getResource( "PomHelperTest.testLongProperties.pom.xml" );        
        StringBuffer input = PomHelper.readFile( new File(url.getPath()) );
        
        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
        
        ModifiedPomXMLEventReader pom = new ModifiedPomXMLEventReader( input, inputFactory );

        PomHelper.setProjectVersion( pom, "1" );
        PomHelper.setDependencyVersion( pom, "localhost", "it-set-004", "${V1234567890123456789012}", "1" );
        
        System.out.println(pom.asStringBuffer().toString());
    }
    
    /**
     * Reads a file into a StringBuffer.
     *
     * @param outFile The file to read.
     * @return StringBuffer The contents of the file.
     * @throws java.io.IOException when things go wrong.
     */
    protected final StringBuffer readFile( File outFile )
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
            input.append( new String( content, 0, length, PomHelper.POM_ENCODING ) );
            return input;
        }
        finally
        {
            reader.close();
        }
    }

}
