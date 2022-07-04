import org.apache.commons.lang.StringUtils

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory

class Checker
{
    def result = true;
    def basedir;

    public Checker(File basedir) {
        this.basedir = basedir;
    }

    def readXPath( String pom, String xPathExpression )
    {
        def stream = new FileInputStream( new File( basedir, pom ) );
        try
        {
            return XPathFactory.newInstance()
                    .newXPath()
                    .evaluate( xPathExpression, DocumentBuilderFactory.newInstance()
                    .newDocumentBuilder()
                    .parse( stream ).documentElement );
        }
        finally
        {
            stream.close();
        }
    }

    Checker check( String message, String pom, String xpath, String expected )
    {
        if ( result )
        {
            try
            {
                def actual = readXPath( pom, xpath )
                if ( !StringUtils.equals( expected, actual ) )
                {
                    System.out.println( pom + " [xpath:" + xpath + "] expected '" + expected + "' found '" + actual + "' : " + message );
                    result = false;
                }
            }
            catch ( Throwable t )
            {
                t.printStackTrace();
                result = false;
            }
        }
        return this;
    }
}

return new Checker(basedir)
        .check( "pom.xml change", "pom.xml", "/project/version", "1.0.0")
        .check( "fake-api/pom.xml change", "fake-api/pom.xml", "/project/parent/version", "1.0.0")
        .check( "fake-api/pom.xml change", "fake-api/pom.xml", "/project/version", "1.0.0")
        .check( "the-bom/pom.xml change", "the-bom/pom.xml", "/project/version", "1.0.0")
        .result;
