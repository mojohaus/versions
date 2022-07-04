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
        .check( "root pom unchanged", "pom.xml", "/project/version", "1.2.0-SNAPSHOT" )
        .check( "module-a1 changed", "module-a1/pom.xml", "/project/version", "2.0.9-SNAPSHOT" )
        .check( "module-a1/module-b1 parent changed", "module-a1/module-b1/pom.xml", "/project/parent/version", "2.0.9-SNAPSHOT" )
        .check( "module-a1/module-b1 remains unspecified", "module-a1/module-b1/pom.xml", "/project/version", "" )
        .check( "module-a1/module-b2 parent changed", "module-a1/module-b2/pom.xml", "/project/parent/version", "2.0.9-SNAPSHOT" )
        .check( "module-a1/module-b2 version unchanged", "module-a1/module-b2/pom.xml", "/project/version", "2.0.7-SNAPSHOT" )
        .check( "module-a1/module-b3 parent changed", "module-a1/module-b3/pom.xml", "/project/parent/version", "2.0.9-SNAPSHOT" )
        .check( "module-a1/module-b3 version unchanged", "module-a1/module-b3/pom.xml", "/project/version", "2.0.8-SNAPSHOT" )
        .check( "module-a2 changed", "module-a2/pom.xml", "/project/version", "2.0.9-SNAPSHOT" )
        .check( "module-a2 dependency changed", "module-a2/pom.xml", "/project/dependencies/dependency/version", "2.0.9-SNAPSHOT" )
        .result;
