import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<junit.version>4.1</junit.version>" ) < 0 )
    {
        System.err.println( "junit.version property was not updated" );
        return false;
    }

    if ( buf.indexOf( "<another.property>1</another.property>" ) < 0 )
    {
        System.err.println( "another.property should not have changed" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
