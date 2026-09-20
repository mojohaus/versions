import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "child2/pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<api>2.0</api>" ) < 0 )
    {
        System.err.println( "Version of api not resolved to 2.0" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
