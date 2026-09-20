import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<api>1.1.0-2</api>" ) < 0 )
    {
        System.err.println( "Version of api not updated to 1.1.0-2" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
