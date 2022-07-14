import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );


    if ( buf.indexOf( "<api>3.0</api>" ) < 0 )
    {
        System.err.println( "${api} version not updated to 3.0" );
        return false;
    }
    if ( buf.indexOf( "<impl>2.2</impl>" ) < 0 )
    {
        System.err.println( "${impl} version not updated to 2.2" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
