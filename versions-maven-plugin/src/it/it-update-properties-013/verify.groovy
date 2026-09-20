import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<api>3.0</api>" ) > 0 )
    {
        System.err.println( "Version updated to 3.0 when it shouldn't have been due to not being covered by the inclusion pattern" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
