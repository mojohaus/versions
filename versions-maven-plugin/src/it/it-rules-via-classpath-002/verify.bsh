import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file, "UTF-8" );
    String expectedMessage = "Resource \"classpath:///package/foo/bar/rules.xml\" not found in classpath.";

    if ( buf.indexOf( expectedMessage ) >= 0 )
    {
        System.err.println( "File resource rules.xml should have been found in the classpath." );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
