import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file, "UTF-8" );
    String expectedMessage = "Resource \"classpath:///package/foo/bar/rule-set.xml\" not found in classpath.";

    if ( buf.indexOf( expectedMessage ) < 0 )
    {
        System.err.println( "Build should have failed as the requested resource is not present." );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
