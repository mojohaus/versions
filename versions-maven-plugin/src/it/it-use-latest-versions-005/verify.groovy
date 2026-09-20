import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<version>2.0.8</version>" ) < 0 )
    {
        System.err.println( "Version has been bumped to something different where it should be keept at 2.0.8" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
