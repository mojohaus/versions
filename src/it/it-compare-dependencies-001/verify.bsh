import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "target/depDiffs.txt" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "2.0.10 -> 2.0.9" ) < 0 )
    {
        System.err.println( "Version diff in maven artifact not found" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
