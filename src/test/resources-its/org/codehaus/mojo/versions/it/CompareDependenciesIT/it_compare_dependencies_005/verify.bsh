import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "target/depDiffs.txt" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf("junit.version") > 0)
    {
        System.err.println( "junit.version property reference should not be found" );
        return false;
    }

    if ( buf.indexOf( "4.1 -> 4.1" ) > 0 )
    {
        System.err.println( "junit.version property update should not be found" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
