import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<version>1.5.1-SNAPSHOT</version>" ) < 0 )
    {
        System.err.println( "Version of plexus not unlocked to 1.5.1-SNAPSHOT" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
