import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    Pattern p = Pattern.compile( "\\Qlocalhost:dummy-api\\E\\s*\\.*\\s*\\Q[1.1,5.0)\\E\\s+->\\s+" );
    Matcher m = p.matcher( buf.toString() );
    if ( m.find() )
    {
        System.out.println( m.group( 0 ) );
        System.out.println( "Did suggest updating dummy-api from range [1.1,5.0)" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
