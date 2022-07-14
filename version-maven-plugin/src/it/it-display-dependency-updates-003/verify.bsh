import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    System.out.println("Running verification...");
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    Pattern p = Pattern.compile( "localhost:dummy-api .* 1.1" );
    Matcher m = p.matcher( buf.toString() );
    if ( m.find() )
    {
        System.out.println( "localhost:dummy-api 1.1 was processed (but should not have been)" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
