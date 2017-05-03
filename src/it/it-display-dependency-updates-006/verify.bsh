import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    System.out.println("Running verification...");
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    // This is the pattern for the artifact from dependencyManagement
    Pattern p = Pattern.compile( "localhost:issue-114-artifact .* 1\\.0 -> 1\\.1" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "localhost:issue-114-artifact 1.0 -> 1.1 was not processed." );
        return false;
    }
    
    Pattern p = Pattern.compile( "localhost:dummy-api .* 2\\.0 -> 3\\.0" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "localhost:dummy-api 2.0 -> 2.0 was not processed." );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
