import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );
    Pattern p = Pattern.compile( "\\QIgnoring dependency with no version: localhost:dummy-api:jar\\E" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "cannot find skipping of dummy-api dependency" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
