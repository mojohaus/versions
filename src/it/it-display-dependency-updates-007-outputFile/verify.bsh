import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    Pattern p = Pattern.compile( "\\Qlocalhost:dummy-api\\E\\s*\\.*\\s*1\\.1\\s+->\\s+3\\.0" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did not suggest updating dummy-api to version 3.0" );
        return false;
    }
    System.out.println( m.group( 0 ) );

    // validate outputFile
    File outputFile = new File( basedir, "dependencyUpdate.txt" );
    if (!outputFile.exists()) {
        System.out.println( "outputFile not found: " + outputFile.getPath() );
        return false;
    }
    buf = FileUtils.fileRead( file );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did not suggest updating dummy-api to version 3.0 in outputFile" );
        return false;
    }

}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
