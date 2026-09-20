import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    // output should not contain any message like 'localhost:dummy-maven-plugin .....' because this plugin should be ignored due to its version defined in parent POM
    Pattern p = Pattern.compile( "\\Qlocalhost:dummy-maven-plugin\\E\\s*\\.+" );
    Matcher m = p.matcher( buf.toString() );
    if ( m.find() )
    {
        System.out.println( "dummy-maven-plugin should not be listed in output, version is defined in Parent POM" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
