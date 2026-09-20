import java.io.*;
import java.util.regex.*;

try
{
    File file = new File( basedir, "child/pom.xml" );

    BufferedReader reader = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    StringBuilder buf = new StringBuilder();
    String line = reader.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = reader.readLine();
    }

    Pattern p = Pattern.compile( "\\Q<parent>\\E.*\\Q<version>\\E\\s*2\\.0\\s*\\Q</version>\\E.*\\Q</parent>\\E" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Updated parent of child module" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
