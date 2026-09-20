import java.io.*;
import java.util.regex.*;

try
{

    File file = new File( basedir, "module-a2/pom.xml" );

    BufferedReader in = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    StringBuilder buf = new StringBuilder();
    String line = in.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = in.readLine();
    }

    Pattern p1 = Pattern.compile( "\\Q<parent>\\E.*\\Q<version>1.0</version>\\E.*\\Q</parent>\\E" );
    Matcher m1 = p1.matcher( buf.toString() );
    Pattern p2 = Pattern.compile( "\\Q<dependency>\\E.*\\Q<version>1.1-SNAPSHOT</version>\\E.*\\Q</dependency>\\E" );
    Matcher m2 = p2.matcher( buf.toString() );

    if ( !m1.find() )
    {
        System.err.println( "Parent was updated incorrectly" );
        return false;
    }
    if ( !m2.find() )
    {
        System.err.println( "Dependency with module-a1 was not updated correctly" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
