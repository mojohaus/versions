import java.io.*;
import java.util.regex.*;

try
{
    File file = new File( basedir, "pom.xml" );

    BufferedReader reader = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    StringBuilder buf = new StringBuilder();
    String line = reader.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = reader.readLine();
    }

    Pattern p = Pattern.compile( "\\Q<parent>\\E.*\\Q<version>\\E\\s*3\\.0-SNAPSHOT\\s*\\Q</version>\\E.*\\Q</parent>\\E" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did not update parent to version 3.0" );
        return false;
    }
    System.out.println( m.group( 0 ) );
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

try
{
    File file = new File( basedir, "pom.xml" );

    BufferedReader reader = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    StringBuilder buf = new StringBuilder();
    String line = reader.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = reader.readLine();
    }

    Pattern p = Pattern.compile( "\\Q<version>\\E\\s*3\\.0-SNAPSHOT\\s*\\Q</version>\\E.*\\Q<type>pom</type>\\E" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did not update  to version 3.0" );
        return false;
    }
    System.out.println( m.group( 0 ) );
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
