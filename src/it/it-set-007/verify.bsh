import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "pom.xml" );

    BufferedReader in = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    StringBuilder buf = new StringBuilder();
    String line = in.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = in.readLine();
    }

    Pattern p = Pattern.compile( "\\Q<version>\\E\\s*1\\.2\\.0-SNAPSHOT\\s*\\Q</version>\\E" );
    Matcher m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did modify project version in parent pom" );
        return false;
    }

    file = new File( basedir, "module-a1/pom.xml" );

    in = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    buf = new StringBuilder();
    line = in.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = in.readLine();
    }

    p = Pattern.compile( "\\Q<version>\\E\\s*2\\.0\\.7-SNAPSHOT\\s*\\Q</version>\\E" );
    m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did update project version in module-a1/pom" );
        return false;
    }

    file = new File( basedir, "module-a2/pom.xml" );

    in = new BufferedReader( new InputStreamReader( new FileInputStream( file ), "UTF-8" ) );
    buf = new StringBuilder();
    line = in.readLine();
    while ( line != null )
    {
        buf.append( line );
        buf.append( " " );
        line = in.readLine();
    }

    p = Pattern.compile( "\\Q<version>\\E\\s*1\\.0\\.3-SNAPSHOT\\s*\\Q</version>\\E" );
    m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did update project version in module-a2/pom" );
        return false;
    }
    p = Pattern.compile( "\\Q<dependency>\\E.*\\Q<version>\\E\\s*2\\.0\\.6\\s*\\Q</version>\\E.*\\Q</dependency>\\E" );
    m = p.matcher( buf.toString() );
    if ( !m.find() )
    {
        System.out.println( "Did not update dependency version in module-a2/pom" );
        return false;
    }

}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
