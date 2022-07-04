import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    Pattern p1 = Pattern.compile( "\\QRequire Maven 2.0.9 to use the following plugin updates:\\E" );
    Matcher m1 = p1.matcher( buf.toString() );
    Pattern p2 = Pattern.compile( "\\Qlocalhost:dummy-maven-plugin\\E \\.* \\Q3.0 -> \\E[0-9\\.]+" );
    Matcher m2 = p1.matcher( buf.toString() );

    if ( m1.find() && m2.find() && m1.start() > m2.start() )
    {
        System.out.println( "Suggested updating dummy-maven-plugin from version 3.0" );
        System.out.println( m1.group( 0 ) );
        System.out.println( m2.group( 0 ) );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
