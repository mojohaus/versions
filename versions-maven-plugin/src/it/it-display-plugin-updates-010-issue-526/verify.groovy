import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import java.util.regex.*;

try
{
    File file = new File( basedir, "build.log" );
    String buf = FileUtils.fileRead( file );

    Pattern p1 = Pattern.compile( "\\QNo plugins require a newer version of Maven than specified by the pom.\\E" );
    Matcher m1 = p1.matcher( buf.toString() );
    Pattern p2 = Pattern.compile( "\\Qlocalhost:dummy-maven-plugin\\E\\s*\\.*\\s*\\Q1.0 -> 3.1\\E" );
    Matcher m2 = p2.matcher( buf.toString() );
    if ( !m1.find() )
    {
        System.out.println( "Did not correctly detect minimum Maven build version 3.0 of this project" );
        return false;
    }
    if ( !m2.find() )
    {
        System.out.println( "Did not suggest updating dummy-maven-plugin to version 3.1" );
        return false;
    }
    System.out.println( m1.group( 0 ) );
    System.out.println( m2.group( 0 ) );
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
