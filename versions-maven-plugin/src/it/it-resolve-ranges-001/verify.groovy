import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( ( buf.indexOf( "<version>2.0.6</version>" ) < 0 )
            && ( buf.indexOf( "<version>2.0.7</version>" ) < 0 )
            && ( buf.indexOf( "<version>2.0.8</version>" ) < 0 )
       )
    {
        System.err.println( "Version of maven-project not resolved to 2.0.6, 2.0.7, or 2.0.8" );
        return false;
    }
    if ( buf.indexOf( "<version>1.5.15</version>" ) < 0 )
    {
        System.err.println( "Version of plexus not resolved to 1.5.15" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
