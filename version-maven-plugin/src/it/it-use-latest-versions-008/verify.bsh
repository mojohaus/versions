import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<api.version>2.0.8</api.version>" ) < 0 ) 
    {
        System.err.println( "The version property has been changed which should never happen." );
        return false;
    }
    if ( buf.indexOf( "<version>${api.version}</version>" ) < 0 ) 
    {
        System.err.println( "The version entry in dependencyManagement has been changed which should never happen." );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
