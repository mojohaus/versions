import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File file = new File( basedir, "pom.xml" );
    String buf = FileUtils.fileRead( file, "UTF-8" );

    if ( buf.indexOf( "<versionProp>1.9.1-SNAPSHOT</versionProp>" ) < 0 )
    {
        System.err.println( "versionProp has been changed which shouldn't happen." );
        return false;
    }
    if ( buf.indexOf( "<version>${versionProp}</version>" ) < 0 )
    {
        System.err.println( "version entry has been changed which should not happen." );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
