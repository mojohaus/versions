import java.io.*;
import org.codehaus.plexus.util.FileUtils;

try
{
    File theParentFile = new File( basedir, "the-parent/pom.xml" );
    File theGrandParentFile = new File( basedir, "the-grandparent/pom.xml" );
    File moduleA1File = new File( basedir, "module-a1/pom.xml" );
    File moduleA2File = new File( basedir, "module-a2/pom.xml" );
    String theParent = FileUtils.fileRead( theParentFile, "UTF-8" );
    String theGrandParent = FileUtils.fileRead( theParentFile, "UTF-8" );
    String moduleA1 = FileUtils.fileRead( theParentFile, "UTF-8" );
    String moduleA2 = FileUtils.fileRead( theParentFile, "UTF-8" );

    // all 1.0 references are updated to 1.1-SNAPSHOT (including parents)
    if ( theParent.indexOf( "<version>1.0</version>" ) > 0 ||
         theGrandParent.indexOf( "<version>1.0</version>" ) > 0 ||
         moduleA1.indexOf( "<version>1.0</version>" ) > 0 ||
         moduleA2.indexOf( "<version>1.0</version>" ) > 0)

    {
        System.err.println( "version 1.0 not bumped to 1.1-SNAPSHOT" );
        return false;
    }
}
catch( Throwable t )
{
    t.printStackTrace();
    return false;
}

return true;
