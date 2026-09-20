import java.io.*;
import org.codehaus.plexus.util.FileUtils;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.model.Model;
import org.apache.maven.model.Dependency;
import java.util.List;


boolean test()
{
    result = true;
    try
    {
        parentPomFile = new File( basedir, "pom.xml" );
    
        result = result && verifyThatRangesAreResolvedInParentPom( parentPomFile );
    }
    catch( Throwable t )
    {
        t.printStackTrace();
        return false;
    }
    
    return result;
}


// == Support functions ===

boolean verifyThatRangesAreResolvedInParentPom( parentPomFile ) {
    pomReader = new MavenXpp3Reader();
    pomModel = pomReader.read( new InputStreamReader( new FileInputStream( parentPomFile ) ) );
    
    return verifyThatDependenciesContainNoRanges( pomModel.getParent() );
}

boolean verifyThatDependenciesContainNoRanges( artifact ) {

      if ( !"2.0".equals(artifact.getVersion()) )
      {
          System.err.println("Unresolved dependency-range: " + artifact.getId() );
          return false;
      }
    return true;
}

return test();
