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
        pomReader = new MavenXpp3Reader();
        pomModel = pomReader.read( new InputStreamReader( new FileInputStream( parentPomFile ) ) );
   
        result = result && verifyThatDependencyHasNotBeenChanged( pomModel.getDependencies() );
        result = result && verifyThatTheProjectVerisonHasBeenChanged( pomModel );

    }
    catch( Throwable t )
    {
        t.printStackTrace();
        return false;
    }
    
    return result;
}


// == Support functions ===
boolean verifyThatTheProjectVerisonHasBeenChanged( model ) {

      if ( !"2.3".equals(model.getVersion()) )
      {
          System.err.println("Model version not been changed " + model.getId() );
          return false;
      }
    return true;
}

boolean verifyThatDependencyHasNotBeenChanged( dependencies ) {

      Dependency dep = dependencies.get(0)
      if ( !"2.2".equals(dep.getVersion()) )
      {
          System.err.println("Dependency has been changed " + dep.getId() );
          return false;
      }
    return true;
}

return test();
