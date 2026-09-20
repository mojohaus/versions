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
        childPomFile = new File( basedir + File.separator + "child" + File.separator + "pom.xml" );
    
        result = result && verifyThatRangesAreResolvedInParentPom( parentPomFile );
        result = result && verifyThatNoVersionsAreSetInChildPom( childPomFile );
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
    
    return verifyThatDependenciesContainNoRanges( pomModel.getDependencyManagement().getDependencies() );
}

boolean verifyThatNoVersionsAreSetInChildPom( childPomFile ) {
    pomReader = new MavenXpp3Reader();
    pomModel = pomReader.read( new InputStreamReader( new FileInputStream( childPomFile ) ) );
    
    return verifyThatDependenciesContainNoVersion( pomModel.getDependencies() );
}

boolean verifyThatDependenciesContainNoRanges( dependencies ) {
    for (dependency: dependencies)
    {
        if ( dependency.getVersion().contains( "," ) )
        {
            System.err.println("Unresolved dependency-range: " + dependencyToString( dependency ) );
            return false;
        }
    }
    return true;
}

boolean verifyThatDependenciesContainNoVersion( dependencies ) {
    for (dependency: dependencies)
    {
        if ( dependency.getVersion() != null )
        {
            System.err.println( "Declared version (version should not be set): " + dependencyToString( dependency ) );
            return false;
        }
    }
    return true;
}
  
String dependencyToString( dependency ) {
    return dependency.getGroupId() + ":" + dependency.getArtifactId() + ":" + dependency.getVersion();
}



return test();
