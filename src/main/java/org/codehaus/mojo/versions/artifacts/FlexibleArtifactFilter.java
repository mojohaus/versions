package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.resolver.filter.ExcludesArtifactFilter;
import org.apache.maven.artifact.Artifact;

import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: user
 * Date: 16-Apr-2009
 * Time: 22:47:47
 * To change this template use File | Settings | File Templates.
 */
public class FlexibleArtifactFilter implements ArtifactFilter
{
    private final Pattern includeGroupIdPattern;
    private final Pattern includeArtifactIdPattern;
    private final Pattern includeTypePattern;
    private final Pattern includeClassifierPattern;
    private final Pattern excludeGroupIdPattern;
    private final Pattern excludeArtifactIdPattern;
    private final Pattern excludeTypePattern;
    private final Pattern excludeClassifierPattern;


    public boolean include( Artifact artifact )
    {
        ArtifactFilter f = new ExcludesArtifactFilter( ); 
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
