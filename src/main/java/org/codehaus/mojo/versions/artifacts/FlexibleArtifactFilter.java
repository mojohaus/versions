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
    private final Pattern includeGroupIdPattern = null;
    private final Pattern includeArtifactIdPattern = null;
    private final Pattern includeTypePattern = null;
    private final Pattern includeClassifierPattern = null;
    private final Pattern excludeGroupIdPattern = null;
    private final Pattern excludeArtifactIdPattern = null;
    private final Pattern excludeTypePattern = null;
    private final Pattern excludeClassifierPattern = null;


    public boolean include( Artifact artifact )
    {
        return false;
    }
}
