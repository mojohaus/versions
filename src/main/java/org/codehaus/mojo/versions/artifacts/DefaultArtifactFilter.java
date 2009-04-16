package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;

/**
 * An {@link ArtifactFilter} that matches everything.
 */
public class DefaultArtifactFilter
    implements ArtifactFilter
{
    public boolean include( Artifact artifact )
    {
        return true;
    }
}
