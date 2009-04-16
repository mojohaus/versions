package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;

/**
 * An {@link org.codehaus.mojo.versions.artifacts.ArtifactFilter} that matches everything.
 */
public class DefaultArtifactFilter
    implements ArtifactFilter
{
    public boolean matches( Artifact artifact )
    {
        return true;
    }
}
