package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;

import java.io.Serializable;

/**
 * A filter for {@link Artifact} instances.
 *
 * @since 1.0-alpha-3
 */
public interface ArtifactFilter
    extends Serializable
{
    /**
     * Returns <code>true</code> if the artifact matches the filter's criteria.
     *
     * @param artifact The artifact instance.
     * @return <code>true</code> if the artifact matches the filter's criteria.
     * @since 1.0-alpha-3
     */
    boolean matches( Artifact artifact );
}
