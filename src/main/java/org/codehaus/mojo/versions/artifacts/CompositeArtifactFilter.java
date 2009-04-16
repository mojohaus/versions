package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;

/**
 * An {@link org.codehaus.mojo.versions.artifacts.ArtifactFilter} that combines multiple
 * {@link org.codehaus.mojo.versions.artifacts.ArtifactFilter} instances.
 */
public class CompositeArtifactFilter
    implements ArtifactFilter
{
    /**
     * The filters to combine.
     */
    private final ArtifactFilter[] filters;

    /**
     * controls whether all filters must match or any filter may match.
     */
    private final boolean matchAll;

    public CompositeArtifactFilter( ArtifactFilter[] filters, boolean matchAll )
    {
        this.filters = filters;
        this.matchAll = matchAll;
    }

    public boolean matches( Artifact artifact )
    {
        if ( matchAll )
        {
            for ( int i = 0; i < filters.length; i++ )
            {
                if ( !filters[i].matches( artifact ) )
                {
                    return false;
                }
            }
            return true;
        }
        for ( int i = 0; i < filters.length; i++ )
        {
            if ( filters[i].matches( artifact ) )
            {
                return true;
            }
        }
        return false;
    }
}
