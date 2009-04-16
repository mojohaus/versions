package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;
import org.codehaus.mojo.versions.utils.RegexUtils;

import java.util.regex.Pattern;

/**
 * An {@link org.codehaus.mojo.versions.artifacts.ArtifactFilter} that filters artifacts based on their ArtifactId.
 */
public class ArtifactIdArtifactFilter
    implements ArtifactFilter
{
    private final String includeArtifactIds;

    private final String excludeArtifactIds;

    public ArtifactIdArtifactFilter( String includeArtifactIds, String excludeArtifactIds )
    {
        this.includeArtifactIds = includeArtifactIds;
        this.excludeArtifactIds = excludeArtifactIds;
    }

    public boolean matches( Artifact artifact )
    {
        Pattern artifactIdPattern =
            Pattern.compile( "(.*,)?\\s*" + RegexUtils.quote( artifact.getArtifactId() ) + "\\s*(,.*)?" );
        boolean haveIncludeArtifactIds = includeArtifactIds != null && includeArtifactIds.trim().length() != 0;
        boolean haveIncludeArtifactIdsMatch = haveIncludeArtifactIds && artifactIdPattern.matcher( includeArtifactIds ).matches();
        boolean haveExcludeArtifactIds = excludeArtifactIds != null && excludeArtifactIds.trim().length() != 0;
        boolean haveExcludeArtifactIdsMatch = haveExcludeArtifactIds && artifactIdPattern.matcher( excludeArtifactIds ).matches();
        return ( !haveIncludeArtifactIds || haveIncludeArtifactIdsMatch ) && ( !haveExcludeArtifactIds
            || !haveExcludeArtifactIdsMatch || haveIncludeArtifactIdsMatch );
    }
}