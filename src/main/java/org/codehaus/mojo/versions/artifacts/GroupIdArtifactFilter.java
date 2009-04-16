package org.codehaus.mojo.versions.artifacts;

import org.apache.maven.artifact.Artifact;
import org.codehaus.mojo.versions.utils.RegexUtils;

import java.util.regex.Pattern;

/**
 * An {@link ArtifactFilter} that filters artifacts based on their GroupId.
 */
public class GroupIdArtifactFilter
    implements ArtifactFilter
{
    private final String includeGroupIds;

    private final String excludeGroupIds;

    public GroupIdArtifactFilter( String includeGroupIds, String excludeGroupIds )
    {
        this.includeGroupIds = includeGroupIds;
        this.excludeGroupIds = excludeGroupIds;
    }

    public boolean matches( Artifact artifact )
    {
        Pattern groupIdPattern =
            Pattern.compile( "(.*,)?\\s*" + RegexUtils.quote( artifact.getGroupId() ) + "\\s*(,.*)?" );
        boolean haveIncludeGroupIds = includeGroupIds != null && includeGroupIds.trim().length() != 0;
        boolean haveIncludeGroupIdsMatch = haveIncludeGroupIds && groupIdPattern.matcher( includeGroupIds ).matches();
        boolean haveExcludeGroupIds = excludeGroupIds != null && excludeGroupIds.trim().length() != 0;
        boolean haveExcludeGroupIdsMatch = haveExcludeGroupIds && groupIdPattern.matcher( excludeGroupIds ).matches();
        return ( !haveIncludeGroupIds || haveIncludeGroupIdsMatch ) && ( !haveExcludeGroupIds
            || !haveExcludeGroupIdsMatch || haveIncludeGroupIdsMatch );
    }
}
