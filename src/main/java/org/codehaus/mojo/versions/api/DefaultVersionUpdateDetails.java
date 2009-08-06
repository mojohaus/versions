package org.codehaus.mojo.versions.api;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: user
 * Date: 06-Aug-2009
 * Time: 07:51:47
 * To change this template use File | Settings | File Templates.
 */
public class DefaultVersionUpdateDetails
    implements VersionUpdateDetails
{
    private final Map/*<UpdateScope,ArtifactVersion>*/ nextUpdateMap = new HashMap();

    private final Map/*<UpdateScope,ArtifactVersion>*/ latestUpdateMap = new HashMap();

    private final ArtifactVersion[] all;

    private final ArtifactVersion current;

    public DefaultVersionUpdateDetails( VersionDetails v, ArtifactVersion current, boolean includeSnapshots )
        throws ArtifactMetadataRetrievalException
    {
        final Iterator i = Arrays.asList( UpdateScope.values() ).iterator();
        while ( i.hasNext() )
        {
            final UpdateScope updateScope = (UpdateScope) i.next();
            nextUpdateMap.put( updateScope, updateScope.getNext( v, current, includeSnapshots ) );
            latestUpdateMap.put( updateScope, updateScope.getLatest( v, current, includeSnapshots ) );
        }
        all = v.getNewerVersions( current, includeSnapshots );
        this.current = current;
    }

    public final ArtifactVersion getCurrent()
    {
        return current;
    }

    public final ArtifactVersion getNext( UpdateScope updateScope )
    {
        return (ArtifactVersion) nextUpdateMap.get( updateScope );
    }

    public final ArtifactVersion getLatest( UpdateScope updateScope )
    {
        return (ArtifactVersion) latestUpdateMap.get( updateScope );
    }

    public final ArtifactVersion[] getAll()
    {
        return all;
    }
}
