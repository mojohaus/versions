package org.codehaus.mojo.versions;

import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Plugin;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionsHelper;

final class PluginArtifactIdentifier implements ArtifactIdentifier
{
    private final Plugin plugin;

    PluginArtifactIdentifier( Plugin plugin )
    {
        this.plugin = plugin;
    }

    @Override
    public String getGroupId()
    {
        return plugin.getGroupId();
    }

    @Override
    public String getArtifactId()
    {
        return plugin.getArtifactId();
    }

    @Override
    public String getVersion()
    {
        return plugin.getVersion();
    }

    @Override
    public Artifact getArtifact( MavenProject project, VersionsHelper versionsHelper )
    {
        final Artifact artifact = findArtifact( plugin, project.getPluginArtifacts() );
        if ( artifact == null )
        {
            return versionsHelper.createPluginArtifact( plugin.getGroupId(), plugin.getArtifactId(), VersionRange.createFromVersion( plugin.getVersion() ) );
        }
        return artifact;
    }

    @Override
    public String toString()
    {
        return plugin.getGroupId() + ':' + plugin.getArtifactId() + ':' + plugin.getVersion();
    }

    private static Artifact findArtifact( Plugin dependency, Iterable<Artifact> artifacts )
    {
        if ( artifacts == null )
        {
            return null;
        }

        for ( final Artifact artifact : artifacts )
        {
            if ( compare( artifact, dependency ) )
            {
                return artifact;
            }
        }

        return null;
    }

    private static boolean compare( Artifact artifact, Plugin dep )
    {
        return StringUtils.equals( artifact.getGroupId(), dep.getGroupId() )
                && StringUtils.equals( artifact.getArtifactId(), dep.getArtifactId() );
    }
}
