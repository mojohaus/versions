package org.codehaus.mojo.versions.api;

import java.util.List;

import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.settings.Settings;

/**
 * Default implementation for VersionsHelperFactory.
 *
 * @author Xeno Amess
 * @since 2.11.1
 */
public class DefaultVersionsHelperFactory implements VersionsHelperFactory{

    @Override
    public VersionsHelper buildVersionsHelper(
            ArtifactFactory artifactFactory,
            ArtifactResolver artifactResolver,
            ArtifactMetadataSource artifactMetadataSource,
            List<ArtifactRepository> remoteArtifactRepositories,
            List<ArtifactRepository> remotePluginRepositories,
            ArtifactRepository localRepository,
            WagonManager wagonManager,
            Settings settings,
            String serverId,
            String rulesUri,
            Log log,
            MavenSession mavenSession,
            PathTranslator pathTranslator
    ) throws MojoExecutionException {
        return new DefaultVersionsHelper (
                artifactFactory,
                artifactResolver,
                artifactMetadataSource,
                remoteArtifactRepositories,
                remotePluginRepositories,
                localRepository,
                wagonManager,
                settings,
                serverId,
                rulesUri,
                log,
                mavenSession,
                pathTranslator
        );
    }

}
