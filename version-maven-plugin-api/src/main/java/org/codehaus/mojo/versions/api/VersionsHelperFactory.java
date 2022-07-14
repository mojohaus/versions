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
 * Factory of VersionsHelper.
 *
 * @author Xeno Amess
 * @since 2.11.1
 */
public interface VersionsHelperFactory {

    /**
     * build VersionsHelper
     *
     * @param artifactFactory The artifact factory.
     * @param artifactResolver Artifact resolver
     * @param artifactMetadataSource The artifact metadata source to use.
     * @param remoteArtifactRepositories The remote artifact repositories to consult.
     * @param remotePluginRepositories The remote plugin repositories to consult.
     * @param localRepository The local repository to consult.
     * @param wagonManager The wagon manager (used if rules need to be retrieved).
     * @param settings The settings (used to provide proxy information to the wagon manager).
     * @param serverId The serverId hint for the wagon manager.
     * @param rulesUri The URL to retrieve the versioning rules from.
     * @param log The {@link org.apache.maven.plugin.logging.Log} to send log messages to.
     * @param mavenSession The maven session information.
     * @param pathTranslator The path translator component. @throws org.apache.maven.plugin.MojoExecutionException If
     *            things go wrong.
     * @throws MojoExecutionException if something goes wrong.
     * @since 2.11.1
     * @return VersionsHelper
     */
    VersionsHelper buildVersionsHelper(
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
    )
    throws MojoExecutionException;

}
