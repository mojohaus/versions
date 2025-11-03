package org.codehaus.mojo.versions.api;

import java.util.Collection;
import java.util.SortedMap;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;

/**
 * A thin adapter over {@link org.eclipse.aether.RepositorySystem} to help
 * discover available versions for provided artifacts.
 *
 * @since 2.20.0
 */
public interface ResolverAdapter {

    /**
     * Attempts to resolve the artifact. If the artifact cannot be resolved, an
     * {@link ArtifactResolutionException} is thrown.
     *
     * @param artifact              The artifact to resolve.
     * @param usePluginRepositories whether to resolve from the plugin repositories or the regular repositories.
     * @throws ArtifactResolutionException if resolution is unsuccessful
     * @since 2.20.0
     */
    void resolveArtifact(Artifact artifact, boolean usePluginRepositories) throws ArtifactResolutionException;

    /**
     * Returns all available versions of the specified artifact that are available in either the local repository, or
     * the
     * appropriate remote repositories.
     * <b>The resulting {@link ArtifactVersions} instance will contain all versions, including snapshots,
     * regardless of the version range specified in the artifact.</b>
     *
     * @param artifact               The artifact to look for versions of.
     * @param usePluginRepositories  {@code true} will consult the pluginRepositories
     * @param useProjectRepositories {@code true} will consult regular project repositories
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 2.15.0
     */
    ArtifactVersions resolveArtifactVersions(
            Artifact artifact, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException;

    /**
     * <p>Returns a sorted map of all possible updates per dependency. The map keys are sorted using
     * {@link org.codehaus.mojo.versions.utils.DependencyComparator}, which boils down to sorting by
     * and groupId:artifactId:classifier.</p>
     * <p>The returned map is sorted by {@code groupId:artifactId:classifier}.</p>
     * <p>Version retrieval is done in parallel, using {@code LOOKUP_PARALLEL_THREADS} threads.</p>
     *
     * @param dependencies           a collection of {@link Dependency} instances to look up.
     * @param usePluginRepositories  {@code true} will consult the pluginRepositories
     * @param useProjectRepositories {@code true} will consult regular project repositories
     * @return map containing the ArtifactVersions object per dependency sorted by groupId, artifactId,
     *         classifier
     * @throws VersionRetrievalException thrown if a version cannot be retrieved
     */
    SortedMap<Dependency, ArtifactVersions> resolveDependencyVersions(
            Collection<Dependency> dependencies, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a plugin. The resulting {@link org.codehaus.mojo.versions.api.PluginUpdatesDetails}
     * instance will only contain versions that are newer than the current version of the plugin.
     *
     * @param plugin         The {@link Plugin} instance to look up.
     * @return The plugin update details.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    PluginUpdatesDetails resolvePluginVersions(Plugin plugin) throws VersionRetrievalException;

    /**
     * <p>Looks up the updates for a set of plugins. The resulting
     * {@link org.codehaus.mojo.versions.api.PluginUpdatesDetails} instance per plugin
     * will only contain versions that are newer than the current version of the plugin.</p>
     * <p>The returned map is sorted by {@code groupId:artifactId:classifier}.</p>
     *
     * @param plugins        A collection of {@link Plugin} instances to look up.
     * @return A map, keyed by plugin, with values of type {@link org.codehaus.mojo.versions.api.PluginUpdatesDetails}.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    SortedMap<Plugin, PluginUpdatesDetails> resolvePluginVersions(Collection<Plugin> plugins)
            throws VersionRetrievalException;
}
