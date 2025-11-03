package org.codehaus.mojo.versions.api.internal;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.RepositoryUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.ResolverAdapter;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;

import static java.util.Optional.of;
import static org.apache.maven.RepositoryUtils.toArtifact;

public class DefaultResolverAdapter implements ResolverAdapter {

    private static final int LOOKUP_PARALLEL_THREADS = 12;

    private final ArtifactFactory artifactFactory;

    private final RepositorySystem repositorySystem;

    private final Log log;

    private final MavenSession mavenSession;

    private final List<RemoteRepository> remotePluginRepositories;

    private final List<RemoteRepository> remoteProjectRepositories;

    public DefaultResolverAdapter(
            ArtifactFactory artifactFactory, RepositorySystem repositorySystem, Log log, MavenSession mavenSession) {
        this.artifactFactory = artifactFactory;
        this.repositorySystem = repositorySystem;
        this.log = log;
        this.mavenSession = mavenSession;

        this.remoteProjectRepositories = of(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemoteProjectRepositories)
                .map(this::forceDailyRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);

        this.remotePluginRepositories = of(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemotePluginRepositories)
                .map(this::forceDailyRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);
    }

    private List<RemoteRepository> forceDailyRemoteRepositoriesRefreshPolicy(
            List<RemoteRepository> remoteRepositories) {
        return remoteRepositories.stream()
                .map(remoteRepository -> {
                    RepositoryPolicy snapshotPolicy = forceDailyUpdatePolicy(remoteRepository.getPolicy(true));
                    RepositoryPolicy releasePolicy = forceDailyUpdatePolicy(remoteRepository.getPolicy(false));
                    if (snapshotPolicy != null || releasePolicy != null) {
                        RemoteRepository.Builder builder = new RemoteRepository.Builder(remoteRepository);
                        Optional.ofNullable(snapshotPolicy).ifPresent(builder::setSnapshotPolicy);
                        Optional.ofNullable(releasePolicy).ifPresent(builder::setReleasePolicy);
                        return builder.build();
                    } else {
                        return remoteRepository;
                    }
                })
                .collect(Collectors.toList());
    }

    private RepositoryPolicy forceDailyUpdatePolicy(RepositoryPolicy policy) {
        if (policy.isEnabled() && RepositoryPolicy.UPDATE_POLICY_NEVER.equals(policy.getUpdatePolicy())) {
            return new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, policy.getChecksumPolicy());
        }
        return null;
    }

    @Override
    public void resolveArtifact(Artifact artifact, boolean usePluginRepositories) throws ArtifactResolutionException {
        try {
            ArtifactResult artifactResult = repositorySystem.resolveArtifact(
                    mavenSession.getRepositorySession(),
                    new ArtifactRequest(
                            RepositoryUtils.toArtifact(artifact),
                            usePluginRepositories
                                    ? mavenSession.getCurrentProject().getRemotePluginRepositories()
                                    : mavenSession.getCurrentProject().getRemoteProjectRepositories(),
                            getClass().getName()));
            artifact.setFile(artifactResult.getArtifact().getFile());
            artifact.setVersion(artifactResult.getArtifact().getVersion());
            artifact.setResolved(artifactResult.isResolved());
        } catch (org.eclipse.aether.resolution.ArtifactResolutionException e) {
            throw new ArtifactResolutionException(e.getMessage(), artifact, e);
        }
    }

    @Override
    public ArtifactVersions resolveArtifactVersions(
            Artifact artifact, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException {
        try {
            VersionRangeRequest versionRangeRequest = new VersionRangeRequest(
                    toArtifact(artifact).setVersion("(,)"),
                    Stream.concat(
                                    usePluginRepositories ? remotePluginRepositories.stream() : Stream.empty(),
                                    useProjectRepositories ? remoteProjectRepositories.stream() : Stream.empty())
                            .distinct()
                            .collect(Collectors.toList()),
                    "lookupArtifactVersions");

            return new ArtifactVersions(
                    artifact,
                    repositorySystem
                            .resolveVersionRange(mavenSession.getRepositorySession(), versionRangeRequest)
                            .getVersions()
                            .stream()
                            .map(v -> ArtifactVersionService.getArtifactVersion(v.toString()))
                            .collect(Collectors.toList()));
        } catch (VersionRangeResolutionException e) {
            throw new VersionRetrievalException(e.getMessage(), artifact, e);
        } catch (RuntimeException e) {
            // log the dependency should any runtime exception occur (e.x. broken comparison contract)
            throw new VersionRetrievalException("Unable to retrieve versions for " + artifact, artifact, e);
        }
    }

    @Override
    public SortedMap<Dependency, ArtifactVersions> resolveDependencyVersions(
            Collection<Dependency> dependencies, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException {
        @SuppressWarnings("resource")
        ExecutorService executor = Executors.newFixedThreadPool(LOOKUP_PARALLEL_THREADS);
        try {
            Collection<Pair<Dependency, Future<ArtifactVersions>>> futures = dependencies.stream()
                    .map(d -> Pair.of(
                            d,
                            executor.submit(() -> resolveArtifactVersions(
                                    artifactFactory.createArtifact(d), usePluginRepositories, useProjectRepositories))))
                    .collect(Collectors.toList());
            SortedMap<Dependency, ArtifactVersions> result = new TreeMap<>(DependencyComparator.INSTANCE);
            for (Pair<Dependency, Future<ArtifactVersions>> entry : futures) {
                try {
                    result.put(entry.getKey(), entry.getValue().get());
                } catch (InterruptedException | ExecutionException e) {
                    if (e.getCause() instanceof VersionRetrievalException) {
                        throw (VersionRetrievalException) e.getCause();
                    }
                    throw new VersionRetrievalException(
                            "Unable to resolve metadata for dependencies " + dependencies + ": " + e.getMessage(),
                            null,
                            e);
                }
            }
            return result;
        } finally {
            executor.shutdown();
        }
    }

    @Override
    public PluginUpdatesDetails resolvePluginVersions(Plugin plugin) throws VersionRetrievalException {
        String version = plugin.getVersion() != null ? plugin.getVersion() : "LATEST";
        Set<Dependency> pluginDependencies = Optional.ofNullable(plugin.getDependencies())
                .map(d -> d.stream().collect(Collectors.toCollection(() ->
                        (Set<Dependency>) new TreeSet<>(DependencyComparator.INSTANCE))))
                .orElse(Collections.emptySet());
        SortedMap<Dependency, ArtifactVersions> pluginDependencyDetails =
                resolveDependencyVersions(pluginDependencies, false, true);
        Artifact pluginArtifact =
                artifactFactory.createMavenPluginArtifact(plugin.getGroupId(), plugin.getArtifactId(), version);
        ArtifactVersions allVersions = resolveArtifactVersions(pluginArtifact, true, false);
        return new PluginUpdatesDetails(allVersions, pluginDependencyDetails, true);
    }

    @Override
    public SortedMap<Plugin, PluginUpdatesDetails> resolvePluginVersions(Collection<Plugin> plugins)
            throws VersionRetrievalException {
        @SuppressWarnings("resource")
        ExecutorService executor = Executors.newFixedThreadPool(LOOKUP_PARALLEL_THREADS);
        try {
            SortedMap<Plugin, PluginUpdatesDetails> pluginUpdates = new TreeMap<>(PluginComparator.INSTANCE);
            List<Future<? extends Pair<Plugin, PluginUpdatesDetails>>> futures = plugins.stream()
                    .map(p -> executor.submit(() -> new ImmutablePair<>(p, resolvePluginVersions(p))))
                    .collect(Collectors.toList());
            for (Future<? extends Pair<Plugin, PluginUpdatesDetails>> details : futures) {
                Pair<Plugin, PluginUpdatesDetails> pair = details.get();
                pluginUpdates.put(pair.getKey(), pair.getValue());
            }
            return pluginUpdates;
        } catch (ExecutionException | InterruptedException e) {
            if (e.getCause() instanceof VersionRetrievalException) {
                throw (VersionRetrievalException) e.getCause();
            }
            throw new VersionRetrievalException(
                    "Unable to acquire metadata for plugins " + plugins + ": " + e.getMessage(), null, e);
        } finally {
            executor.shutdown();
        }
    }
}
