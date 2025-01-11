package org.codehaus.mojo.versions.api;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.rule.RuleService;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;

import static java.util.Objects.requireNonNull;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static org.apache.maven.RepositoryUtils.toArtifact;
;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class DefaultVersionsHelper implements VersionsHelper {

    private static final int LOOKUP_PARALLEL_THREADS = 5;

    private final RuleService ruleService;

    private final ArtifactFactory artifactFactory;

    private final RepositorySystem repositorySystem;

    /**
     * The {@link Log} to send log messages to.
     *
     * @since 1.0-alpha-3
     */
    private final Log log;

    /**
     * The maven session.
     *
     * @since 1.0-beta-1
     */
    private final MavenSession mavenSession;

    private final List<RemoteRepository> remotePluginRepositories;

    private final List<RemoteRepository> remoteProjectRepositories;

    private final PomHelper pomHelper;

    /**
     * Private constructor used by the builder
     */
    private DefaultVersionsHelper(
            PomHelper pomHelper,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            MavenSession mavenSession,
            RuleService ruleService,
            Log log) {
        this.pomHelper = requireNonNull(pomHelper);
        this.artifactFactory = requireNonNull(artifactFactory);
        this.repositorySystem = requireNonNull(repositorySystem);
        this.mavenSession = requireNonNull(mavenSession);
        this.ruleService = requireNonNull(ruleService);
        this.log = requireNonNull(log);

        this.remoteProjectRepositories = of(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemoteProjectRepositories)
                .map(DefaultVersionsHelper::forceDailyRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);

        this.remotePluginRepositories = of(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemotePluginRepositories)
                .map(DefaultVersionsHelper::forceDailyRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);
    }

    static List<RemoteRepository> forceDailyRemoteRepositoriesRefreshPolicy(List<RemoteRepository> remoteRepositories) {
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

    private static RepositoryPolicy forceDailyUpdatePolicy(RepositoryPolicy policy) {
        if (policy.isEnabled() && RepositoryPolicy.UPDATE_POLICY_NEVER.equals(policy.getUpdatePolicy())) {
            return new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, policy.getChecksumPolicy());
        }
        return null;
    }

    @Override
    public ArtifactVersions lookupArtifactVersions(
            Artifact artifact, VersionRange versionRange, boolean usePluginRepositories)
            throws VersionRetrievalException {
        return lookupArtifactVersions(artifact, versionRange, usePluginRepositories, !usePluginRepositories);
    }

    @Override
    public ArtifactVersions lookupArtifactVersions(
            Artifact artifact, VersionRange versionRange, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException {
        try {
            Collection<IgnoreVersion> ignoredVersions = ruleService.getIgnoredVersions(artifact);
            if (!ignoredVersions.isEmpty() && log.isDebugEnabled()) {
                log.debug("Found ignored versions: " + ignoredVersions + " for artifact" + artifact);
            }

            VersionRangeRequest versionRangeRequest = new VersionRangeRequest(
                    toArtifact(artifact)
                            .setVersion(ofNullable(versionRange)
                                    .map(VersionRange::getRestrictions)
                                    .flatMap(list -> list.stream().findFirst().map(Restriction::toString))
                                    .orElse("(,)")),
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
                            .filter(v -> ignoredVersions.stream().noneMatch(i -> {
                                if (IgnoreVersionHelper.isVersionIgnored(v, i)) {
                                    if (log.isDebugEnabled()) {
                                        log.debug("Version " + v + " for artifact "
                                                + ArtifactUtils.versionlessKey(artifact)
                                                + " found on ignore list: "
                                                + i);
                                    }
                                    return true;
                                }

                                return false;
                            }))
                            .map(v -> ArtifactVersionService.getArtifactVersion(v.toString()))
                            .collect(Collectors.toList()));
        } catch (VersionRangeResolutionException e) {
            throw new VersionRetrievalException(e.getMessage(), e);
        }
    }

    @Override
    public ArtifactVersions lookupArtifactVersions(Artifact artifact, boolean usePluginRepositories)
            throws VersionRetrievalException {
        return lookupArtifactVersions(artifact, null, usePluginRepositories);
    }

    @Override
    public void resolveArtifact(Artifact artifact, boolean usePluginRepositories) throws ArtifactResolutionException {
        try {
            ArtifactResult artifactResult = repositorySystem.resolveArtifact(
                    mavenSession.getRepositorySession(),
                    new ArtifactRequest(
                            toArtifact(artifact),
                            usePluginRepositories
                                    ? mavenSession.getCurrentProject().getRemotePluginRepositories()
                                    : mavenSession.getCurrentProject().getRemoteProjectRepositories(),
                            getClass().getName()));
            artifact.setFile(artifactResult.getArtifact().getFile());
            artifact.setVersion(artifactResult.getArtifact().getVersion());
            artifact.setResolved(artifactResult.isResolved());
        } catch (org.eclipse.aether.resolution.ArtifactResolutionException e) {
            throw new ArtifactResolutionException(e.getMessage(), artifact);
        }
    }

    public Map<Dependency, ArtifactVersions> lookupDependenciesUpdates(
            Stream<Dependency> dependencies,
            boolean usePluginRepositories,
            boolean useProjectRepositories,
            boolean allowSnapshots)
            throws VersionRetrievalException {
        ExecutorService executor = Executors.newFixedThreadPool(LOOKUP_PARALLEL_THREADS);
        try {
            Map<Dependency, ArtifactVersions> dependencyUpdates = new TreeMap<>(DependencyComparator.INSTANCE);
            List<Future<? extends Pair<Dependency, ArtifactVersions>>> futures = dependencies
                    .map(dependency -> executor.submit(() -> new ImmutablePair<>(
                            dependency,
                            lookupDependencyUpdates(
                                    dependency, usePluginRepositories, useProjectRepositories, allowSnapshots))))
                    .collect(Collectors.toList());
            for (Future<? extends Pair<Dependency, ArtifactVersions>> details : futures) {
                Pair<Dependency, ArtifactVersions> pair = details.get();
                dependencyUpdates.put(pair.getKey(), pair.getValue());
            }

            return dependencyUpdates;
        } catch (ExecutionException | InterruptedException ie) {
            throw new VersionRetrievalException(
                    "Unable to acquire metadata for dependencies " + dependencies + ": " + ie.getMessage(), ie);
        } finally {
            executor.shutdown();
        }
    }

    @Override
    public Map<Dependency, ArtifactVersions> lookupDependenciesUpdates(
            Stream<Dependency> dependencies, boolean usePluginRepositories, boolean allowSnapshots)
            throws VersionRetrievalException {
        return lookupDependenciesUpdates(dependencies, usePluginRepositories, !usePluginRepositories, allowSnapshots);
    }

    @Override
    public ArtifactVersions lookupDependencyUpdates(
            Dependency dependency,
            boolean usePluginRepositories,
            boolean useProjectRepositories,
            boolean allowSnapshots)
            throws VersionRetrievalException {
        ArtifactVersions allVersions = lookupArtifactVersions(
                artifactFactory.createArtifact(dependency), null, usePluginRepositories, useProjectRepositories);
        return new ArtifactVersions(
                allVersions.getArtifact(),
                Arrays.stream(allVersions.getAllUpdates(allowSnapshots)).collect(Collectors.toList()));
    }

    @Override
    public Map<Plugin, PluginUpdatesDetails> lookupPluginsUpdates(Stream<Plugin> plugins, boolean allowSnapshots)
            throws VersionRetrievalException {
        ExecutorService executor = Executors.newFixedThreadPool(LOOKUP_PARALLEL_THREADS);
        try {
            Map<Plugin, PluginUpdatesDetails> pluginUpdates = new TreeMap<>(PluginComparator.INSTANCE);
            List<Future<? extends Pair<Plugin, PluginUpdatesDetails>>> futures = plugins.map(
                            p -> executor.submit(() -> new ImmutablePair<>(p, lookupPluginUpdates(p, allowSnapshots))))
                    .collect(Collectors.toList());
            for (Future<? extends Pair<Plugin, PluginUpdatesDetails>> details : futures) {
                Pair<Plugin, PluginUpdatesDetails> pair = details.get();
                pluginUpdates.put(pair.getKey(), pair.getValue());
            }

            return pluginUpdates;
        } catch (ExecutionException | InterruptedException ie) {
            throw new VersionRetrievalException(
                    "Unable to acquire metadata for plugins " + plugins + ": " + ie.getMessage(), ie);
        } finally {
            executor.shutdown();
        }
    }

    @Override
    public PluginUpdatesDetails lookupPluginUpdates(Plugin plugin, boolean allowSnapshots)
            throws VersionRetrievalException {
        String version = plugin.getVersion() != null ? plugin.getVersion() : "LATEST";

        Set<Dependency> pluginDependencies = new TreeSet<>(DependencyComparator.INSTANCE);
        if (plugin.getDependencies() != null) {
            pluginDependencies.addAll(plugin.getDependencies());
        }
        Map<Dependency, ArtifactVersions> pluginDependencyDetails =
                lookupDependenciesUpdates(pluginDependencies.stream(), false, allowSnapshots);

        ArtifactVersions allVersions = lookupArtifactVersions(
                artifactFactory.createMavenPluginArtifact(plugin.getGroupId(), plugin.getArtifactId(), version), true);
        ArtifactVersions updatedVersions = new ArtifactVersions(
                allVersions.getArtifact(),
                Arrays.stream(allVersions.getAllUpdates(allowSnapshots)).collect(Collectors.toList()));
        return new PluginUpdatesDetails(updatedVersions, pluginDependencyDetails, allowSnapshots);
    }

    @Override
    public Map<Property, PropertyVersions> getVersionPropertiesMap(VersionPropertiesMapRequest request)
            throws MojoExecutionException {
        Map<String, Property> properties = new HashMap<>();
        // Populate properties map from request
        if (request.getPropertyDefinitions() != null) {
            Arrays.stream(request.getPropertyDefinitions()).forEach(p -> properties.put(p.getName(), p));
        }

        Map<String, PropertyVersionsBuilder> builders = new HashMap<>();

        // Auto-link items if required
        if (request.isAutoLinkItems()) {
            try {
                PropertyVersionsBuilder[] propertyVersionsBuilders = pomHelper.getPropertyVersionsBuilders(
                        this, log, request.getMavenProject(), request.isIncludeParent());

                for (PropertyVersionsBuilder builder : propertyVersionsBuilders) {
                    String propertyName = builder.getName();
                    builders.put(propertyName, builder);

                    properties.computeIfAbsent(propertyName, name -> {
                        Property property = new Property(name);
                        log.debug(String.format(
                                "Property ${%s}: Adding inferred version range of %s",
                                name, builder.getVersionRange()));
                        property.setVersion(builder.getVersionRange());
                        return property;
                    });
                }
            } catch (ExpressionEvaluationException | IOException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            }
        }

        // Create include and exclude properties lists
        List<String> includePropertiesList = Optional.ofNullable(request.getIncludeProperties())
                .map(s -> Arrays.asList(s.split("\\s*,\\s*")))
                .orElse(Collections.emptyList());

        List<String> excludePropertiesList = Optional.ofNullable(request.getExcludeProperties())
                .map(s -> Arrays.asList(s.split("\\s*,\\s*")))
                .orElse(Collections.emptyList());

        // Filter properties based on include/exclude lists
        properties.values().removeIf(property -> {
            String name = property.getName();
            if (!includePropertiesList.isEmpty() && !includePropertiesList.contains(name)) {
                log.debug("Skipping property ${" + name + "}");
                return true;
            }
            if (excludePropertiesList.contains(name)) {
                log.debug("Ignoring property ${" + name + "}");
                return true;
            }
            return false;
        });

        log.debug("Processing properties to build PropertyVersions");
        Map<Property, PropertyVersions> propertyVersions = new LinkedHashMap<>(properties.size());

        for (Property property : properties.values()) {
            String propertyName = property.getName();
            log.debug("Property ${" + propertyName + "}");

            PropertyVersionsBuilder builder = builders.get(propertyName);
            if (builder == null || !builder.isAssociated()) {
                log.debug(String.format("Property ${%s}: Not associated with any dependency.", propertyName));
                builder = new PropertyVersionsBuilder(this, null, propertyName, log);
            }

            if (!property.isAutoLinkDependencies()) {
                log.debug(String.format("Property ${%s}: Clearing auto-link dependencies", propertyName));
                builder.clearAssociations();
            }

            // Add specified dependencies to the builder
            Dependency[] dependencies = property.getDependencies();
            if (dependencies != null) {
                for (Dependency dependency : dependencies) {
                    log.debug(String.format("Property ${%s}: Adding association to %s", propertyName, dependency));
                    builder.withAssociation(artifactFactory.createArtifact(dependency), false);
                }
            }

            try {
                String currentVersion =
                        request.getMavenProject().getProperties().getProperty(propertyName);
                property.setValue(currentVersion);

                if (property.isAutoLinkDependencies()
                        && StringUtils.isEmpty(property.getVersion())
                        && !StringUtils.isEmpty(builder.getVersionRange())) {
                    log.debug(String.format(
                            "Property ${%s}: Adding inferred version range of %s",
                            propertyName, builder.getVersionRange()));
                    property.setVersion(builder.getVersionRange());
                }

                if (currentVersion != null) {
                    builder.withCurrentVersion(ArtifactVersionService.getArtifactVersion(currentVersion))
                            .withCurrentVersionRange(VersionRange.createFromVersionSpec(currentVersion));
                }

                PropertyVersions versions = builder.build();
                propertyVersions.put(property, versions);

            } catch (VersionRetrievalException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            } catch (InvalidVersionSpecificationException e) {
                throw new RuntimeException(e);
            }
        }

        return propertyVersions;
    }

    /**
     * Builder class for {@linkplain DefaultVersionsHelper}
     */
    public static class Builder {
        private Log log;

        private MavenSession mavenSession;

        private RepositorySystem repositorySystem;

        private RuleService ruleService;

        private ArtifactFactory artifactFactory;

        private PomHelper pomHelper;

        public Builder() {}

        public Builder withLog(Log log) {
            this.log = log;
            return this;
        }

        public Builder withMavenSession(MavenSession mavenSession) {
            this.mavenSession = mavenSession;
            return this;
        }

        public Builder withRepositorySystem(RepositorySystem repositorySystem) {
            this.repositorySystem = repositorySystem;
            return this;
        }

        public Builder withRuleService(RuleService ruleService) {
            this.ruleService = ruleService;
            return this;
        }

        public Builder withArtifactCreationService(ArtifactFactory artifactFactory) {
            this.artifactFactory = artifactFactory;
            return this;
        }

        public Builder withPomHelper(PomHelper pomHelper) {
            this.pomHelper = pomHelper;
            return this;
        }

        /**
         * Builds the constructed {@linkplain DefaultVersionsHelper} object
         *
         * @return constructed {@linkplain DefaultVersionsHelper}
         * @throws MojoExecutionException should the constructor with the RuleSet retrieval doesn't succeed
         */
        public DefaultVersionsHelper build() throws MojoExecutionException {
            return new DefaultVersionsHelper(
                    pomHelper, artifactFactory, repositorySystem, mavenSession, ruleService, log);
        }
    }
}
