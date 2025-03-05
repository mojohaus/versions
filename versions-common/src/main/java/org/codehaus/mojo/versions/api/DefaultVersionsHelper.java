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

import javax.xml.stream.XMLStreamException;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Pattern;
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
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.observers.Debug;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.model.io.stax.RuleStaxReader;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.mojo.versions.utils.RegexUtils;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.repository.AuthenticationContext;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Objects.requireNonNull;
import static java.util.Optional.empty;
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
    private static final String CLASSPATH_PROTOCOL = "classpath";

    private static final int LOOKUP_PARALLEL_THREADS = 5;

    // for testing purpose
    RuleSet getRuleSet() {
        return ruleSet;
    }

    /**
     * The artifact comparison rules to use.
     *
     * @since 1.0-alpha-3
     */
    private RuleSet ruleSet;

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

    private final MojoExecution mojoExecution;

    /**
     * A cache mapping artifacts to their best fitting rule, since looking up
     * this information can be quite costly.
     *
     * @since 2.12
     */
    private final Map<String, Rule> artifactBestFitRule = new ConcurrentHashMap<>();

    private final List<RemoteRepository> remoteProjectRepositories;

    private final List<RemoteRepository> remotePluginRepositories;

    private final List<RemoteRepository> remoteRepositories;

    private final PomHelper pomHelper;

    /**
     * Private constructor used by the builder
     */
    private DefaultVersionsHelper(
            PomHelper pomHelper,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            MavenSession mavenSession,
            MojoExecution mojoExecution,
            Log log) {
        this.pomHelper = requireNonNull(pomHelper);
        this.artifactFactory = requireNonNull(artifactFactory);
        this.repositorySystem = repositorySystem;
        this.mavenSession = mavenSession;
        this.mojoExecution = mojoExecution;
        this.log = requireNonNull(log);

        this.remoteProjectRepositories = Optional.ofNullable(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemoteProjectRepositories)
                .map(DefaultVersionsHelper::adjustRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);

        this.remotePluginRepositories = Optional.ofNullable(mavenSession)
                .map(MavenSession::getCurrentProject)
                .map(MavenProject::getRemotePluginRepositories)
                .map(DefaultVersionsHelper::adjustRemoteRepositoriesRefreshPolicy)
                .orElseGet(Collections::emptyList);

        this.remoteRepositories = Stream.concat(remoteProjectRepositories.stream(), remotePluginRepositories.stream())
                .distinct()
                .collect(Collectors.toList());
    }

    static List<RemoteRepository> adjustRemoteRepositoriesRefreshPolicy(List<RemoteRepository> remoteRepositories) {
        return remoteRepositories.stream()
                .map(DefaultVersionsHelper::adjustRemoteRepositoryRefreshPolicy)
                .collect(Collectors.toList());
    }

    static RemoteRepository adjustRemoteRepositoryRefreshPolicy(RemoteRepository remoteRepository) {
        RepositoryPolicy snapshotPolicy = remoteRepository.getPolicy(true);
        RepositoryPolicy releasePolicy = remoteRepository.getPolicy(false);

        RepositoryPolicy newSnapshotPolicy = null;
        RepositoryPolicy newReleasePolicy = null;

        if (snapshotPolicy.isEnabled()
                && RepositoryPolicy.UPDATE_POLICY_NEVER.equals(snapshotPolicy.getUpdatePolicy())) {
            newSnapshotPolicy = new RepositoryPolicy(
                    true, RepositoryPolicy.UPDATE_POLICY_DAILY, snapshotPolicy.getChecksumPolicy());
        }

        if (releasePolicy.isEnabled() && RepositoryPolicy.UPDATE_POLICY_NEVER.equals(releasePolicy.getUpdatePolicy())) {
            newReleasePolicy =
                    new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, releasePolicy.getChecksumPolicy());
        }

        if (newSnapshotPolicy != null || newReleasePolicy != null) {
            RemoteRepository.Builder builder = new RemoteRepository.Builder(remoteRepository);
            if (newSnapshotPolicy != null) {
                builder.setSnapshotPolicy(newSnapshotPolicy);
            }
            if (newReleasePolicy != null) {
                builder.setReleasePolicy(newReleasePolicy);
            }
            return builder.build();
        } else {
            return remoteRepository;
        }
    }

    static boolean exactMatch(String wildcardRule, String value) {
        Pattern p = Pattern.compile(RegexUtils.convertWildcardsToRegex(wildcardRule, true));
        return p.matcher(value).matches();
    }

    static boolean match(String wildcardRule, String value) {
        Pattern p = Pattern.compile(RegexUtils.convertWildcardsToRegex(wildcardRule, false));
        return p.matcher(value).matches();
    }

    static boolean isClasspathUri(String uri) {
        return (uri != null && uri.startsWith(CLASSPATH_PROTOCOL + ":"));
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
            Collection<IgnoreVersion> ignoredVersions = getIgnoredVersions(artifact);
            if (!ignoredVersions.isEmpty() && log.isDebugEnabled()) {
                log.debug("Found ignored versions: " + ignoredVersions + " for artifact" + artifact);
            }

            final List<RemoteRepository> repositories;
            if (usePluginRepositories && !useProjectRepositories) {
                repositories = remotePluginRepositories;
            } else if (!usePluginRepositories && useProjectRepositories) {
                repositories = remoteProjectRepositories;
            } else if (usePluginRepositories) {
                repositories = remoteRepositories;
            } else {
                // testing?
                repositories = emptyList();
            }

            return new ArtifactVersions(
                    artifact,
                    repositorySystem
                            .resolveVersionRange(
                                    mavenSession.getRepositorySession(),
                                    new VersionRangeRequest(
                                            toArtifact(artifact)
                                                    .setVersion(ofNullable(versionRange)
                                                            .map(VersionRange::getRestrictions)
                                                            .flatMap(list -> list.stream()
                                                                    .findFirst()
                                                                    .map(Restriction::toString))
                                                            .orElse("(,)")),
                                            repositories,
                                            "lookupArtifactVersions"))
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
                            .collect(Collectors.toList()),
                    getVersionComparator(artifact));
        } catch (VersionRangeResolutionException e) {
            throw new VersionRetrievalException(e.getMessage(), e);
        }
    }

    @Override
    public ArtifactVersions lookupArtifactVersions(Artifact artifact, boolean usePluginRepositories)
            throws VersionRetrievalException {
        return lookupArtifactVersions(artifact, null, usePluginRepositories);
    }

    /**
     * Returns a list of versions which should not be considered when looking for updates.
     *
     * @param artifact The artifact
     * @return List of ignored version
     */
    private List<IgnoreVersion> getIgnoredVersions(Artifact artifact) {
        final List<IgnoreVersion> ret = new ArrayList<>();

        for (final IgnoreVersion ignoreVersion : ruleSet.getIgnoreVersions()) {
            if (IgnoreVersionHelper.isValidType(ignoreVersion)) {
                ret.add(ignoreVersion);
            } else {
                log.warn("The type attribute '" + ignoreVersion.getType() + "' for global ignoreVersion["
                        + ignoreVersion + "] is not valid. Please use one of '" + IgnoreVersionHelper.VALID_TYPES
                        + "'.");
            }
        }

        final Rule rule = getBestFitRule(artifact.getGroupId(), artifact.getArtifactId());

        if (rule != null) {
            for (IgnoreVersion ignoreVersion : rule.getIgnoreVersions()) {
                if (IgnoreVersionHelper.isValidType(ignoreVersion)) {
                    ret.add(ignoreVersion);
                } else {
                    log.warn("The type attribute '" + ignoreVersion.getType() + "' for " + rule + " is not valid."
                            + " Please use one of '" + IgnoreVersionHelper.VALID_TYPES + "'.");
                }
            }
        }

        return ret;
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

    @Override
    public VersionComparator getVersionComparator(Artifact artifact) {
        return getVersionComparator(artifact.getGroupId(), artifact.getArtifactId());
    }

    @Override
    public VersionComparator getVersionComparator(String groupId, String artifactId) {
        Rule rule = getBestFitRule(groupId, artifactId);
        final String comparisonMethod = rule == null ? ruleSet.getComparisonMethod() : rule.getComparisonMethod();
        return VersionComparators.getVersionComparator(comparisonMethod);
    }

    /**
     * Find the rule, if any, which best fits the artifact details given.
     *
     * @param groupId    Group id of the artifact
     * @param artifactId Artifact id of the artifact
     * @return Rule which best describes the given artifact
     */
    protected Rule getBestFitRule(String groupId, String artifactId) {
        String groupArtifactId = groupId + ':' + artifactId;
        if (artifactBestFitRule.containsKey(groupArtifactId)) {
            return artifactBestFitRule.get(groupArtifactId);
        }

        Rule bestFit = null;
        final List<Rule> rules = ruleSet.getRules();
        int bestGroupIdScore = Integer.MAX_VALUE;
        int bestArtifactIdScore = Integer.MAX_VALUE;
        boolean exactGroupId = false;
        boolean exactArtifactId = false;
        for (Rule rule : rules) {
            int groupIdScore = RegexUtils.getWildcardScore(rule.getGroupId());
            if (groupIdScore > bestGroupIdScore) {
                continue;
            }
            boolean exactMatch = exactMatch(rule.getGroupId(), groupId);
            boolean match = exactMatch || match(rule.getGroupId(), groupId);
            if (!match || (exactGroupId && !exactMatch)) {
                continue;
            }
            if (bestGroupIdScore > groupIdScore) {
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            bestGroupIdScore = groupIdScore;
            if (exactMatch && !exactGroupId) {
                exactGroupId = true;
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            int artifactIdScore = RegexUtils.getWildcardScore(rule.getArtifactId());
            if (artifactIdScore > bestArtifactIdScore) {
                continue;
            }
            exactMatch = exactMatch(rule.getArtifactId(), artifactId);
            match = exactMatch || match(rule.getArtifactId(), artifactId);
            if (!match || (exactArtifactId && !exactMatch)) {
                continue;
            }
            bestArtifactIdScore = artifactIdScore;
            if (exactMatch && !exactArtifactId) {
                exactArtifactId = true;
            }
            bestFit = rule;
        }

        if (bestFit != null) {
            artifactBestFitRule.put(groupArtifactId, bestFit);
        }
        return bestFit;
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
                Arrays.stream(allVersions.getAllUpdates(allowSnapshots)).collect(Collectors.toList()),
                allVersions.getVersionComparator());
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
                Arrays.stream(allVersions.getAllUpdates(allowSnapshots)).collect(Collectors.toList()),
                allVersions.getVersionComparator());
        return new PluginUpdatesDetails(updatedVersions, pluginDependencyDetails, allowSnapshots);
    }

    @Override
    public ExpressionEvaluator getExpressionEvaluator(MavenProject project) {
        return new VersionsExpressionEvaluator(mavenSession, mojoExecution);
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

        List<String> includePropertiesList = request.getIncludeProperties() != null
                ? Arrays.asList(request.getIncludeProperties().split("\\s*,\\s*"))
                : Collections.emptyList();
        List<String> excludePropertiesList = request.getExcludeProperties() != null
                ? Arrays.asList(request.getExcludeProperties().split("\\s*,\\s*"))
                : Collections.emptyList();

        log.debug("Searching for properties associated with builders");
        Iterator<Property> i = properties.values().iterator();
        while (i.hasNext()) {
            Property property = i.next();

            log.debug("includePropertiesList:" + includePropertiesList + " property: " + property.getName());
            log.debug("excludePropertiesList:" + excludePropertiesList + " property: " + property.getName());
            if (!includePropertiesList.isEmpty() && !includePropertiesList.contains(property.getName())) {
                log.debug("Skipping property ${" + property.getName() + "}");
                i.remove();
            } else if (!excludePropertiesList.isEmpty() && excludePropertiesList.contains(property.getName())) {
                log.debug("Ignoring property ${" + property.getName() + "}");
                i.remove();
            }
        }
        i = properties.values().iterator();
        Map<Property, PropertyVersions> propertyVersions = new LinkedHashMap<>(properties.size());
        while (i.hasNext()) {
            Property property = i.next();
            log.debug("Property ${" + property.getName() + "}");
            PropertyVersionsBuilder builder = builders.get(property.getName());
            if (builder == null || !builder.isAssociated()) {
                log.debug("Property ${" + property.getName() + "}: Looks like this property is not "
                        + "associated with any dependency...");
                builder = new PropertyVersionsBuilder(this, null, property.getName(), log);
            }
            if (!property.isAutoLinkDependencies()) {
                log.debug("Property ${" + property.getName() + "}: Removing any autoLinkDependencies");
                builder.clearAssociations();
            }
            Dependency[] dependencies = property.getDependencies();
            if (dependencies != null) {
                for (Dependency dependency : dependencies) {
                    log.debug("Property ${" + property.getName() + "}: Adding association to " + dependency);
                    builder.withAssociation(artifactFactory.createArtifact(dependency), false);
                }
            }
            try {
                if (property.isAutoLinkDependencies()
                        && StringUtils.isEmpty(property.getVersion())
                        && !StringUtils.isEmpty(builder.getVersionRange())) {
                    log.debug("Property ${" + property.getName() + "}: Adding inferred version range of "
                            + builder.getVersionRange());
                    property.setVersion(builder.getVersionRange());
                }
                final String currentVersion =
                        request.getMavenProject().getProperties().getProperty(property.getName());
                property.setValue(currentVersion);
                final PropertyVersions versions;
                try {
                    if (currentVersion != null) {
                        builder.withCurrentVersion(ArtifactVersionService.getArtifactVersion(currentVersion))
                                .withCurrentVersionRange(VersionRange.createFromVersionSpec(currentVersion));
                    }
                } catch (InvalidVersionSpecificationException e) {
                    throw new RuntimeException(e);
                }
                versions = builder.build();
                propertyVersions.put(property, versions);
            } catch (VersionRetrievalException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            }
        }
        return propertyVersions;
    }

    /**
     * Builder class for {@linkplain DefaultVersionsHelper}
     */
    public static class Builder {
        private Collection<String> ignoredVersions;

        private RuleSet ruleSet;

        private String serverId;

        private String rulesUri;

        private Log log;

        private MavenSession mavenSession;

        private MojoExecution mojoExecution;

        private RepositorySystem repositorySystem;

        private ArtifactFactory artifactFactory;

        private PomHelper pomHelper;

        private Map<String, Wagon> wagonMap;

        public Builder() {}

        private static RuleSet getRulesFromClasspath(String uri, Log logger) throws MojoExecutionException {
            logger.debug("Going to load rules from \"" + uri + "\"");
            String choppedUrl = uri.substring(CLASSPATH_PROTOCOL.length() + 3);
            URL url = DefaultVersionsHelper.class.getResource(choppedUrl);
            if (url == null) {
                throw new MojoExecutionException("Resource \"" + uri + "\" not found in classpath.");
            }

            try (BufferedInputStream bis = new BufferedInputStream(url.openStream())) {
                RuleSet result = new RuleStaxReader().read(bis);
                logger.debug("Loaded rules from \"" + uri + "\" successfully");
                return result;
            } catch (IOException | XMLStreamException e) {
                throw new MojoExecutionException("Could not load specified rules from " + uri, e);
            }
        }

        /**
         * <p>Creates the enriched version of the ruleSet given as parameter; the ruleSet will contain the
         * set of ignored versions passed on top of its own (if defined).</p>
         *
         * <p>If the {@code originalRuleSet} is {@code null}, a new {@linkplain RuleSet} will be created as
         * a result.</p>
         *
         * <p><em>The method does not change the {@code originalRuleSet} object.</em></p>
         *
         * @param ignoredVersions collection of ignored version to enrich the clone of the original rule set
         * @param originalRuleSet original rule set
         * @return new RuleSet object containing the (if passed) cloned version of the rule set, enriched with
         *         the given set of ignored versions
         */
        @SuppressWarnings("checkstyle:AvoidNestedBlocks")
        private static RuleSet enrichRuleSet(Collection<String> ignoredVersions, RuleSet originalRuleSet) {
            RuleSet ruleSet = new RuleSet();
            if (originalRuleSet != null) {
                ruleSet.setComparisonMethod(originalRuleSet.getComparisonMethod());
                if (originalRuleSet.getRules() != null) {
                    ruleSet.setRules(new ArrayList<>(originalRuleSet.getRules()));
                }
                if (originalRuleSet.getIgnoreVersions() != null) {
                    ruleSet.setIgnoreVersions(new ArrayList<>(originalRuleSet.getIgnoreVersions()));
                }
            }

            if (ruleSet.getIgnoreVersions() == null) {
                ruleSet.setIgnoreVersions(new ArrayList<>());
            }
            ruleSet.getIgnoreVersions()
                    .addAll(ignoredVersions.stream()
                            .map(v -> {
                                IgnoreVersion ignoreVersion = new IgnoreVersion();
                                ignoreVersion.setType(IgnoreVersion.TYPE_REGEX);
                                ignoreVersion.setVersion(v);
                                return ignoreVersion;
                            })
                            .collect(Collectors.toList()));

            return ruleSet;
        }

        private static class RulesUri {
            String basePath;
            String resource;

            private RulesUri(String basePath, String resource) {
                this.basePath = basePath;
                this.resource = resource;
            }

            static RulesUri build(String rulesUri) throws URISyntaxException {
                int split = rulesUri.lastIndexOf('/');
                return split == -1
                        ? new RulesUri(rulesUri, "")
                        : new RulesUri(
                                rulesUri.substring(0, split) + '/',
                                split + 1 < rulesUri.length() ? rulesUri.substring(split + 1) : "");
            }
        }

        private RemoteRepository remoteRepository(RulesUri uri) {
            RemoteRepository prototype = new RemoteRepository.Builder(serverId, null, uri.basePath).build();
            RemoteRepository.Builder builder = new RemoteRepository.Builder(prototype);
            ofNullable(mavenSession.getRepositorySession().getProxySelector().getProxy(prototype))
                    .ifPresent(builder::setProxy);
            ofNullable(mavenSession
                            .getRepositorySession()
                            .getAuthenticationSelector()
                            .getAuthentication(prototype))
                    .ifPresent(builder::setAuthentication);
            ofNullable(mavenSession.getRepositorySession().getMirrorSelector().getMirror(prototype))
                    .ifPresent(mirror -> builder.setMirroredRepositories(singletonList(mirror)));
            return builder.build();
        }

        private Optional<ProxyInfo> getProxyInfo(RemoteRepository repository) {
            return ofNullable(repository.getProxy()).map(proxy -> new ProxyInfo() {
                {
                    setHost(proxy.getHost());
                    setPort(proxy.getPort());
                    setType(proxy.getType());
                    ofNullable(proxy.getAuthentication()).ifPresent(auth -> {
                        try (AuthenticationContext authCtx =
                                AuthenticationContext.forProxy(mavenSession.getRepositorySession(), repository)) {
                            ofNullable(authCtx.get(AuthenticationContext.USERNAME))
                                    .ifPresent(this::setUserName);
                            ofNullable(authCtx.get(AuthenticationContext.PASSWORD))
                                    .ifPresent(this::setPassword);
                            ofNullable(authCtx.get(AuthenticationContext.NTLM_DOMAIN))
                                    .ifPresent(this::setNtlmDomain);
                            ofNullable(authCtx.get(AuthenticationContext.NTLM_WORKSTATION))
                                    .ifPresent(this::setNtlmHost);
                        }
                    });
                }
            });
        }

        private Optional<AuthenticationInfo> getAuthenticationInfo(RemoteRepository repository) {
            return ofNullable(repository.getAuthentication()).map(authentication -> new AuthenticationInfo() {
                {
                    try (AuthenticationContext authCtx =
                            AuthenticationContext.forRepository(mavenSession.getRepositorySession(), repository)) {
                        ofNullable(authCtx.get(AuthenticationContext.USERNAME)).ifPresent(this::setUserName);
                        ofNullable(authCtx.get(AuthenticationContext.PASSWORD)).ifPresent(this::setPassword);
                        ofNullable(authCtx.get(AuthenticationContext.PRIVATE_KEY_PASSPHRASE))
                                .ifPresent(this::setPassphrase);
                        ofNullable(authCtx.get(AuthenticationContext.PRIVATE_KEY_PATH))
                                .ifPresent(this::setPrivateKey);
                    }
                }
            });
        }

        private org.apache.maven.wagon.repository.Repository wagonRepository(RemoteRepository repository) {
            return new org.apache.maven.wagon.repository.Repository(repository.getId(), repository.getUrl());
        }

        private RuleSet getRulesUsingWagon() throws MojoExecutionException {
            RulesUri uri;
            try {
                uri = RulesUri.build(rulesUri);
            } catch (URISyntaxException e) {
                log.warn("Invalid rulesUri protocol: " + e.getMessage());
                return null;
            }

            RemoteRepository repository = remoteRepository(uri);
            return ofNullable(wagonMap.get(repository.getProtocol()))
                    .map(wagon -> {
                        if (log.isDebugEnabled()) {
                            Debug debug = new Debug();
                            wagon.addSessionListener(debug);
                            wagon.addTransferListener(debug);
                        }

                        try {
                            Optional<ProxyInfo> proxyInfo = getProxyInfo(repository);
                            Optional<AuthenticationInfo> authenticationInfo = getAuthenticationInfo(repository);
                            if (log.isDebugEnabled()) {
                                log.debug("Connecting to remote repository \"" + repository.getId() + "\""
                                        + proxyInfo
                                                .map(pi -> " using proxy " + pi.getHost() + ":" + pi.getPort())
                                                .orElse("")
                                        + authenticationInfo
                                                .map(ai -> " as " + ai.getUserName())
                                                .orElse(""));
                            }
                            wagon.connect(
                                    wagonRepository(repository),
                                    getAuthenticationInfo(repository).orElse(null),
                                    getProxyInfo(repository).orElse(null));
                            try {
                                Path tempFile = Files.createTempFile("rules-", ".xml");
                                wagon.get(uri.resource, tempFile.toFile());
                                try (InputStream is = Files.newInputStream(tempFile)) {
                                    return new RuleStaxReader().read(is);
                                } finally {
                                    Files.deleteIfExists(tempFile);
                                }

                            } finally {
                                wagon.disconnect();
                            }
                        } catch (Exception e) {
                            throw new RuntimeException(e);
                        }
                    })
                    .orElseThrow(() -> new MojoExecutionException("Could not load specified rules from " + rulesUri));
        }

        public static Optional<String> protocol(final String url) {
            int pos = url.indexOf(":");
            return pos == -1 ? empty() : of(url.substring(0, pos).trim());
        }

        public Builder withIgnoredVersions(Collection<String> ignoredVersions) {
            this.ignoredVersions = ignoredVersions;
            return this;
        }

        public Builder withRuleSet(RuleSet ruleSet) {
            this.ruleSet = ruleSet;
            return this;
        }

        public Builder withServerId(String serverId) {
            this.serverId = serverId;
            return this;
        }

        public Builder withRulesUri(String rulesUri) {
            this.rulesUri = rulesUri;
            return this;
        }

        public Builder withLog(Log log) {
            this.log = log;
            return this;
        }

        public Builder withMavenSession(MavenSession mavenSession) {
            this.mavenSession = mavenSession;
            return this;
        }

        public Builder withMojoExecution(MojoExecution mojoExecution) {
            this.mojoExecution = mojoExecution;
            return this;
        }

        public Builder withRepositorySystem(RepositorySystem repositorySystem) {
            this.repositorySystem = repositorySystem;
            return this;
        }

        public Builder withWagonMap(Map<String, Wagon> wagonMap) {
            this.wagonMap = wagonMap;
            return this;
        }

        public Builder withArtifactFactory(ArtifactFactory artifactFactory) {
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
            DefaultVersionsHelper instance = new DefaultVersionsHelper(
                    pomHelper, artifactFactory, repositorySystem, mavenSession, mojoExecution, log);
            if (ruleSet != null) {
                if (!isBlank(rulesUri)) {
                    log.warn("rulesUri is ignored if rules are specified in pom or as parameters");
                }
                instance.ruleSet = ruleSet;
            } else {
                instance.ruleSet = isBlank(rulesUri)
                        ? new RuleSet()
                        : isClasspathUri(rulesUri) ? getRulesFromClasspath(rulesUri, log) : getRulesUsingWagon();
            }
            if (ignoredVersions != null && !ignoredVersions.isEmpty()) {
                instance.ruleSet = enrichRuleSet(ignoredVersions, instance.ruleSet);
            }
            return instance;
        }

        private static boolean isBlank(String s) {
            return s == null || s.trim().isEmpty();
        }
    }
}
