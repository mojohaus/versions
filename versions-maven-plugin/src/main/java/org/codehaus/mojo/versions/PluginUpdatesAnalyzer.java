package org.codehaus.mojo.versions;

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

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingResult;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

/**
 * Analyzes plugins in one project and repository context. Create a fresh instance for each
 * project/execution: prerequisite caches must never leak across repository configurations.
 */
final class PluginUpdatesAnalyzer {
    private final ArtifactFactory artifactFactory;
    private final VersionsHelper helper;
    private final ProjectBuilder projectBuilder;
    private final MavenSession session;
    private final MavenProject project;
    private final Log log;
    private final ArtifactVersion runningMaven;
    private final ArtifactVersion minimumMaven;
    private final boolean allowSnapshots;
    // Optional.empty means a resolved POM without prerequisites; missing POMs are cached separately.
    private final Map<String, Optional<ArtifactVersion>> prerequisites = new HashMap<>();
    private final Map<String, Exception> failures = new HashMap<>();
    private final Map<String, ArtifactVersions> versions = new HashMap<>();

    @SuppressWarnings("checkstyle:ParameterNumber")
    PluginUpdatesAnalyzer(
            ArtifactFactory artifactFactory,
            VersionsHelper helper,
            ProjectBuilder projectBuilder,
            MavenSession session,
            MavenProject project,
            Log log,
            ArtifactVersion runningMaven,
            boolean allowSnapshots) {
        this.artifactFactory = artifactFactory;
        this.helper = helper;
        this.projectBuilder = projectBuilder;
        this.session = session;
        this.project = project;
        this.log = log;
        this.runningMaven = runningMaven;
        this.minimumMaven = MinimalMavenBuildVersionFinder.find(project, log);
        this.allowSnapshots = allowSnapshots;
    }

    ArtifactVersion getMinimumMaven() {
        return minimumMaven;
    }

    PluginUpdateAnalysis analyze(Plugin plugin, String effectiveVersion)
            throws MojoExecutionException, VersionRetrievalException {
        ArtifactVersions allVersions = lookupVersions(plugin, effectiveVersion);
        ArtifactVersion[] candidates = allVersions.getVersions(allowSnapshots);
        ArtifactVersion compatible = null;
        ArtifactVersion minRequires = null;
        Map<ArtifactVersion, ArtifactVersion> upgrades = new LinkedHashMap<>();
        Map<ArtifactVersion, String> upgradeFrom = new LinkedHashMap<>();
        for (int i = candidates.length - 1; i >= 0; i--) {
            String candidate = candidates[i].toString();
            try {
                ArtifactVersion required = prerequisite(plugin, candidate);
                if (compatible == null && compare(minimumMaven, required) >= 0) {
                    compatible = candidates[i];
                }
                if (effectiveVersion == null && compare(runningMaven, required) >= 0) {
                    effectiveVersion = candidate;
                }
                if (compatible != null && effectiveVersion != null) {
                    break;
                }
                if (minRequires == null || compare(minRequires, required) > 0) {
                    upgrades.putIfAbsent(required, candidates[i]);
                    upgradeFrom.putIfAbsent(required, effectiveVersion);
                    minRequires = required;
                }
            } catch (ArtifactResolutionException | ProjectBuildingException e) {
                // Preserve the CLI policy of skipping candidate POMs that cannot be resolved.
            }
        }
        ArtifactVersion currentRequires = null;
        if (effectiveVersion != null) {
            try {
                currentRequires = prerequisite(plugin, effectiveVersion);
            } catch (ArtifactResolutionException | ProjectBuildingException e) {
                // The current plugin's Maven requirement remains unknown.
            }
        }
        return new PluginUpdateAnalysis(effectiveVersion, compatible, currentRequires, upgrades, upgradeFrom);
    }

    private ArtifactVersions lookupVersions(Plugin plugin, String effectiveVersion) throws VersionRetrievalException {
        String key = plugin.getKey() + ":" + effectiveVersion;
        ArtifactVersions allVersions = versions.get(key);
        if (allVersions == null) {
            allVersions = helper.lookupArtifactVersions(
                    artifactFactory.createMavenPluginArtifact(
                            plugin.getGroupId(), plugin.getArtifactId(), effectiveVersion),
                    true);
            versions.put(key, allVersions);
        }
        return allVersions;
    }

    /** Reports list all updates. Only an unspecified current version needs Maven prerequisite inference. */
    PluginUpdatesDetails reportDetails(Plugin plugin) throws MojoExecutionException, VersionRetrievalException {
        String currentVersion = plugin.getVersion();
        ArtifactVersions all = lookupVersions(plugin, currentVersion);
        if (currentVersion == null) {
            currentVersion = analyze(plugin, null).getEffectiveVersion();
            if (currentVersion != null) {
                all = new ArtifactVersions(
                        artifactFactory.createMavenPluginArtifact(
                                plugin.getGroupId(), plugin.getArtifactId(), currentVersion),
                        Arrays.asList(all.getVersions(allowSnapshots)));
            }
        }
        return new PluginUpdatesDetails(
                new ArtifactVersions(
                        all.getArtifact(),
                        Arrays.stream(all.getAllUpdates(allowSnapshots)).collect(Collectors.toList())),
                helper.lookupDependenciesUpdates(plugin.getDependencies().stream(), false, allowSnapshots),
                allowSnapshots);
    }

    private ArtifactVersion prerequisite(Plugin plugin, String version)
            throws MojoExecutionException, ArtifactResolutionException, ProjectBuildingException {
        String key = plugin.getKey() + ":" + version;
        if (failures.containsKey(key)) {
            Exception failure = failures.get(key);
            if (failure instanceof ArtifactResolutionException) {
                throw (ArtifactResolutionException) failure;
            }
            throw (ProjectBuildingException) failure;
        }
        if (!prerequisites.containsKey(key)) {
            try {
                Artifact probe = artifactFactory.createArtifact(DependencyBuilder.newBuilder()
                        .withGroupId(plugin.getGroupId())
                        .withArtifactId(plugin.getArtifactId())
                        .withVersion(version)
                        .withType("pom")
                        .withScope(Artifact.SCOPE_RUNTIME)
                        .build());
                helper.resolveArtifact(probe, true);
                ProjectBuildingResult result = projectBuilder.build(
                        probe,
                        true,
                        PomHelper.createProjectBuilderRequest(
                                session,
                                r -> r.setProcessPlugins(false),
                                r -> r.setRemoteRepositories(project.getRemoteArtifactRepositories()),
                                r -> r.setPluginArtifactRepositories(project.getPluginArtifactRepositories())));
                if (!result.getProblems().isEmpty()) {
                    log.warn("Problems encountered during construction of the plugin POM for " + probe);
                    result.getProblems().forEach(p -> log.warn("\t" + p.getMessage()));
                }
                prerequisites.put(
                        key,
                        Optional.ofNullable(result.getProject().getPrerequisites())
                                .map(Prerequisites::getMaven)
                                .map(ArtifactVersionService::getArtifactVersion));
            } catch (ArtifactResolutionException | ProjectBuildingException e) {
                failures.put(key, e);
                throw e;
            }
        }
        return prerequisites.get(key).orElse(null);
    }

    private static int compare(ArtifactVersion left, ArtifactVersion right) {
        return left == null ? right == null ? 0 : -1 : right == null ? 1 : left.compareTo(right);
    }
}
