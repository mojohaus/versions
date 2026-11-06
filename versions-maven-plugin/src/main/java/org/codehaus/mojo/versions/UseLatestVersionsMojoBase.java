package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.BuildBase;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.model.Reporting;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.recording.DefaultDependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;

/**
 * Common base class for {@link UseLatestVersionsMojo}
 * and {@link UseLatestReleasesMojo}
 */
public abstract class UseLatestVersionsMojoBase extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * Number of executor threads for update retrieval.
     *
     * @since 2.19.0
     */
    @Parameter(property = "numThreads", defaultValue = "5")
    private int numThreads = 5;

    /**
     * Whether snapshots should be allowed when searching for newer versions
     * @return {@code true} if snapshots should be allowed when searching for newer versions
     */
    protected abstract boolean getAllowMajorUpdates();

    /**
     * Whether minor updates should be allowed when searching for newer versions
     * @return {@code true} if minor updates should be allowed when searching for newer versions
     */
    protected abstract boolean getAllowMinorUpdates();

    /**
     * Whether incremental updates should be allowed when searching for newer versions
     * @return {@code true} if incremental updates should be allowed when searching for newer versions
     */
    protected abstract boolean getAllowIncrementalUpdates();

    /**
     * Whether downgrades should be allowed when searching for newer versions
     * @return {@code true} if downgrades should be allowed when searching for newer versions
     */
    protected abstract boolean getAllowDowngrade();

    /**
     * Whether snapshots should be allowed when searching for newer versions
     * @return {@code true} if snapshots should be allowed when searching for newer versions
     */
    protected abstract boolean getAllowSnapshots();
    /**
     * A filter for dependencies to be updated.
     *
     * @param dep the dependency to check
     * @return {@code true} if the dependency should be updated, {@code false} otherwise
     */
    protected abstract boolean updateFilter(Dependency dep);

    /**
     * A filter for artifact versions to be considered.
     *
     * @param ver the artifact version to check
     * @return {@code true} if the artifact version should be considered, {@code false} otherwise
     */
    protected abstract boolean artifactVersionsFilter(ArtifactVersion ver);

    /**
     * Produces a version from a stream of artifact versions.
     *
     * @param stream the stream of artifact versions
     * @return an optional containing the produced version, or an empty optional if no version could be produced
     */
    protected abstract Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream);

    private final ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    private final ProjectBuilder projectBuilder;

    private final RuntimeInformation runtimeInformation;

    /**
     * Creates a new instance
     *
     * @param artifactFactory   an {@link ArtifactFactory} instance
     * @param repositorySystem  a {@link RepositorySystem} instance
     * @param wagonMap          a map of wagon providers per protocol
     * @param changeRecorders   a map of change recorders
     * @throws MojoExecutionException when things go wrong
     */
    public UseLatestVersionsMojoBase(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        this(artifactFactory, repositorySystem, null, null, wagonMap, changeRecorders);
    }

    /**
     * Creates a new instance that can also update plugin versions.
     *
     * @param artifactFactory    an {@link ArtifactFactory} instance
     * @param repositorySystem   a {@link RepositorySystem} instance
     * @param projectBuilder     a {@link ProjectBuilder} instance, used to read the Maven prerequisites of plugins
     * @param runtimeInformation a {@link RuntimeInformation} instance
     * @param wagonMap           a map of wagon providers per protocol
     * @param changeRecorders    a map of change recorders
     * @throws MojoExecutionException when things go wrong
     * @since 2.25.0
     */
    public UseLatestVersionsMojoBase(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            ProjectBuilder projectBuilder,
            RuntimeInformation runtimeInformation,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
        this.projectBuilder = projectBuilder;
        this.runtimeInformation = runtimeInformation;
    }

    /**
     * Whether the versions of plugins declared in the pom should be updated.
     *
     * @return {@code true} if plugin versions should be updated; {@code false} by default
     * @since 2.25.0
     */
    protected boolean getProcessPlugins() {
        return false;
    }

    /**
     * Represents a version change of an artifact
     */
    private abstract static class ArtifactVersionChange {
        private final DependencyChangeRecord.ChangeKind changeKind;

        private final String newVersion;

        /**
         * Creates a new instance
         *
         * @param changeKind change kind
         * @param newVersion new version
         */
        ArtifactVersionChange(DependencyChangeRecord.ChangeKind changeKind, String newVersion) {
            this.changeKind = changeKind;
            this.newVersion = newVersion;
        }

        /**
         * @return change kind
         */
        DependencyChangeRecord.ChangeKind getChangeKind() {
            return changeKind;
        }

        /**
         * @return new version
         */
        String getNewVersion() {
            return newVersion;
        }
    }

    /**
     * Represents a version change of a dependency
     */
    private static final class DependencyVersionChange extends ArtifactVersionChange {
        private final Dependency dependency;

        /**
         * Constructs a new instance
         *
         * @param changeKind change kind
         * @param dependency {@code Dependency} instance
         * @param newVersion new version
         */
        DependencyVersionChange(
                DependencyChangeRecord.ChangeKind changeKind, Dependency dependency, String newVersion) {
            super(changeKind, newVersion);
            this.dependency = dependency;
        }

        /**
         * @return {@code Dependency} instance
         */
        Dependency getDependency() {
            return dependency;
        }
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        if (getAllowDowngrade() && getAllowSnapshots()) {
            throw new MojoExecutionException("allowDowngrade is only valid with allowSnapshots equal to false");
        }

        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                getAllowMajorUpdates(), getAllowMinorUpdates(), getAllowIncrementalUpdates(), getLog());
        ConcurrentLinkedQueue<DependencyVersionChange> versionChanges = new ConcurrentLinkedQueue<>();
        Collection<CompletableFuture<Void>> versionChangeFutures = new ArrayList<>();
        try {
            if (getProcessDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    versionChangeFutures.add(getUpdates(
                            versionChanges,
                            dependencyManagement.getDependencies(),
                            DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT,
                            unchangedSegment));
                }
            }
            if (getProject().getDependencies() != null && getProcessDependencies()) {
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        getProject().getDependencies(),
                        DependencyChangeRecord.ChangeKind.DEPENDENCY,
                        unchangedSegment));
            }
            if (getProject().getParent() != null && getProcessParent()) {
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        singletonList(getParentDependency()),
                        DependencyChangeRecord.ChangeKind.PARENT,
                        unchangedSegment));
            }

            PluginCeiling pluginCeiling = null;
            if (getProcessPlugins()) {
                pluginCeiling = pluginCeiling();
                Model rawModel = PomHelper.getRawModel(getProject());
                List<Dependency> managed = new ArrayList<>();
                List<Dependency> plugins = new ArrayList<>();
                collectPlugins(rawModel, managed, plugins);
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        managed,
                        DependencyChangeRecord.ChangeKind.PLUGIN_MANAGEMENT,
                        unchangedSegment,
                        pluginCeiling));
                versionChangeFutures.add(getUpdates(
                        versionChanges,
                        plugins,
                        DependencyChangeRecord.ChangeKind.PLUGIN,
                        unchangedSegment,
                        pluginCeiling));
            }

            CompletableFuture.allOf(versionChangeFutures.toArray(new CompletableFuture[0]))
                    .join();
            List<DependencyVersionChange> pluginChanges = new ArrayList<>();
            for (DependencyVersionChange change : versionChanges) {
                if (isPluginChange(change.getChangeKind())) {
                    pluginChanges.add(change);
                } else {
                    updateDependencyVersion(
                            getProject(), pom, change.getDependency(), change.getNewVersion(), change.getChangeKind());
                }
            }
            updatePluginVersions(pom, pluginChanges);
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        } catch (CompletionException e) {
            if (e.getCause() instanceof IllegalStateException) {
                IllegalStateException ise = (IllegalStateException) e.getCause();
                if (ise.getCause() instanceof MojoExecutionException) {
                    throw (MojoExecutionException) ise.getCause();
                }
                if (ise.getCause() instanceof VersionRetrievalException) {
                    throw (VersionRetrievalException) ise.getCause();
                }
            }
            throw e;
        }
    }

    private CompletableFuture<Void> getUpdates(
            ConcurrentLinkedQueue<DependencyVersionChange> updates,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind,
            Optional<Segment> unchangedSegment) {
        return getUpdates(updates, dependencies, changeKind, unchangedSegment, null);
    }

    private CompletableFuture<Void> getUpdates(
            ConcurrentLinkedQueue<DependencyVersionChange> updates,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind,
            Optional<Segment> unchangedSegment,
            PluginCeiling pluginCeiling) {
        boolean plugins = isPluginChange(changeKind);
        return CompletableFuture.allOf(dependencies.stream()
                .map(dep -> CompletableFuture.runAsync(
                        () -> {
                            if (!updateFilter(dep)) {
                                return;
                            } else if (getExcludeReactor() && isProducedByReactor(dep)) {
                                getLog().info("Ignoring reactor dependency: " + toString(dep));
                            } else if (isHandledByProperty(dep)) {
                                getLog().debug("Ignoring dependency with property as version: " + toString(dep));
                            } else {
                                try {
                                    ArtifactFilter included = plugins ? includedPluginArtifacts() : includedArtifacts();
                                    Artifact artifact = plugins
                                            ? artifactFactory.createMavenPluginArtifact(
                                                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion())
                                            : toArtifact(dep);
                                    if (!included.include(artifact)) {
                                        return;
                                    } else if (getLog().isDebugEnabled()) {
                                        ArtifactVersion selectedVersion =
                                                ArtifactVersionService.getArtifactVersion(dep.getVersion());
                                        getLog().debug("Selected version:" + selectedVersion);
                                        getLog().debug("Looking for newer versions of " + toString(dep));
                                    }

                                    ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, plugins);
                                    Artifact candidate = ArtifactUtils.copyArtifact(artifact);
                                    List<ArtifactVersion> candidates = Arrays.stream(versions.getNewerVersions(
                                                    dep.getVersion(),
                                                    unchangedSegment,
                                                    getAllowSnapshots(),
                                                    getAllowDowngrade()))
                                            .filter(this::artifactVersionsFilter)
                                            // includes and excludes can name versions (#1378)
                                            .filter(ver -> {
                                                candidate.setVersion(ver.toString());
                                                return included.include(candidate);
                                            })
                                            .collect(Collectors.toList());
                                    selectVersion(dep, candidates, pluginCeiling)
                                            .ifPresent(ver -> updates.add(
                                                    new DependencyVersionChange(changeKind, dep, ver.toString())));
                                } catch (VersionRetrievalException
                                        | InvalidSegmentException
                                        | MojoExecutionException e) {
                                    throw new IllegalStateException(e);
                                }
                            }
                        },
                        executor))
                .toArray(CompletableFuture[]::new));
    }

    /**
     * Picks the version {@link #versionProducer(Stream)} chooses. For a plugin, a version that requires a newer Maven
     * than the ceiling is dropped and the choice is made again from the remaining versions.
     */
    private Optional<ArtifactVersion> selectVersion(
            Dependency dep, List<ArtifactVersion> candidates, PluginCeiling pluginCeiling)
            throws MojoExecutionException {
        Optional<ArtifactVersion> selected = versionProducer(candidates.stream());
        if (pluginCeiling == null) {
            return selected;
        }
        boolean logged = false;
        while (selected.isPresent()) {
            String reason = pluginCeiling.rejects(dep, selected.get());
            if (reason == null) {
                return selected;
            }
            if (!logged) {
                getLog().info("Not updating " + toString(dep) + " to " + selected.get() + ": " + reason);
                logged = true;
            }
            candidates.remove(selected.get());
            selected = versionProducer(candidates.stream());
        }
        return selected;
    }

    private static boolean isPluginChange(DependencyChangeRecord.ChangeKind changeKind) {
        return changeKind == DependencyChangeRecord.ChangeKind.PLUGIN
                || changeKind == DependencyChangeRecord.ChangeKind.PLUGIN_MANAGEMENT;
    }

    private PluginCeiling pluginCeiling() throws MojoExecutionException {
        if (projectBuilder == null || runtimeInformation == null) {
            throw new MojoExecutionException(getClass().getName() + " cannot update plugins: it was created without"
                    + " a ProjectBuilder and RuntimeInformation");
        }
        ArtifactVersion runningMaven = ArtifactVersionService.getArtifactVersion(runtimeInformation.getMavenVersion());
        PluginUpdatesAnalyzer analyzer = new PluginUpdatesAnalyzer(
                artifactFactory,
                getHelper(),
                projectBuilder,
                session,
                getProject(),
                getLog(),
                runningMaven,
                getAllowSnapshots());
        ArtifactVersion maven = analyzer.getMinimumMaven();
        String source = "the project requires Maven " + maven;
        if (maven == null) {
            maven = runningMaven;
            source = "the project declares no minimum Maven version and this build runs on Maven " + maven;
            getLog().info("Plugin updates are limited to versions that run on Maven " + maven
                    + ", the running version: the project declares no minimum Maven version");
        }
        return new PluginCeiling(analyzer, maven, source);
    }

    /**
     * Adds the plugins of the raw model that have a version of their own, each version of a plugin once:
     * {@link PomHelper#setPluginVersion} rewrites every declaration of a plugin that has the old version.
     */
    private static void collectPlugins(Model rawModel, List<Dependency> managed, List<Dependency> plugins) {
        Collection<String> seen = new HashSet<>();
        List<BuildBase> builds = new ArrayList<>();
        List<Reporting> reportings = new ArrayList<>();
        builds.add(rawModel.getBuild());
        reportings.add(rawModel.getReporting());
        for (Profile profile : rawModel.getProfiles()) {
            builds.add(profile.getBuild());
            reportings.add(profile.getReporting());
        }
        for (BuildBase build : builds) {
            if (build != null && build.getPluginManagement() != null) {
                for (Plugin plugin : build.getPluginManagement().getPlugins()) {
                    addPlugin(managed, seen, plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion());
                }
            }
        }
        for (BuildBase build : builds) {
            if (build != null) {
                for (Plugin plugin : build.getPlugins()) {
                    addPlugin(plugins, seen, plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion());
                }
            }
        }
        for (Reporting reporting : reportings) {
            if (reporting != null) {
                for (ReportPlugin plugin : reporting.getPlugins()) {
                    addPlugin(plugins, seen, plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion());
                }
            }
        }
    }

    private static void addPlugin(
            List<Dependency> result, Collection<String> seen, String groupId, String artifactId, String version) {
        // unversioned plugins take their version from elsewhere; partly interpolated ones cannot be rewritten
        if (version == null
                || version.trim().isEmpty()
                || (version.contains("${") && !version.startsWith("${"))
                || !seen.add(groupId + ":" + artifactId + ":" + version)) {
            return;
        }
        result.add(DependencyBuilder.newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version.trim())
                .withType("maven-plugin")
                .build());
    }

    /**
     * Applies the plugin changes in two passes, through a placeholder per change, because one plugin can be
     * declared with several versions: updating 1.0 to 2.0 before 2.0 to 2.1 would update both declarations to 2.1.
     */
    private void updatePluginVersions(MutableXMLStreamReader pom, List<DependencyVersionChange> changes)
            throws XMLStreamException, MojoExecutionException {
        List<String> placeholders = new ArrayList<>();
        for (DependencyVersionChange change : changes) {
            Dependency plugin = change.getDependency();
            String placeholder = "versions-maven-plugin-placeholder-" + placeholders.size();
            boolean replaced = PomHelper.setPluginVersion(
                    pom, plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion(), placeholder);
            placeholders.add(replaced ? placeholder : null);
        }
        for (int i = 0; i < changes.size(); i++) {
            DependencyVersionChange change = changes.get(i);
            Dependency plugin = change.getDependency();
            if (placeholders.get(i) != null
                    && PomHelper.setPluginVersion(
                            pom,
                            plugin.getGroupId(),
                            plugin.getArtifactId(),
                            placeholders.get(i),
                            change.getNewVersion())) {
                getLog().info("Updated " + toString(plugin) + " to version " + change.getNewVersion() + " in "
                        + toString(getProject()));
                getChangeRecorder()
                        .recordChange(DefaultDependencyChangeRecord.builder()
                                .withKind(change.getChangeKind())
                                .withDependency(plugin)
                                .withNewVersion(change.getNewVersion())
                                .build());
            }
        }
    }

    /**
     * The newest Maven version a plugin update may require.
     */
    private static final class PluginCeiling {
        private final PluginUpdatesAnalyzer analyzer;

        private final ArtifactVersion maven;

        private final String source;

        PluginCeiling(PluginUpdatesAnalyzer analyzer, ArtifactVersion maven, String source) {
            this.analyzer = analyzer;
            this.maven = maven;
            this.source = source;
        }

        /**
         * @return why the version cannot be used, or {@code null} if it can
         */
        String rejects(Dependency plugin, ArtifactVersion version) throws MojoExecutionException {
            Plugin probe = new Plugin();
            probe.setGroupId(plugin.getGroupId());
            probe.setArtifactId(plugin.getArtifactId());
            try {
                Optional<ArtifactVersion> required = analyzer.requiredMaven(probe, version.toString());
                if (required.isPresent() && maven.compareTo(required.get()) < 0) {
                    return "it requires Maven " + required.get() + ", but " + source;
                }
                return null;
            } catch (ArtifactResolutionException | ProjectBuildingException e) {
                return "its POM cannot be read to check the Maven version it requires (" + e.getMessage() + ")";
            }
        }
    }
}
