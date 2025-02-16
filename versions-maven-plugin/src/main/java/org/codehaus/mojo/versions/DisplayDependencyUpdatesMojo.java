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

import javax.inject.Inject;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.internal.DependencyUpdatesLoggingHelper;
import org.codehaus.mojo.versions.internal.DependencyUpdatesLoggingHelper.DependencyUpdatesResult;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.MavenProjectUtils;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.emptySet;
import static org.codehaus.mojo.versions.filtering.DependencyFilter.filterDependencies;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractDependenciesFromDependencyManagement;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractDependenciesFromPlugins;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractPluginDependenciesFromPluginsInPluginManagement;

/**
 * Displays all dependencies that have newer versions available.
 * It will also display dependencies which are used by a plugin or
 * defined in the plugin within a pluginManagement.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo(name = "display-dependency-updates", threadSafe = true)
public class DisplayDependencyUpdatesMojo extends AbstractVersionsDisplayMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.2
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    protected boolean processDependencyManagement = true;

    /**
     * Whether to process the dependencyManagement part transitive or not.
     * In case of <code>&lt;type&gt;pom&lt;/type&gt;</code>and
     * <code>&lt;scope&gt;import&lt;/scope&gt;</code> this means
     * by default to report also the imported dependencies.
     * If processTransitive is set to <code>false</code> the report will only show
     * updates of the imported pom it self.
     *
     * @since 2.11
     */
    @Parameter(property = "processDependencyManagementTransitive", defaultValue = "true")
    protected boolean processDependencyManagementTransitive = true;

    /**
     * <p>Include dependencies with version set in a parent or in a BOM.</p>
     * <p>This is similar to {@code processDependencyManagementTransitive}, but will
     * report updates on dependencies.</p>
     * <p>Default is {@code false}.</p>
     *
     * @since 2.19.0
     */
    @Parameter(property = "showVersionless", defaultValue = "true")
    protected boolean showVersionless = true;

    /**
     * Only take these artifacts into consideration.
     * <p>
     * Comma-separated list of extended GAV patterns.
     * <p>
     * Extended GAV: groupId:artifactId:version:type:classifier:scope
     * </p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,*:*:*:*:*:compile"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "dependencyManagementIncludes", defaultValue = WildcardMatcher.WILDCARD)
    private List<String> dependencyManagementIncludes;

    /**
     * Exclude these artifacts from consideration.
     * <p>
     * Comma-separated list of extended GAV patterns.
     * <p>
     * Extended GAV: groupId:artifactId:version:type:classifier:scope
     * </p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,*:*:*:*:*:provided,*:*:*:*:*:system"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "dependencyManagementExcludes")
    private List<String> dependencyManagementExcludes;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.2
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    protected boolean processDependencies;

    /**
     * <p>Only take the specified <u>input</u> dependencies into account.</p>
     * <p><b><u>Note</u>: even if a version is specified, it will refer to the input dependency version.</b>
     * To filter <u>output</u> versions, please use {@link #ruleSet} or {@link #ignoredVersions}.</p>
     * <p>
     * Comma-separated list of extended GAV patterns.
     * <p>
     * Extended GAV: groupId:artifactId:version:type:classifier:scope
     * </p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,*:*:*:*:*:compile"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "dependencyIncludes", defaultValue = WildcardMatcher.WILDCARD)
    protected List<String> dependencyIncludes;

    /**
     * <p>Do not take the specified <u>input</u> dependencies into account.</p>
     * <p><b><u>Note</u>: even if a version is specified, it will refer to the input dependency version.</b>
     * To filter <u>output</u> versions, please use {@link #ruleSet} or {@link #ignoredVersions}.</p>
     * <p>
     * Comma-separated list of extended GAV patterns.
     * <p>
     * Extended GAV: groupId:artifactId:version:type:classifier:scope
     * </p>
     * <p>
     * The wildca<u>Note:</u>rd "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,*:*:*:*:*:provided,*:*:*:*:*:system"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "dependencyExcludes")
    protected List<String> dependencyExcludes;

    /**
     * Whether to process the dependencies sections of plugins.
     *
     * @since 2.5
     */
    @Parameter(property = "processPluginDependencies", defaultValue = "true")
    private boolean processPluginDependencies;

    /**
     * Whether to process the dependencies sections of plugins which are defined in pluginManagement.
     *
     * @since 2.5
     */
    @Parameter(property = "processPluginDependenciesInPluginManagement", defaultValue = "true")
    private boolean processPluginDependenciesInPluginManagement;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.5
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} to be {@code false}</b></p>
     *
     * @since 2.5
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} to be {@code false}</b></p>
     *
     * @since 2.5
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * <p>If {@code true}, shows dependencies that do not have updates. Also, with dependencies with
     * versions managed outside the reactor, will show the location of the pom.xml managing the version.</p>
     * <p>Default is {@code false}.</p>
     *
     * @since 2.1
     */
    @Parameter(property = "verbose", defaultValue = "false")
    protected boolean verbose;

    /**
     * <p>Only take these artifacts into consideration:<br/>
     * Comma-separated list of {@code groupId:[artifactId[:version]]} patterns</p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,othergroup:*,anothergroup"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "pluginDependencyIncludes", defaultValue = WildcardMatcher.WILDCARD)
    private List<String> pluginDependencyIncludes;

    /**
     * <p>Exclude these artifacts into consideration:<br/>
     * Comma-separated list of {@code groupId:[artifactId[:version]]} patterns</p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,othergroup:*,anothergroup"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "pluginDependencyExcludes")
    private List<String> pluginDependencyExcludes;

    /**
     * <p>Only take these artifacts into consideration:<br/>
     * Comma-separated list of {@code groupId:[artifactId[:version]]} patterns</p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,othergroup:*,anothergroup"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "pluginManagementDependencyIncludes", defaultValue = WildcardMatcher.WILDCARD)
    private List<String> pluginManagementDependencyIncludes;

    /**
     * <p>Exclude these artifacts into consideration:<br/>
     * Comma-separated list of {@code groupId:[artifactId[:version]]} patterns</p>
     * <p>
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     * </p>
     * <p>
     * Example: {@code "mygroup:artifact:*,othergroup:*,anothergroup"}
     * </p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "pluginManagementDependencyExcludes")
    private List<String> pluginManagementDependencyExcludes;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    // --------------------- GETTER / SETTER METHODS ---------------------

    @Inject
    public DisplayDependencyUpdatesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    // open for tests
    protected static boolean dependenciesMatch(Dependency dependency, Dependency managedDependency) {
        if (!managedDependency.getGroupId().equals(dependency.getGroupId())) {
            return false;
        }

        if (!managedDependency.getArtifactId().equals(dependency.getArtifactId())) {
            return false;
        }

        if (managedDependency.getScope() == null
                || Objects.equals(managedDependency.getScope(), dependency.getScope())) {
            return false;
        }

        if (managedDependency.getClassifier() == null
                || Objects.equals(managedDependency.getClassifier(), dependency.getClassifier())) {
            return false;
        }

        return dependency.getVersion() == null
                || managedDependency.getVersion() == null
                || Objects.equals(managedDependency.getVersion(), dependency.getVersion());
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        logInit();
        validateInput();

        Set<Dependency> dependencyManagement;
        try {
            if (processDependencyManagement) {
                dependencyManagement = filterDependencies(
                        extractDependenciesFromDependencyManagement(
                                getProject(), processDependencyManagementTransitive, getLog()),
                        dependencyManagementIncludes,
                        dependencyManagementExcludes,
                        "Dependecy Management",
                        getLog());

                logUpdates(
                        getHelper()
                                .lookupDependenciesUpdates(
                                        dependencyManagement.stream().filter(d -> d.getVersion() != null),
                                        false,
                                        allowSnapshots),
                        "Dependency Management");
            } else {
                dependencyManagement = emptySet();
            }
            if (processDependencies) {
                logUpdates(
                        getHelper()
                                .lookupDependenciesUpdates(
                                        filterDependencies(
                                                        getProject().getDependencies().stream()
                                                                .filter(dep -> dependencyManagement.stream()
                                                                        .noneMatch(depMan ->
                                                                                dependenciesMatch(dep, depMan)))
                                                                .filter(dep -> showVersionless
                                                                        || MavenProjectUtils
                                                                                .dependencyVersionLocalToReactor(dep))
                                                                .collect(
                                                                        () -> new TreeSet<>(
                                                                                DependencyComparator.INSTANCE),
                                                                        Set::add,
                                                                        Set::addAll),
                                                        dependencyIncludes,
                                                        dependencyExcludes,
                                                        "Dependencies",
                                                        getLog())
                                                .stream()
                                                .filter(d -> d.getVersion() != null),
                                        false,
                                        allowSnapshots),
                        "Dependencies");
            }
            if (processPluginDependenciesInPluginManagement) {
                logUpdates(
                        getHelper()
                                .lookupDependenciesUpdates(
                                        filterDependencies(
                                                        extractPluginDependenciesFromPluginsInPluginManagement(
                                                                getProject()),
                                                        pluginManagementDependencyIncludes,
                                                        pluginManagementDependencyExcludes,
                                                        "Plugin Management Dependencies",
                                                        getLog())
                                                .stream()
                                                .filter(d -> d.getVersion() != null),
                                        false,
                                        allowSnapshots),
                        "pluginManagement of plugins");
            }
            if (processPluginDependencies) {
                logUpdates(
                        getHelper()
                                .lookupDependenciesUpdates(
                                        filterDependencies(
                                                        extractDependenciesFromPlugins(getProject()),
                                                        pluginDependencyIncludes,
                                                        pluginDependencyExcludes,
                                                        "Plugin Dependencies",
                                                        getLog())
                                                .stream()
                                                .filter(d -> d.getVersion() != null),
                                        false,
                                        allowSnapshots),
                        "Plugin Dependencies");
            }
        } catch (VersionRetrievalException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    @Override
    protected void validateInput() throws MojoExecutionException {
        validateGAVList(dependencyIncludes, 6, "dependencyIncludes");
        validateGAVList(dependencyExcludes, 6, "dependencyExcludes");
        validateGAVList(dependencyManagementIncludes, 6, "dependencyManagementIncludes");
        validateGAVList(dependencyManagementExcludes, 6, "dependencyManagementExcludes");
        validateGAVList(pluginDependencyIncludes, 3, "pluginDependencyIncludes");
        validateGAVList(pluginDependencyExcludes, 3, "pluginDependencyExcludes");
        validateGAVList(pluginManagementDependencyIncludes, 3, "pluginManagementDependencyIncludes");
        validateGAVList(pluginManagementDependencyExcludes, 3, "pluginManagementDependencyExcludes");
    }

    /**
     * Validates a list of GAV strings
     *
     * @param gavList      list of the input GAV strings
     * @param numSections  number of sections in the GAV to verify against
     * @param argumentName argument name to indicate in the exception
     * @throws MojoExecutionException if the argument is invalid
     */
    static void validateGAVList(List<String> gavList, int numSections, String argumentName)
            throws MojoExecutionException {
        if (gavList != null && gavList.stream().anyMatch(gav -> StringUtils.countMatches(gav, ":") >= numSections)) {
            throw new MojoExecutionException(argumentName + " should not contain more than 6 segments");
        }
    }

    private void logUpdates(Map<Dependency, ArtifactVersions> versionMap, String section) {
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog());
        DependencyUpdatesResult updates = DependencyUpdatesLoggingHelper.getDependencyUpdates(
                getProject(),
                versionMap,
                allowSnapshots,
                unchangedSegment,
                INFO_PAD_SIZE + getOutputLineWidthOffset(),
                verbose);

        if (verbose) {
            if (updates.getUsingLatest().isEmpty()) {
                if (!updates.getWithUpdates().isEmpty()) {
                    logLine(false, "No dependencies in " + section + " are using the newest version.");
                    logLine(false, "");
                }
            } else {
                logLine(false, "The following dependencies in " + section + " are using the newest version:");
                updates.getUsingLatest().forEach(s -> logLine(false, s));
                logLine(false, "");
            }
        }

        if (updates.getWithUpdates().isEmpty()) {
            if (!updates.getUsingLatest().isEmpty()) {
                logLine(false, "No dependencies in " + section + " have newer versions.");
                logLine(false, "");
            }
        } else {
            logLine(false, "The following dependencies in " + section + " have newer versions:");
            updates.getWithUpdates().forEach(s -> logLine(false, s));
            logLine(false, "");
        }
    }

    /**
     * @param pom the pom to update.
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     * @since 1.0-alpha-1
     */
    @Override
    protected void update(MutableXMLStreamReader pom) {
        // do nothing
    }
}
