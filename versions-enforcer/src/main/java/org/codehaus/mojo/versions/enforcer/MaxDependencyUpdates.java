package org.codehaus.mojo.versions.enforcer;
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
import javax.inject.Named;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.enforcer.rule.api.AbstractEnforcerRule;
import org.apache.maven.enforcer.rule.api.EnforcerRuleError;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.rule.RuleService;
import org.codehaus.mojo.versions.rule.RulesServiceBuilder;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.VersionsExpressionEvaluator;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.codehaus.mojo.versions.filtering.DependencyFilter.filterDependencies;
import static org.codehaus.mojo.versions.filtering.WildcardMatcher.WILDCARD;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractDependenciesFromDependencyManagement;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractDependenciesFromPlugins;
import static org.codehaus.mojo.versions.utils.MavenProjectUtils.extractPluginDependenciesFromPluginsInPluginManagement;

@Named("maxDependencyUpdates")
public class MaxDependencyUpdates extends AbstractEnforcerRule {
    private final ArtifactFactory artifactFactory;

    private final ArtifactHandlerManager artifactHandlerManager;

    /**
     * Maximum allowed number of updates.
     *
     * @since 2.14.0
     */
    protected int maxUpdates = 0;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 2.14.0
     */
    protected boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.2
     */
    protected boolean processDependencyManagement = true;

    /**
     * Whether to process the dependencyManagement part transitive or not.
     * In case of type {@code pom} and scope {@code import}, this means
     * by default to report also the imported dependencies.
     * If the parameter is set to {@code false}, the report will only show
     * updates of the imported pom itself.
     *
     * @since 2.14.0
     */
    protected boolean processDependencyManagementTransitive = true;

    /**
     * Whether to process the dependencies sections of plugins.
     *
     * @since 2.14.0
     */
    protected boolean processPluginDependencies = true;

    /**
     * Whether to process the dependencies sections of plugins which are defined in pluginManagement.
     *
     * @since 2.14.0
     */
    protected boolean processPluginDependenciesInPluginManagement = true;

    /**
     * Whether minor updates should be ignored. Default {@code false}.
     *
     * <p><b>Note: </b> when {@code true}, will also assume that {@link #ignoreIncrementalUpdates}
     * and {@link #ignoreSubIncrementalUpdates} are {@code true}.</p>
     *
     * @since 2.14.0
     */
    protected boolean ignoreMinorUpdates = false;

    /**
     * Whether incremental updates should be ignored. Default {@code false}.
     *
     * <p><b>Note: </b> when {@code true}, will also assume that
     * {@link #ignoreSubIncrementalUpdates} is {@code true}.</p>
     *
     * @since 2.14.0
     */
    protected boolean ignoreIncrementalUpdates = false;

    /**
     * Whether sub-incremental updates should be ignored. Default {@code false}.
     *
     * @since 2.14.0
     */
    protected boolean ignoreSubIncrementalUpdates = false;

    /**
     * <p>List of <u>input</u> dependency inclusion patterns.</p>
     * <p><b><u>Note</u>: even if a version is specified, it will refer to the input dependency version.</b>
     * To filter <u>output</u> versions, please use {@link #ruleSet}.</p>
     * <p>Only dependencies matching all the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     *
     * @since 2.14.0
     */
    protected List<String> dependencyIncludes = singletonList(WILDCARD);

    /**
     * <p>List of <u>input</u> dependency exclusion patterns.</p>
     * <p><b><u>Note</u>: even if a version is specified, it will refer to the input dependency version.</b>
     * To filter <u>output</u> versions, please use {@link #ruleSet}.</p>
     * <p>Only dependencies matching none of the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     *
     * @since 2.14.0
     */
    protected List<String> dependencyExcludes = emptyList();

    /**
     * List of dependency management inclusion patterns.
     * Only dependencies matching all the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> dependencyManagementIncludes = singletonList(WILDCARD);

    /**
     * List of dependency management exclusion patterns.
     * Only dependencies matching none of the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> dependencyManagementExcludes = emptyList();

    /**
     * List of plugin dependency inclusion patterns.
     * Only dependencies matching all the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> pluginDependencyIncludes = singletonList(WILDCARD);

    /**
     * List of plugin dependency exclusion patterns.
     * Only dependencies matching none of the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> pluginDependencyExcludes = emptyList();

    /**
     * List of plugin management dependency inclusion patterns.
     * Only dependencies matching all the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> pluginManagementDependencyIncludes = singletonList(WILDCARD);

    /**
     * List of plugin dependency management exclusion patterns.
     * Only dependencies matching none of the patterns will be considered.
     * The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.
     *
     * @since 2.14.0
     */
    protected List<String> pluginManagementDependencyExcludes = emptyList();

    /**
     * settings.xml's server id for the URL. This is used when wagon needs extra authentication information.
     *
     * @since 2.14.0
     */
    private String serverId;

    /**
     * URI of a ruleSet file containing the rules that control how to compare
     * version numbers. The URI could be either a Wagon URI or a classpath URI
     * (e.g. <code>classpath:///package/sub/package/rules.xml</code>).
     *
     * @since 2.14.0
     */
    private String rulesUri;

    /**
     * <p>Allows specifying the {@linkplain RuleSet} object describing rules
     * on artifact versions to ignore when considering updates.</p>
     *
     * @see <a href="https://www.mojohaus.org/versions/versions-maven-plugin/version-rules.html#Using_the_ruleSet_element_in_the_POM">
     *     Using the ruleSet element in the POM</a>
     *
     * @since 2.14.0
     */
    protected RuleSet ruleSet;

    /**
     * Whether snapshots should be counted as updates. Default is {@code false}.
     *
     * @since 2.14.2
     */
    protected boolean allowSnapshots;

    private final MavenProject project;

    private final RepositorySystem repositorySystem;

    private final Map<String, Wagon> wagonMap;

    private final MavenSession mavenSession;

    private final MojoExecution mojoExecution;

    @Inject
    public MaxDependencyUpdates(
            MavenProject project,
            ArtifactFactory artifactFactory,
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            MavenSession mavenSession,
            MojoExecution mojoExecution) {
        this.project = project;
        this.artifactFactory = artifactFactory;
        this.artifactHandlerManager = artifactHandlerManager;
        this.repositorySystem = repositorySystem;
        this.wagonMap = wagonMap;
        this.mavenSession = mavenSession;
        this.mojoExecution = mojoExecution;
    }

    /**
     * Creates the VersionsHelper object
     * @return VersionsHelper object
     */
    private VersionsHelper createVersionsHelper(String serverId, String rulesUri, RuleSet ruleSet)
            throws EnforcerRuleError {
        try {
            Log log = new PluginLogWrapper(getLog());
            RuleService ruleService = new RulesServiceBuilder()
                    .withWagonMap(wagonMap)
                    .withServerId(serverId)
                    .withRulesUri(rulesUri)
                    .withRuleSet(ruleSet)
                    .withIgnoredVersions(null)
                    .withLog(log)
                    .withMavenSession(mavenSession)
                    .build();
            PomHelper pomHelper =
                    new PomHelper(artifactFactory, new VersionsExpressionEvaluator(mavenSession, mojoExecution));
            return new DefaultVersionsHelper.Builder()
                    .withArtifactCreationService(artifactFactory)
                    .withRepositorySystem(repositorySystem)
                    .withLog(log)
                    .withMavenSession(mavenSession)
                    .withPomHelper(pomHelper)
                    .withRuleService(ruleService)
                    .build();
        } catch (MojoExecutionException e) {
            throw new EnforcerRuleError("Cannot resolve dependency", e);
        }
    }

    @Override
    public void execute() throws EnforcerRuleException {

        PluginLogWrapper pluginLog = new PluginLogWrapper(getLog());

        VersionsHelper versionsHelper =
                createVersionsHelper(serverId != null ? serverId : "serverId", rulesUri, ruleSet);

        Set<Dependency> dependencies = new TreeSet<>(DependencyComparator.INSTANCE);
        if (processDependencyManagement) {
            try {
                dependencies.addAll(filterDependencies(
                        extractDependenciesFromDependencyManagement(
                                project, processDependencyManagementTransitive, pluginLog),
                        dependencyManagementIncludes,
                        dependencyManagementExcludes,
                        "Dependency Management",
                        pluginLog));
            } catch (VersionRetrievalException e) {
                throw new EnforcerRuleError(e.getMessage());
            }
        }
        if (processPluginDependencies) {
            dependencies.addAll(filterDependencies(
                    extractDependenciesFromPlugins(project),
                    pluginDependencyIncludes,
                    pluginDependencyExcludes,
                    "Plugin Dependencies",
                    pluginLog));
        }
        if (processPluginDependenciesInPluginManagement) {
            dependencies.addAll(filterDependencies(
                    extractPluginDependenciesFromPluginsInPluginManagement(project),
                    pluginManagementDependencyIncludes,
                    pluginManagementDependencyExcludes,
                    "Plugin Management Dependencies",
                    pluginLog));
        }
        if (processDependencies) {
            dependencies.addAll(filterDependencies(
                    project.getDependencies(), dependencyIncludes, dependencyExcludes, "Dependencies", pluginLog));
        }
        try {
            Optional<Segment> ignoredSegment = ignoreSubIncrementalUpdates
                    ? of(SUBINCREMENTAL)
                    : ignoreIncrementalUpdates ? of(INCREMENTAL) : ignoreMinorUpdates ? of(MINOR) : empty();
            List<ArtifactVersions> upgradable = versionsHelper
                    .lookupDependenciesUpdates(
                            dependencies.stream().filter(d -> d.getVersion() != null), false, allowSnapshots)
                    .values()
                    .stream()
                    .filter(v -> v.getVersions(v.restrictionForIgnoreScope(v.getCurrentVersion(), ignoredSegment), true)
                                    .length
                            > 0)
                    .collect(Collectors.toList());
            if (upgradable.size() > maxUpdates) {
                throw new EnforcerRuleException("More than " + maxUpdates + " upgradable artifacts detected: "
                        + upgradable.stream()
                                .map(av -> av.getArtifact() + " -> ["
                                        + Arrays.stream(av.getVersions(allowSnapshots))
                                                .map(ArtifactVersion::toString)
                                                .collect(Collectors.joining(", "))
                                        + "]")
                                .collect(Collectors.joining(", ")));
            }
        } catch (VersionRetrievalException e) {
            throw new EnforcerRuleError(e);
        }
    }
}
