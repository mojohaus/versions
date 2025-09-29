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
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.TransformerException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.model.BuildBase;
import org.apache.maven.model.Extension;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
import org.apache.maven.model.Reporting;
import org.apache.maven.model.building.DefaultModelBuildingRequest;
import org.apache.maven.model.building.ModelBuildingRequest;
import org.apache.maven.model.building.ModelProblemCollector;
import org.apache.maven.model.building.ModelProblemCollectorRequest;
import org.apache.maven.model.interpolation.ModelInterpolator;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingResult;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.ExtensionUtils;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.emptyMap;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toMap;
import static javax.xml.stream.XMLStreamConstants.END_DOCUMENT;

/**
 * Displays all plugins that have newer versions available, taking care of Maven version prerequisites.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo(name = "display-plugin-updates", threadSafe = true)
public class DisplayPluginUpdatesMojo extends AbstractVersionsDisplayMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * The width to pad warn messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int WARN_PAD_SIZE = 65;

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 68;

    /**
     * String to flag a plugin version being forced by the super-pom.
     *
     * @since 1.0-alpha-1
     */
    private static final String FROM_SUPER_POM = "(from super-pom) ";

    /**
     * String to flag a plugin version being forced by a parent pom.
     */
    public static final Pattern PATTERN_PROJECT_PLUGIN = Pattern.compile(
            "/project(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin");

    /**
     * The path to the super-pom.
     */
    public static final String SUPERPOM_PATH = "org/apache/maven/model/pom-4.0.0.xml";

    /**
     * @since 1.0-alpha-1
     */
    private LifecycleExecutor lifecycleExecutor;

    /**
     * @since 1.0-alpha-3
     */
    private ModelInterpolator modelInterpolator;

    /**
     * (Injected) instance of {@link RuntimeInformation}
     *
     * @since 2.14.0
     */
    private final RuntimeInformation runtimeInformation;

    /**
     * The (injected) instance of {@link ProjectBuilder}
     *
     * @since 2.14.0
     */
    protected final ProjectBuilder projectBuilder;

    /**
     * <p>If set to {@code true}, will also display updates to plugins where no version is specified
     * in the current POM, but whose version is specified in the parent or the "superpom".</p>
     * <p>It might not always be possible to update these plugins,
     * thus the default value of this parameter is {@code false}</p>.
     *
     * @since 2.15.0
     */
    @Parameter(property = "processUnboundPlugins", defaultValue = "false")
    protected boolean processUnboundPlugins;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    // --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Creates a new instance.
     *
     * @param artifactFactory   an {@link ArtifactFactory} instance
     * @param repositorySystem  a {@link RepositorySystem} instance
     * @param projectBuilder    a {@link ProjectBuilder} instance
     * @param wagonMap          a map of wagon providers per protocol
     * @param lifecycleExecutor the (injected) instance of {@link LifecycleExecutor}
     * @param modelInterpolator the (injected) instance of {@link ModelInterpolator}
     * @param runtimeInformation the (injected) instance of {@link RuntimeInformation}
     * @param changeRecorders   a map of change recorders
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    @SuppressWarnings("checkstyle:ParameterNumber")
    public DisplayPluginUpdatesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            ProjectBuilder projectBuilder,
            Map<String, Wagon> wagonMap,
            LifecycleExecutor lifecycleExecutor,
            ModelInterpolator modelInterpolator,
            RuntimeInformation runtimeInformation,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
        this.projectBuilder = projectBuilder;
        this.lifecycleExecutor = lifecycleExecutor;
        this.modelInterpolator = modelInterpolator;
        this.runtimeInformation = runtimeInformation;
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * Returns the pluginManagement section of the super-pom.
     *
     * @return Returns the pluginManagement section of the super-pom.
     */
    private Map<String, String> getSuperPomPluginManagement() {
        // we need to provide a copy with the version blanked out so that inferring from super-pom
        // works as for 2.x as 3.x fills in the version on us!
        Map<String, String> result =
                lifecycleExecutor
                        .getPluginsBoundByDefaultToAllLifecycles(getProject().getPackaging())
                        .stream()
                        .collect(LinkedHashMap::new, (m, p) -> m.put(p.getKey(), p.getVersion()), Map::putAll);

        try (InputStream superPomIs = getClass().getClassLoader().getResourceAsStream(SUPERPOM_PATH)) {
            Objects.requireNonNull(superPomIs);
            MutableXMLStreamReader pomReader = new MutableXMLStreamReader(superPomIs, Paths.get(SUPERPOM_PATH));

            Stack<StackState> pathStack = new Stack<>();
            StackState curState = new StackState("");

            for (int event = pomReader.getEventType();
                    event != END_DOCUMENT && pomReader.hasNext();
                    event = pomReader.next()) {
                if (pomReader.isStartElement()) {
                    if (curState != null) {
                        String elementName = pomReader.getLocalName();
                        if (PATTERN_PROJECT_PLUGIN.matcher(curState.path).matches()) {
                            switch (elementName) {
                                case "groupId":
                                    curState.groupId =
                                            pomReader.getElementText().trim();
                                    break;
                                case "artifactId":
                                    curState.artifactId =
                                            pomReader.getElementText().trim();
                                    break;
                                case "version":
                                    curState.version =
                                            pomReader.getElementText().trim();
                                    break;
                                default:
                                    break;
                            }
                        }
                        pathStack.push(curState);
                        curState = new StackState(curState.path + "/" + elementName);
                    }
                } else if (pomReader.isEndElement()) {
                    if (curState != null
                            && curState.artifactId != null
                            && PATTERN_PROJECT_PLUGIN.matcher(curState.path).matches()) {
                        result.putIfAbsent(
                                Plugin.constructKey(
                                        curState.groupId == null
                                                ? PomHelper.APACHE_MAVEN_PLUGINS_GROUPID
                                                : curState.groupId,
                                        curState.artifactId),
                                curState.version);
                    }
                    curState = pathStack.pop();
                }
            }
        } catch (IOException | XMLStreamException e) {
            // ignore
        }

        return result;
    }

    /**
     * Gets the plugin management plugins of a specific project.
     *
     * @param model the model to get the plugin management plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getPluginManagement(Model model) {
        // we want only those parts of pluginManagement that are defined in this project
        Map<String, String> pluginManagement = new HashMap<>();
        try {
            for (Plugin plugin : model.getBuild().getPluginManagement().getPlugins()) {
                String coord = plugin.getKey();
                String version = plugin.getVersion();
                if (version != null) {
                    pluginManagement.put(coord, version);
                }
            }
        } catch (NullPointerException e) {
            // guess there are no plugins here
        }
        try {
            for (Profile profile : model.getProfiles()) {
                try {
                    for (Plugin plugin :
                            profile.getBuild().getPluginManagement().getPlugins()) {
                        String coord = plugin.getKey();
                        String version = plugin.getVersion();
                        if (version != null) {
                            pluginManagement.put(coord, version);
                        }
                    }
                } catch (NullPointerException e) {
                    // guess there are no plugins here
                }
            }
        } catch (NullPointerException e) {
            // guess there are no profiles here
        }

        return pluginManagement;
    }

    // ------------------------ INTERFACE METHODS ------------------------

    // --------------------- Interface Mojo ---------------------

    /**
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @see AbstractVersionsUpdaterMojo#execute()
     * @since 1.0-alpha-1
     */
    @SuppressWarnings("checkstyle:MethodLength")
    public void execute() throws MojoExecutionException, MojoFailureException {
        logInit();
        Set<String> pluginsWithVersionsSpecified;
        try (MutableXMLStreamReader pomReader =
                new MutableXMLStreamReader(getProject().getFile().toPath())) {
            pluginsWithVersionsSpecified = findPluginsWithVersionsSpecified(pomReader);
        } catch (XMLStreamException | IOException | TransformerException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }

        Map<String, String> superPomPluginManagement = getSuperPomPluginManagement();
        getLog().debug("superPom plugins = " + superPomPluginManagement);

        List<MavenProject> parents = getParentProjects(getProject());
        Map<String, String> parentPlugins = getParentsPlugins(parents);
        // TODO remove, not used any more (found while extracting getParentsPlugins method and
        //      renaming parentPluginManagement to parentPlugins)
        // NOTICE: getPluginManagementPlugins() takes profiles while getParentPlugins does not
        //         there is probably a little inconsistency (if plugins configured in profiles of parents)
        Map<String, String> parentBuildPlugins = new HashMap<>();
        Map<String, String> parentReportPlugins = new HashMap<>();

        Set<Plugin> plugins;
        try {
            plugins = getPluginManagementPlugins(
                    superPomPluginManagement,
                    parentPlugins,
                    parentBuildPlugins,
                    parentReportPlugins,
                    pluginsWithVersionsSpecified);
        } catch (XMLStreamException | TransformerException | IOException e) {
            throw new MojoFailureException(e.getMessage(), e);
        }

        List<String> pluginUpdates = new ArrayList<>();
        List<String> pluginLockdowns = new ArrayList<>();
        ArtifactVersion curMavenVersion =
                ArtifactVersionService.getArtifactVersion(runtimeInformation.getMavenVersion());
        ArtifactVersion specMavenVersion = MinimalMavenBuildVersionFinder.find(getProject(), getLog());
        ArtifactVersion minMavenVersion = null;
        boolean superPomDrivingMinVersion = false;
        // if Maven prerequisite upgraded to a version, Map<plugin compact key, latest compatible plugin vesion>
        Map<ArtifactVersion, Map<String, String>> mavenUpgrades = new TreeMap<>();

        for (Plugin plugin : plugins) {
            String coords = ArtifactUtils.versionlessKey(plugin.getGroupId(), plugin.getArtifactId());
            String version = ofNullable(plugin.getVersion()).orElse(parentPlugins.get(coords));

            boolean versionSpecifiedInCurrentPom = pluginsWithVersionsSpecified.contains(coords);
            if (!versionSpecifiedInCurrentPom && !processUnboundPlugins && parentPlugins.containsKey(coords)) {
                getLog().debug("Skip " + coords + ", version " + version + " is defined in parent POM.");
                getLog().debug("Use the \"processUnboundPlugins\" parameter to see these updates.");
                continue;
            }

            getLog().debug("Checking " + coords + " for updates newer than " + version);
            String effectiveVersion;
            ArtifactVersion artifactVersion;
            try {
                // now we want to find the newest versions and check their Maven version prerequisite
                Pair<ArtifactVersion, String> effectiveVersionPair =
                        getEffectivePluginVersion(plugin, version, specMavenVersion, curMavenVersion, mavenUpgrades);
                effectiveVersion = effectiveVersionPair.getRight();
                artifactVersion = effectiveVersionPair.getLeft();
                if (effectiveVersion != null) {
                    try {
                        ArtifactVersion requires = getPrerequisitesMavenVersion(
                                getPluginProject(plugin.getGroupId(), plugin.getArtifactId(), effectiveVersion));
                        if (minMavenVersion == null || compare(minMavenVersion, requires) < 0) {
                            minMavenVersion = requires;
                        }
                    } catch (ArtifactResolutionException | ProjectBuildingException e) {
                        // ignore bad version
                    }
                }
            } catch (VersionRetrievalException e) {
                throw new MojoExecutionException(e.getMessage(), e);
            }

            String newVersion;

            if (version == null && versionSpecifiedInCurrentPom) {
                // Hack ALERT!
                //
                // All this should be re-written in a less "pom is xml" way... but it'll
                // work for now :-(
                //
                // we have removed the version information, as it was the same as from
                // the super-pom... but it actually was specified.
                version = artifactVersion != null ? artifactVersion.toString() : null;
            }

            if (getLog().isDebugEnabled()) {
                getLog().debug("[" + coords + "].version=" + version);
                getLog().debug("[" + coords + "].artifactVersion=" + artifactVersion);
                getLog().debug("[" + coords + "].effectiveVersion=" + effectiveVersion);
                getLog().debug("[" + coords + "].specified=" + versionSpecifiedInCurrentPom);
            }
            if (version == null || !processUnboundPlugins && !versionSpecifiedInCurrentPom) {
                version = superPomPluginManagement.get(coords);
                if (getLog().isDebugEnabled()) {
                    getLog().debug("[" + coords + "].superPom.version=" + version);
                }

                newVersion = artifactVersion != null
                        ? artifactVersion.toString()
                        : (version != null ? version : (effectiveVersion != null ? effectiveVersion : "(unknown)"));
                if (version != null) {
                    superPomDrivingMinVersion = true;
                }

                pluginLockdowns.add(pad(
                        compactKey(plugin.getGroupId(), plugin.getArtifactId()),
                        WARN_PAD_SIZE + getOutputLineWidthOffset(),
                        superPomDrivingMinVersion ? FROM_SUPER_POM : "",
                        newVersion));
            } else if (artifactVersion != null) {
                newVersion = artifactVersion.toString();
            } else {
                newVersion = null;
            }
            if (version != null
                    && artifactVersion != null
                    && newVersion != null
                    && effectiveVersion != null
                    && ArtifactVersionService.getArtifactVersion(effectiveVersion)
                                    .compareTo(ArtifactVersionService.getArtifactVersion(newVersion))
                            < 0) {
                pluginUpdates.add(pad(
                        compactKey(plugin.getGroupId(), plugin.getArtifactId()),
                        INFO_PAD_SIZE + getOutputLineWidthOffset(),
                        effectiveVersion,
                        " -> ",
                        newVersion));
            }
        }

        // info on each plugin gathered: now it's time to display the result!
        //
        logLine(false, "");

        // updates keeping currently defined Maven version minimum
        if (pluginUpdates.isEmpty()) {
            logLine(false, "All plugins with a version specified are using the latest versions.");
        } else {
            logLine(false, "The following plugin updates are available:");
            for (String update : new TreeSet<>(pluginUpdates)) {
                logLine(false, update);
            }
        }
        logLine(false, "");

        // has every plugin a specified version?
        if (pluginLockdowns.isEmpty()) {
            logLine(false, "All plugins have a version specified.");
        } else {
            getLog().warn("The following plugins do not have their version specified:");
            for (String lockdown : new TreeSet<>(pluginLockdowns)) {
                getLog().warn(lockdown);
            }
        }
        logLine(false, "");

        // information on minimum Maven version
        if (specMavenVersion == null) {
            getLog().warn("Project does not define minimum Maven version required for build");
        } else {
            logLine(false, "Project requires minimum Maven version for build of: " + specMavenVersion);
        }
        logLine(false, "Plugins require minimum Maven version of: " + minMavenVersion);
        if (superPomDrivingMinVersion) {
            logLine(false, "Note: the super-pom from Maven " + curMavenVersion + " defines some of the plugin");
            logLine(false, "      versions and may be influencing the plugins required minimum Maven");
            logLine(false, "      version.");
        }
        logLine(false, "");

        if (isMavenPluginProject()) {
            if (specMavenVersion == null) {
                getLog().warn("Project (which is a Maven plugin) does not define required minimum version of Maven.");
                getLog().warn("Update the pom.xml to contain");
                getLog().warn("    <prerequisites>");
                getLog().warn("      <maven><!-- minimum version of Maven that the plugin works with --></maven>");
                getLog().warn("    </prerequisites>");
                getLog().warn("To build this plugin you need at least Maven " + minMavenVersion);
                getLog().warn("A Maven Enforcer rule can be used to enforce this if you have not already set one up");
                getLog().warn("See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html");
            } else if (minMavenVersion != null && compare(specMavenVersion, minMavenVersion) < 0) {
                getLog().warn("Project (which is a Maven plugin) targets Maven " + specMavenVersion + " or newer");
                getLog().warn("but requires Maven " + minMavenVersion + " or newer to build.");
                getLog().warn("This may or may not be a problem. A Maven Enforcer rule can help ");
                getLog().warn("enforce that the correct version of Maven is used to build this plugin.");
                getLog().warn("See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html");
            } else {
                logLine(false, "No plugins require a newer version of Maven than specified by the pom.");
            }
        } else {
            if (specMavenVersion == null) {
                logLine(true, "Project does not define required minimum version of Maven.");
                logLine(true, "Update the pom.xml to contain maven-enforcer-plugin to");
                logLine(true, "force the Maven version which is needed to build this project.");
                logLine(true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html");
                logLine(true, "Using the minimum version of Maven: " + minMavenVersion);
            } else if (minMavenVersion != null && compare(specMavenVersion, minMavenVersion) < 0) {
                logLine(true, "Project requires an incorrect minimum version of Maven.");
                logLine(true, "Update the pom.xml to contain maven-enforcer-plugin to");
                logLine(true, "force the Maven version which is needed to build this project.");
                logLine(true, "See https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html");
                logLine(true, "Using the minimum version of Maven: " + specMavenVersion);
            } else {
                logLine(false, "No plugins require a newer version of Maven than specified by the pom.");
            }
        }

        // updates if minimum Maven version is changed
        for (Map.Entry<ArtifactVersion, Map<String, String>> mavenUpgrade : mavenUpgrades.entrySet()) {
            ArtifactVersion mavenUpgradeVersion = mavenUpgrade.getKey();
            Map<String, String> upgradePlugins = mavenUpgrade.getValue();
            if (upgradePlugins.isEmpty() || compare(mavenUpgradeVersion, specMavenVersion) < 0) {
                continue;
            }
            logLine(false, "");
            logLine(false, "Require Maven " + mavenUpgradeVersion + " to use the following plugin updates:");
            for (Map.Entry<String, String> entry : upgradePlugins.entrySet()) {
                logLine(false, entry.getValue());
            }
        }
        logLine(false, "");
    }

    private Pair<ArtifactVersion, String> getEffectivePluginVersion(
            Plugin plugin,
            String effectiveVersion,
            ArtifactVersion specMavenVersion,
            ArtifactVersion curMavenVersion,
            Map<ArtifactVersion, Map<String, String>> mavenUpgrades)
            throws MojoExecutionException, VersionRetrievalException {
        Artifact artifactRange = artifactFactory.createMavenPluginArtifact(
                plugin.getGroupId(), plugin.getArtifactId(), effectiveVersion);
        ArtifactVersion[] newerVersions =
                getHelper().lookupArtifactVersions(artifactRange, true).getVersions(this.allowSnapshots);
        ArtifactVersion minRequires = null;
        ArtifactVersion artifactVersion = null;
        for (int j = newerVersions.length - 1; j >= 0; j--) {
            try {
                ArtifactVersion pluginRequires = getPrerequisitesMavenVersion(
                        getPluginProject(plugin.getGroupId(), plugin.getArtifactId(), newerVersions[j].toString()));
                if (artifactVersion == null && compare(specMavenVersion, pluginRequires) >= 0) {
                    // ok, newer version compatible with current specMavenVersion
                    artifactVersion = newerVersions[j];
                }
                if (effectiveVersion == null && compare(curMavenVersion, pluginRequires) >= 0) {
                    // version was unspecified, current version of maven thinks it should use this
                    effectiveVersion = newerVersions[j].toString();
                }
                if (artifactVersion != null && effectiveVersion != null) {
                    // no need to look at any older versions: latest compatible found
                    break;
                }
                // newer version not compatible with current specMavenVersion: track opportunity if Maven spec
                // upgrade
                if (minRequires == null || compare(minRequires, pluginRequires) > 0) {
                    Map<String, String> upgradePlugins =
                            mavenUpgrades.computeIfAbsent(pluginRequires, k -> new LinkedHashMap<>());

                    String upgradePluginKey = compactKey(plugin.getGroupId(), plugin.getArtifactId());
                    if (!upgradePlugins.containsKey(upgradePluginKey)) {
                        String newer = newerVersions[j].toString();
                        if (newer.equals(effectiveVersion)) {
                            // plugin version configured that require a Maven version higher than spec
                            upgradePlugins.put(
                                    upgradePluginKey,
                                    pad(upgradePluginKey, INFO_PAD_SIZE + getOutputLineWidthOffset(), newer));
                        } else {
                            // plugin that can be upgraded
                            upgradePlugins.put(
                                    upgradePluginKey,
                                    pad(
                                            upgradePluginKey,
                                            INFO_PAD_SIZE + getOutputLineWidthOffset(),
                                            effectiveVersion,
                                            " -> ",
                                            newer));
                        }
                    }
                    minRequires = pluginRequires;
                }
            } catch (ArtifactResolutionException | ProjectBuildingException e) {
                // ignore bad version
            }
        }
        return new ImmutablePair<>(artifactVersion, effectiveVersion);
    }

    /**
     * Builds a {@link MavenProject} instance for the plugin with a given {@code groupId},
     * {@code artifactId}, and {@code version}.
     *
     * @param groupId    {@code groupId} of the plugin
     * @param artifactId {@code artifactId} of the plugin
     * @param version    {@code version} of the plugin
     * @return retrieved {@link MavenProject} instance for the given plugin
     * @throws MojoExecutionException   thrown if the artifact for the plugin could not be constructed
     * @throws ProjectBuildingException thrown if the {@link MavenProject} instance could not be constructed
     */
    private MavenProject getPluginProject(String groupId, String artifactId, String version)
            throws MojoExecutionException, ProjectBuildingException, ArtifactResolutionException {
        Artifact probe = artifactFactory.createArtifact(DependencyBuilder.newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version)
                .withType("pom")
                .withScope(Artifact.SCOPE_RUNTIME)
                .build());
        getHelper().resolveArtifact(probe, true);
        ProjectBuildingResult result = projectBuilder.build(
                probe,
                true,
                PomHelper.createProjectBuilderRequest(
                        session,
                        r -> r.setProcessPlugins(false),
                        r -> r.setRemoteRepositories(session.getCurrentProject().getRemoteArtifactRepositories()),
                        r -> r.setPluginArtifactRepositories(
                                session.getCurrentProject().getPluginArtifactRepositories())));
        if (!result.getProblems().isEmpty()) {
            getLog().warn("Problems encountered during construction of the plugin POM for " + probe.toString());
            result.getProblems().forEach(p -> getLog().warn("\t" + p.getMessage()));
        }
        return result.getProject();
    }

    private static String pad(String start, int len, String... ends) {
        StringBuilder buf = new StringBuilder(len).append("  ").append(start).append(' ');
        int padding = len
                - Arrays.stream(ends)
                        .map(String::valueOf)
                        .map(String::length)
                        .reduce(Integer::sum)
                        .orElse(0);
        IntStream.range(0, padding - buf.length()).forEach(ignored -> buf.append('.'));
        buf.append(' ');
        Arrays.stream(ends).forEach(buf::append);
        return buf.toString();
    }

    private Map<String, String> getParentsPlugins(List<MavenProject> parents) throws MojoExecutionException {
        Map<String, String> parentPlugins = new HashMap<>();
        for (MavenProject parentProject : parents) {
            getLog().debug("Processing parent: " + parentProject.getGroupId() + ":" + parentProject.getArtifactId()
                    + ":" + parentProject.getVersion() + " -> " + parentProject.getFile());

            StringWriter writer = new StringWriter();
            boolean havePom = false;
            Model interpolatedModel;

            Model originalModel = parentProject.getOriginalModel();
            if (originalModel == null) {
                getLog().warn("project.getOriginalModel()==null for  " + parentProject.getGroupId() + ":"
                        + parentProject.getArtifactId() + ":" + parentProject.getVersion()
                        + " is null, substituting project.getModel()");
                originalModel = parentProject.getModel();
            }
            try {
                new MavenXpp3Writer().write(writer, originalModel);
                writer.close();
                havePom = true;
            } catch (IOException e) {
                // ignore
            }
            ModelBuildingRequest modelBuildingRequest = new DefaultModelBuildingRequest();
            modelBuildingRequest.setUserProperties(getProject().getProperties());
            interpolatedModel = modelInterpolator.interpolateModel(
                    originalModel, null, modelBuildingRequest, new IgnoringModelProblemCollector());
            if (havePom) {
                try (ByteArrayInputStream bais =
                        new ByteArrayInputStream(writer.toString().getBytes())) {
                    try (MutableXMLStreamReader pomReader = new MutableXMLStreamReader(
                            bais,
                            ofNullable(parentProject.getFile())
                                    .map(File::toPath)
                                    .orElse(null))) {
                        Set<String> withVersionSpecified = findPluginsWithVersionsSpecified(pomReader);

                        Map<String, String> map = getPluginManagement(interpolatedModel);
                        map.keySet().retainAll(withVersionSpecified);
                        parentPlugins.putAll(map);

                        map = getBuildPlugins(interpolatedModel, true);
                        map.keySet().retainAll(withVersionSpecified);
                        parentPlugins.putAll(map);

                        map = getReportPlugins(interpolatedModel, true);
                        map.keySet().retainAll(withVersionSpecified);
                        parentPlugins.putAll(map);
                    }
                } catch (XMLStreamException | TransformerException | IOException e) {
                    throw new MojoExecutionException(e.getMessage(), e);
                }
            } else {
                parentPlugins.putAll(getPluginManagement(interpolatedModel));
                parentPlugins.putAll(getBuildPlugins(interpolatedModel, true));
                parentPlugins.putAll(getReportPlugins(interpolatedModel, true));
            }
        }
        return parentPlugins;
    }

    private boolean isMavenPluginProject() {
        return "maven-plugin".equals(getProject().getPackaging());
    }

    private String compactKey(String groupId, String artifactId) {
        return PomHelper.APACHE_MAVEN_PLUGINS_GROUPID.equals(groupId)
                // a core plugin... group id is not needed
                ? artifactId
                : groupId + ":" + artifactId;
    }

    private static final class StackState {
        private final String path;

        private String groupId;

        private String artifactId;

        private String version;

        StackState(String path) {
            this.path = path;
        }

        public String toString() {
            return path + "[groupId=" + groupId + ", artifactId=" + artifactId + ", version=" + version + "]";
        }
    }

    /**
     * Returns a set of Strings which correspond to the plugin coordinates where there is a version specified.
     *
     * @param pom a {@link MutableXMLStreamReader} instance for the pom project to get the plugins with versions
     *            specified.
     * @return a set of Strings which correspond to the plugin coordinates where there is a version specified.
     */
    private Set<String> findPluginsWithVersionsSpecified(MutableXMLStreamReader pom)
            throws XMLStreamException, IOException, TransformerException {
        Set<String> result = new HashSet<>();
        Stack<StackState> pathStack = new Stack<>();
        StackState curState = new StackState("");

        while (pom.hasNext()) {
            pom.next();
            if (pom.isStartElement()) {
                if (curState != null
                        && PATTERN_PROJECT_PLUGIN.matcher(curState.path).matches()) {
                    if ("groupId".equals(pom.getLocalName())) {
                        curState.groupId = pom.getElementText().trim();
                        continue;
                    } else if ("artifactId".equals(pom.getLocalName())) {
                        curState.artifactId = pom.getElementText().trim();
                        continue;

                    } else if ("version".equals(pom.getLocalName())) {
                        curState.version = pom.getElementText().trim();
                        continue;
                    }
                }

                assert curState != null;
                pathStack.push(curState);
                curState = new StackState(curState.path + "/" + pom.getLocalName());
            }
            // for empty elements, pom can be both start- and end element
            if (pom.isEndElement()) {
                if (curState != null
                        && PATTERN_PROJECT_PLUGIN.matcher(curState.path).matches()) {
                    if (curState.artifactId != null && curState.version != null) {
                        if (curState.groupId == null) {
                            curState.groupId = PomHelper.APACHE_MAVEN_PLUGINS_GROUPID;
                        }
                        result.add(curState.groupId + ":" + curState.artifactId);
                    }
                }
                curState = pathStack.pop();
            }
        }

        return result;
    }

    // -------------------------- OTHER METHODS --------------------------

    /**
     * Get the minimum required Maven version of the given plugin
     * Same logic as in
     *
     * @param pluginProject the plugin for which to retrieve the minimum Maven version which is required
     * @return The minimally required Maven version (never {@code null})
     * @see <a
     *         href="https://github.com/apache/maven-plugin-tools/blob/c8ddcdcb10d342a5a5e2f38245bb569af5730c7c/maven-plugin-plugin/src/main/java/org/apache/maven/plugin/plugin/PluginReport.java#L711">PluginReport</a>
     */
    private ArtifactVersion getPrerequisitesMavenVersion(MavenProject pluginProject) {
        return ofNullable(pluginProject.getPrerequisites())
                .map(Prerequisites::getMaven)
                .map(ArtifactVersionService::getArtifactVersion)
                .orElse(null);
    }

    /**
     * Retrieves plugins from the given model
     *
     * @param build                build
     * @param onlyIncludeInherited {@code true} to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return map of plugin name x version
     */
    private Map<String, String> getPluginsFromBuild(BuildBase build, boolean onlyIncludeInherited) {
        return ofNullable(build)
                .flatMap(b -> ofNullable(b.getPlugins()).map(plugins -> plugins.stream()
                        .filter(plugin -> plugin.getVersion() != null)
                        .filter(plugin -> !onlyIncludeInherited || getPluginInherited(plugin))
                        .collect(toMap(Plugin::getKey, Plugin::getVersion))))
                .orElse(emptyMap());
    }

    /**
     * Gets the build plugins of a specific project.
     *
     * @param model                the model to get the build plugins from.
     * @param onlyIncludeInherited {@code true} to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getBuildPlugins(Model model, boolean onlyIncludeInherited) {
        Map<String, String> buildPlugins = new HashMap<>(getPluginsFromBuild(model.getBuild(), onlyIncludeInherited));
        ofNullable(model.getProfiles()).ifPresent(profiles -> profiles.stream()
                .map(profile -> getPluginsFromBuild(profile.getBuild(), onlyIncludeInherited))
                .forEach(buildPlugins::putAll));
        return buildPlugins;
    }

    /**
     * Returns the Inherited of a {@link Plugin} or {@link ReportPlugin}
     *
     * @param plugin the {@link Plugin} or {@link ReportPlugin}
     * @return the Inherited of the {@link Plugin} or {@link ReportPlugin}
     * @since 1.0-alpha-1
     */
    private static boolean getPluginInherited(Object plugin) {
        return "true"
                .equalsIgnoreCase(
                        plugin instanceof ReportPlugin
                                ? ((ReportPlugin) plugin).getInherited()
                                : ((Plugin) plugin).getInherited());
    }

    /**
     * Gets the plugins that are bound to the defined phases. This does not find plugins bound in the pom to a phase
     * later than the plugin is executing.
     *
     * @param project the project
     * @return the bound plugins
     */
    // pilfered this from enforcer-rules
    // TODO coordinate with Brian Fox to remove the duplicate code
    private Stream<Plugin> getBoundPlugins(MavenProject project) {
        // we need to provide a copy with the version blanked out so that inferring from super-pom
        // works as for 2.x as 3.x fills in the version on us!
        return lifecycleExecutor.getPluginsBoundByDefaultToAllLifecycles(project.getPackaging()).stream()
                .map(p -> new Plugin() {
                    {
                        setGroupId(p.getGroupId());
                        setArtifactId(p.getArtifactId());
                    }
                });
    }

    /**
     * Returns all the parent projects of the specified project, with the root project first.
     *
     * @param project The maven project to get the parents of
     * @return the parent projects of the specified project, with the root project first.
     * @throws org.apache.maven.plugin.MojoExecutionException if the super-pom could not be created.
     * @since 1.0-alpha-1
     */
    private List<MavenProject> getParentProjects(MavenProject project) throws MojoExecutionException {
        List<MavenProject> parents = new ArrayList<>();
        while (project.getParent() != null) {
            project = project.getParent();
            parents.add(0, project);
        }
        return parents;
    }

    /**
     * Returns the set of all plugins used by the project.
     *
     * @param superPomPluginManagement     the super pom's pluginManagement plugins.
     * @param parentPluginManagement       the parent pom's pluginManagement plugins.
     * @param parentBuildPlugins           the parent pom's build plugins.
     * @param parentReportPlugins          the parent pom's report plugins.
     * @param pluginsWithVersionsSpecified the plugin coords that have a version defined in the project.
     * @return the set of plugins used by the project.
     */
    @SuppressWarnings("checkstyle:MethodLength")
    private Set<Plugin> getPluginManagementPlugins(
            Map<String, String> superPomPluginManagement,
            Map<String, String> parentPluginManagement,
            Map<String, String> parentBuildPlugins,
            Map<String, String> parentReportPlugins,
            Set<String> pluginsWithVersionsSpecified)
            throws XMLStreamException, IOException, TransformerException {

        getLog().debug("Building list of project plugins...");

        if (getLog().isDebugEnabled()) {
            try (OutputStream outputStream = new ByteArrayOutputStream()) {
                new MavenXpp3Writer().write(outputStream, getProject().getOriginalModel());
                getLog().debug("Original model:\n" + outputStream);
            } catch (IOException e) {
                getLog().debug(e);
            }
        }

        Set<Extension> extensions = Stream.concat(
                        ExtensionUtils.getCoreExtensions(getProject()),
                        ExtensionUtils.getBuildExtensions(getProject(), getLog(), false))
                .collect(Collectors.toSet());
        if (getLog().isDebugEnabled()) {
            getLog().debug("Extensions:"
                    + (extensions.isEmpty()
                            ? "(none)"
                            : extensions.stream()
                                    .map(e -> "\n\t" + e.getGroupId() + ":" + e.getArtifactId()
                                            + Optional.ofNullable(e.getVersion())
                                                    .map(v -> ":")
                                                    .orElse(""))
                                    .collect(Collectors.joining("\n"))));
        }

        ModelBuildingRequest modelBuildingRequest = new DefaultModelBuildingRequest();
        modelBuildingRequest.setUserProperties(getProject().getProperties());
        Model originalModel = modelInterpolator.interpolateModel(
                getProject().getOriginalModel(),
                getProject().getBasedir(),
                modelBuildingRequest,
                new IgnoringModelProblemCollector());

        Map<String, String> excludePluginManagement = new HashMap<>(superPomPluginManagement);
        excludePluginManagement.putAll(parentPluginManagement);
        debugVersionMap("super-pom version map", superPomPluginManagement);
        debugVersionMap("parent version map", parentPluginManagement);
        debugVersionMap("aggregate version map", excludePluginManagement);

        excludePluginManagement.keySet().removeAll(pluginsWithVersionsSpecified);
        debugVersionMap("final aggregate version map", excludePluginManagement);

        Map<String, Plugin> plugins = new HashMap<>();
        ofNullable(originalModel.getBuild())
                .map(DisplayPluginUpdatesMojo::getPluginManagementPlugins)
                .ifPresent(p -> mergePluginsMap(plugins, p, excludePluginManagement));
        debugPluginMap("after adding local pluginManagement", plugins);

        mergePluginsMap(plugins, getLifecyclePlugins(parentPluginManagement), parentPluginManagement);
        debugPluginMap("after adding lifecycle plugins", plugins);
        debugPluginMap("after adding lifecycle plugins", plugins);

        ofNullable(originalModel.getBuild())
                .map(b -> getBuildPlugins(b, parentPluginManagement, extensions))
                .ifPresent(p -> mergePluginsMap(plugins, p, parentPluginManagement));
        debugPluginMap("after adding build plugins", plugins);

        ofNullable(originalModel.getReporting())
                .map(r -> getReportingPlugins(r, parentPluginManagement))
                .ifPresent(p -> mergePluginsMap(plugins, p, parentReportPlugins));
        debugPluginMap("after adding reporting plugins", plugins);

        for (Profile profile : originalModel.getProfiles()) {
            if (getLog().isDebugEnabled()) {
                getLog().debug("Processing profile " + profile.getId());
            }
            ofNullable(profile.getBuild())
                    .map(DisplayPluginUpdatesMojo::getPluginManagementPlugins)
                    .ifPresent(p -> mergePluginsMap(plugins, p, excludePluginManagement));
            debugPluginMap("after adding profile " + profile.getId() + " pluginManagement", plugins);

            ofNullable(profile.getBuild())
                    .map(b -> getBuildPlugins(b, parentPluginManagement, extensions))
                    .ifPresent(p -> mergePluginsMap(plugins, p, parentBuildPlugins));
            debugPluginMap("after adding profile " + profile.getId() + " build plugins", plugins);

            ofNullable(profile.getReporting())
                    .map(r -> getReportingPlugins(r, parentPluginManagement))
                    .ifPresent(p -> mergePluginsMap(plugins, p, parentReportPlugins));
            debugPluginMap("after adding profile " + profile.getId() + " reporting plugins", plugins);
        }
        Set<Plugin> result = new TreeSet<>(PluginComparator.INSTANCE);
        result.addAll(plugins.values());
        return result;
    }

    private static Stream<Plugin> getReportingPlugins(Reporting reporting, Map<String, String> parentPluginManagement) {
        return reporting.getPlugins().stream()
                // removing plugins without a version
                // and with the parent also not defining it for them
                .filter(plugin -> plugin.getVersion() != null || parentPluginManagement.get(plugin.getKey()) == null)
                .map(DisplayPluginUpdatesMojo::toPlugin);
    }

    private static Stream<Plugin> getPluginManagementPlugins(BuildBase buildBase) {
        return ofNullable(buildBase.getPluginManagement())
                .map(PluginManagement::getPlugins)
                .map(Collection::stream)
                .orElse(Stream.empty());
    }

    private static Stream<Plugin> getBuildPlugins(
            BuildBase buildBase, Map<String, String> parentPluginManagement, Set<Extension> extensions) {
        return buildBase.getPlugins().stream()
                // removing plugins without a version
                // and with the parent also not defining it for them
                .filter(plugin -> plugin.getVersion() != null || parentPluginManagement.get(plugin.getKey()) == null)
                .filter(plugin -> extensions.stream()
                        .noneMatch(e -> Objects.equals(plugin.getGroupId(), e.getGroupId())
                                && Objects.equals(plugin.getArtifactId(), e.getArtifactId())
                                && ofNullable(e.getVersion())
                                        .map(v -> v.equals(plugin.getVersion()))
                                        .orElseGet(() -> Objects.isNull(plugin.getVersion()))));
    }

    private Stream<Plugin> getLifecyclePlugins(Map<String, String> parentPluginManagement) {
        return getBoundPlugins(getProject())
                .filter(Objects::nonNull)
                .filter(p -> p.getKey() != null)
                .filter(p -> p.getVersion() != null)
                .filter(p -> parentPluginManagement.get(p.getKey()) != null);
    }

    /**
     * Adds those project plugins which are not inherited from the parent definitions to the list of plugins.
     *
     * @param plugins           map of plugins to merge into
     * @param pluginsToMerge    plugins that need to be merged with the map
     * @param parentDefinitions The parent plugin definitions.
     * @since 1.0-alpha-1
     */
    private void mergePluginsMap(
            Map<String, Plugin> plugins, Stream<Plugin> pluginsToMerge, Map<String, String> parentDefinitions) {
        pluginsToMerge.forEach(plugin -> {
            plugins.compute(plugin.getKey(), (key, existingVal) -> {
                String versionFromParent = parentDefinitions.get(key);
                if (plugin.getVersion() == null
                        && (existingVal == null || existingVal.getVersion() == null)
                        && versionFromParent != null) {
                    // if the plugin is not present in plugins or if it is, it doesn't have a version,
                    // but the parent does have it -> take the version from parent
                    Plugin parentPlugin = new Plugin();
                    parentPlugin.setGroupId(plugin.getGroupId());
                    parentPlugin.setArtifactId(plugin.getArtifactId());
                    parentPlugin.setVersion(versionFromParent);
                    return parentPlugin;
                } else if ((versionFromParent == null || !versionFromParent.equals(plugin.getVersion()))
                        && (existingVal == null || existingVal.getVersion() == null)) {
                    // if parent doesn't contain the plugin key or its version differs from plugin version
                    // and currently stored version is either null or not there
                    return plugin;
                }
                // otherwise, put the new value in the map only if existingVal is null
                if (!processUnboundPlugins) {
                    return existingVal != null ? existingVal : plugin;
                } else {
                    return plugin.getVersion() != null ? plugin : existingVal;
                }
            });
        });
    }

    /**
     * Logs at debug level a map of plugins keyed by versionless key.
     *
     * @param description log description
     * @param plugins     a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                    values
     *                    being {@link Plugin} or {@link ReportPlugin}.
     */
    private void debugPluginMap(String description, Map<String, Plugin> plugins) {
        if (getLog().isDebugEnabled()) {
            Set<Plugin> sorted = new TreeSet<>(PluginComparator.INSTANCE);
            sorted.addAll(plugins.values());
            getLog().debug(sorted.stream()
                    .collect(
                            () -> new StringBuilder(description),
                            (s, e) -> s.append("\n    ")
                                    .append(e.getKey())
                                    .append(":")
                                    .append(e.getVersion()),
                            StringBuilder::append));
        }
    }

    /**
     * Logs at debug level a map of plugin versions keyed by versionless key.
     *
     * @param description    log description
     * @param pluginVersions a map with keys being the {@link String} corresponding to the versionless artifact key and
     *                       values
     *                       being {@link String} plugin version.
     */
    private void debugVersionMap(String description, Map<String, String> pluginVersions) {
        if (getLog().isDebugEnabled()) {
            getLog().debug(pluginVersions.entrySet().stream()
                    .collect(
                            () -> new StringBuilder(description),
                            (s, e) -> s.append("\n    ")
                                    .append(e.getKey())
                                    .append(":")
                                    .append(e.getValue()),
                            StringBuilder::append));
        }
    }

    private static Plugin toPlugin(ReportPlugin reportPlugin) {
        Plugin plugin = new Plugin();
        plugin.setGroupId(reportPlugin.getGroupId());
        plugin.setArtifactId(reportPlugin.getArtifactId());
        plugin.setVersion(reportPlugin.getVersion());
        return plugin;
    }

    private static Stream<Plugin> toPlugins(Collection<ReportPlugin> reportPlugins) {
        return reportPlugins.stream().map(DisplayPluginUpdatesMojo::toPlugin);
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param model                the model to get the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getReportPlugins(Model model, boolean onlyIncludeInherited) {
        return Stream.concat(
                        ofNullable(model.getReporting())
                                .map(Reporting::getPlugins)
                                .map(Collection::stream)
                                .orElse(Stream.empty()),
                        ofNullable(model.getProfiles())
                                .flatMap(profiles -> profiles.stream()
                                        .map(Profile::getReporting)
                                        .filter(Objects::nonNull)
                                        .map(Reporting::getPlugins)
                                        .map(Collection::stream)
                                        .reduce(Stream::concat))
                                .orElse(Stream.empty()))
                .filter(p -> p.getVersion() != null)
                .filter(p -> !onlyIncludeInherited || getPluginInherited(p))
                .collect(toMap(ReportPlugin::getKey, ReportPlugin::getVersion));
    }

    /**
     * @param pom the pom to update.
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     * @since 1.0-alpha-1
     */
    protected void update(MutableXMLStreamReader pom) {
        // do nothing
    }

    private static int compare(ArtifactVersion a, ArtifactVersion b) {
        return a == null ? b == null ? 0 : -1 : b == null ? 1 : a.compareTo(b);
    }

    private static class IgnoringModelProblemCollector implements ModelProblemCollector {

        @Override
        public void add(ModelProblemCollectorRequest req) {
            // ignore
        }
    }
}
