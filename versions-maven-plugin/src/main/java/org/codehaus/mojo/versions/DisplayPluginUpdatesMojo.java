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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.interpolation.ModelInterpolator;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.eclipse.aether.RepositorySystem;

import static java.util.Optional.ofNullable;

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
    public static final Pattern PATTERN_PROJECT_PLUGIN = PluginUpdatesDiscovery.PATTERN_PROJECT_PLUGIN;

    /**
     * The path to the super-pom.
     */
    public static final String SUPERPOM_PATH = PluginUpdatesDiscovery.SUPERPOM_PATH;

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
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
        this.projectBuilder = projectBuilder;
        this.lifecycleExecutor = lifecycleExecutor;
        this.modelInterpolator = modelInterpolator;
        this.runtimeInformation = runtimeInformation;
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
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
        if (skip) {
            getLog().info("Skipping execution");
            return;
        }
        logInit();
        PluginUpdatesDiscovery.DeclaredPlugins declarations;
        try {
            declarations = new PluginUpdatesDiscovery(
                            getProject(), getLog(), lifecycleExecutor, modelInterpolator, processUnboundPlugins)
                    .declaredPlugins();
        } catch (XMLStreamException | TransformerException | IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
        Set<String> pluginsWithVersionsSpecified = declarations.specified;
        Map<String, String> superPomPluginManagement = declarations.defaults;
        Map<String, String> parentPlugins = declarations.parents;
        Set<Plugin> plugins = declarations.plugins;

        List<String> pluginUpdates = new ArrayList<>();
        List<String> pluginLockdowns = new ArrayList<>();
        ArtifactVersion curMavenVersion =
                ArtifactVersionService.getArtifactVersion(runtimeInformation.getMavenVersion());
        ArtifactVersion specMavenVersion = MinimalMavenBuildVersionFinder.find(getProject(), getLog());
        ArtifactVersion minMavenVersion = null;
        boolean superPomDrivingMinVersion = false;
        // if Maven prerequisite upgraded to a version, Map<plugin compact key, latest compatible plugin vesion>
        Map<ArtifactVersion, Map<String, String>> mavenUpgrades = new TreeMap<>();

        PluginUpdatesAnalyzer analyzer = new PluginUpdatesAnalyzer(
                artifactFactory,
                getHelper(),
                projectBuilder,
                session,
                getProject(),
                getLog(),
                curMavenVersion,
                getAllowSnapshots());

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
                PluginUpdateAnalysis analysis = analyzer.analyze(plugin, version);
                effectiveVersion = analysis.getEffectiveVersion();
                artifactVersion = analysis.getCompatibleVersion();
                ArtifactVersion requires = analysis.getRequiredMavenVersion();
                if (minMavenVersion == null || compare(minMavenVersion, requires) < 0) {
                    minMavenVersion = requires;
                }
                analysis.getMavenUpgrades().forEach((required, candidate) -> {
                    String key = compactKey(plugin.getGroupId(), plugin.getArtifactId());
                    String newer = candidate.toString();
                    String previous = analysis.getUpgradeFromVersions().get(required);
                    mavenUpgrades
                            .computeIfAbsent(required, k -> new LinkedHashMap<>())
                            .putIfAbsent(
                                    key,
                                    newer.equals(previous)
                                            ? pad(key, INFO_PAD_SIZE + getOutputLineWidthOffset(), newer)
                                            : pad(
                                                    key,
                                                    INFO_PAD_SIZE + getOutputLineWidthOffset(),
                                                    previous,
                                                    " -> ",
                                                    newer));
                });
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

    private boolean isMavenPluginProject() {
        return "maven-plugin".equals(getProject().getPackaging());
    }

    private String compactKey(String groupId, String artifactId) {
        return PomHelper.APACHE_MAVEN_PLUGINS_GROUPID.equals(groupId)
                // a core plugin... group id is not needed
                ? artifactId
                : groupId + ":" + artifactId;
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
}
