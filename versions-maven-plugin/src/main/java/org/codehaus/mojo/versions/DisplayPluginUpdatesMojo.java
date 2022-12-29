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
import javax.xml.stream.events.XMLEvent;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.model.BuildBase;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin;
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
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.plexus.util.IOUtil;
import org.codehaus.plexus.util.ReaderFactory;

import static java.util.Collections.emptyMap;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

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

    public static final String DEFAULT_MVN_VERSION = "3.2.5";

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

    // --------------------- GETTER / SETTER METHODS ---------------------

    @Inject
    @SuppressWarnings("checkstyle:ParameterNumber")
    public DisplayPluginUpdatesMojo(
            RepositorySystem repositorySystem,
            org.eclipse.aether.RepositorySystem aetherRepositorySystem,
            ProjectBuilder projectBuilder,
            Map<String, Wagon> wagonMap,
            LifecycleExecutor lifecycleExecutor,
            ModelInterpolator modelInterpolator,
            RuntimeInformation runtimeInformation,
            Map<String, ChangeRecorder> changeRecorders) {
        super(repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders);
        this.projectBuilder = projectBuilder;
        this.lifecycleExecutor = lifecycleExecutor;
        this.modelInterpolator = modelInterpolator;
        this.runtimeInformation = runtimeInformation;
    }

    /**
     * Returns the pluginManagement section of the super-pom.
     *
     * @return Returns the pluginManagement section of the super-pom.
     * @throws MojoExecutionException when things go wrong.
     */
    private Map<String, String> getSuperPomPluginManagement() throws MojoExecutionException {
        // we need to provide a copy with the version blanked out so that inferring from super-pom
        // works as for 2.x as 3.x fills in the version on us!
        Map<String, String> result =
                lifecycleExecutor
                        .getPluginsBoundByDefaultToAllLifecycles(getProject().getPackaging())
                        .stream()
                        .collect(LinkedHashMap::new, (m, p) -> m.put(p.getKey(), p.getVersion()), Map::putAll);

        URL superPom = getClass().getClassLoader().getResource("org/apache/maven/model/pom-4.0.0.xml");
        if (superPom != null) {
            try {
                try (Reader reader = ReaderFactory.newXmlReader(superPom)) {
                    StringBuilder buf = new StringBuilder(IOUtil.toString(reader));
                    ModifiedPomXMLEventReader pom = newModifiedPomXER(buf, superPom.toString());

                    Pattern pathRegex = Pattern.compile("/project(/profiles/profile)?"
                            + "((/build(/pluginManagement)?)|(/reporting))"
                            + "/plugins/plugin");
                    Stack<StackState> pathStack = new Stack<>();
                    StackState curState = null;
                    while (pom.hasNext()) {
                        XMLEvent event = pom.nextEvent();
                        if (event.isStartDocument()) {
                            curState = new StackState("");
                            pathStack.clear();
                        } else if (event.isStartElement()) {
                            String elementName =
                                    event.asStartElement().getName().getLocalPart();
                            if (curState != null
                                    && pathRegex.matcher(curState.path).matches()) {
                                if ("groupId".equals(elementName)) {
                                    curState.groupId = pom.getElementText().trim();
                                    continue;
                                } else if ("artifactId".equals(elementName)) {
                                    curState.artifactId = pom.getElementText().trim();
                                    continue;

                                } else if ("version".equals(elementName)) {
                                    curState.version = pom.getElementText().trim();
                                    continue;
                                }
                            }

                            pathStack.push(curState);
                            curState = new StackState(curState.path + "/" + elementName);
                        } else if (event.isEndElement()) {
                            if (curState != null
                                    && pathRegex.matcher(curState.path).matches()) {
                                if (curState.artifactId != null) {
                                    Plugin plugin = new Plugin();
                                    plugin.setArtifactId(curState.artifactId);
                                    plugin.setGroupId(
                                            curState.groupId == null
                                                    ? PomHelper.APACHE_MAVEN_PLUGINS_GROUPID
                                                    : curState.groupId);
                                    plugin.setVersion(curState.version);
                                    if (!result.containsKey(plugin.getKey())) {
                                        result.put(plugin.getKey(), plugin.getVersion());
                                    }
                                }
                            }
                            curState = pathStack.pop();
                        }
                    }
                }
            } catch (IOException | XMLStreamException e) {
                // ignore
            }
        }

        return result;
    }

    /**
     * Gets the plugin management plugins of a specific project.
     *
     * @param model the model to getModel the plugin management plugins from.
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
        try {
            MavenProject project1 = getProject();
            pluginsWithVersionsSpecified = findPluginsWithVersionsSpecified(
                    PomHelper.readXmlFile(project1.getFile()), getSafeProjectPathInfo(project1));
        } catch (XMLStreamException | IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }

        Map<String, String> superPomPluginManagement = getSuperPomPluginManagement();
        getLog().debug("superPom plugins = " + superPomPluginManagement);

        List<MavenProject> parents = getParentProjects(getProject());
        Map<String, String> parentPlugins = getParentsPlugins(parents);
        // TODO remove, not used any more (found while extracting getParentsPlugins method and
        //      renaming parentPluginManagement to parentPlugins)
        // NOTICE: getProjectPlugins() takes profiles while getParentPlugins does not
        //         there is probably a little inconsistency (if plugins configured in profiles of parents)
        Map<String, String> parentBuildPlugins = new HashMap<>();
        Map<String, String> parentReportPlugins = new HashMap<>();

        Set<Plugin> plugins = getProjectPlugins(
                superPomPluginManagement,
                parentPlugins,
                parentBuildPlugins,
                parentReportPlugins,
                pluginsWithVersionsSpecified);

        List<String> pluginUpdates = new ArrayList<>();
        List<String> pluginLockdowns = new ArrayList<>();
        ArtifactVersion curMavenVersion = new DefaultArtifactVersion(runtimeInformation.getMavenVersion());
        ArtifactVersion specMavenVersion =
                MinimalMavenBuildVersionFinder.find(getProject(), DEFAULT_MVN_VERSION, getLog());
        ArtifactVersion minMavenVersion = null;
        boolean superPomDrivingMinVersion = false;
        // if Maven prerequisite upgraded to a version, Map<plugin compact key, latest compatible plugin vesion>
        Map<ArtifactVersion, Map<String, String>> mavenUpgrades = new TreeMap<>(new MavenVersionComparator());

        for (Plugin plugin : plugins) {
            String groupId = plugin.getGroupId();
            String artifactId = plugin.getArtifactId();
            String version = plugin.getVersion();
            String coords = ArtifactUtils.versionlessKey(groupId, artifactId);

            if (version == null) {
                version = parentPlugins.get(coords);
            }

            boolean versionSpecifiedInCurrentPom = pluginsWithVersionsSpecified.contains(coords);
            if (!versionSpecifiedInCurrentPom && !processUnboundPlugins && parentPlugins.containsKey(coords)) {
                getLog().debug("Skip " + coords + ", version " + version + " is defined in parent POM.");
                getLog().debug("Use the \"processUnboundPlugins\" parameter to see these updates.");
                continue;
            }
            getLog().debug("Checking " + coords + " for updates newer than " + version);
            String effectiveVersion = version;

            Artifact artifactRange =
                    getHelper().createPluginArtifact(plugin.getGroupId(), plugin.getArtifactId(), version);

            ArtifactVersion artifactVersion = null;
            try {
                // now we want to find the newest versions and check their Maven version prerequisite
                ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions(artifactRange, true);
                ArtifactVersion[] newerVersions = artifactVersions.getVersions(this.allowSnapshots);
                ArtifactVersion minRequires = null;
                for (int j = newerVersions.length - 1; j >= 0; j--) {
                    try {
                        ArtifactVersion pluginRequires = getPrerequisitesMavenVersion(
                                getPluginProject(groupId, artifactId, newerVersions[j].toString()));
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

                            String upgradePluginKey = compactKey(groupId, artifactId);
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
                if (effectiveVersion != null) {
                    try {
                        ArtifactVersion requires =
                                getPrerequisitesMavenVersion(getPluginProject(groupId, artifactId, effectiveVersion));
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

            getLog().debug("[" + coords + "].version=" + version);
            getLog().debug("[" + coords + "].artifactVersion=" + artifactVersion);
            getLog().debug("[" + coords + "].effectiveVersion=" + effectiveVersion);
            getLog().debug("[" + coords + "].specified=" + versionSpecifiedInCurrentPom);
            if (version == null || (!processUnboundPlugins && !versionSpecifiedInCurrentPom)) {
                version = superPomPluginManagement.get(coords);
                getLog().debug("[" + coords + "].superPom.version=" + version);

                newVersion = artifactVersion != null
                        ? artifactVersion.toString()
                        : (version != null ? version : (effectiveVersion != null ? effectiveVersion : "(unknown)"));
                if (version != null) {
                    superPomDrivingMinVersion = true;
                }

                pluginLockdowns.add(pad(
                        compactKey(groupId, artifactId),
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
                    && new DefaultArtifactVersion(effectiveVersion).compareTo(new DefaultArtifactVersion(newVersion))
                            < 0) {
                pluginUpdates.add(pad(
                        compactKey(groupId, artifactId),
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
        boolean noMavenMinVersion = MinimalMavenBuildVersionFinder.find(getProject(), null, getLog()) == null;
        if (noMavenMinVersion) {
            getLog().warn("Project does not define minimum Maven version required for build, default is: "
                    + DEFAULT_MVN_VERSION);
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
            if (noMavenMinVersion) {
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
            if (noMavenMinVersion) {
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
            if (upgradePlugins.isEmpty() || compare(specMavenVersion, mavenUpgradeVersion) >= 0) {
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
        Artifact probe = getHelper()
                .createDependencyArtifact(DependencyBuilder.newBuilder()
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
                        r -> r.setRemoteRepositories(session.getCurrentProject().getPluginArtifactRepositories())));
        if (!result.getProblems().isEmpty()) {
            getLog().warn("Problems encountered during construction of the plugin POM for " + probe.toString());
            result.getProblems().forEach(p -> getLog().warn("\t" + p.getMessage()));
        }
        return result.getProject();
    }

    private static String pad(String start, int len, String... ends) {
        StringBuilder buf = new StringBuilder(len);
        buf.append("  ");
        buf.append(start);
        int padding = len;
        for (String end : ends) {
            padding -= String.valueOf(end).length();
        }
        buf.append(' ');
        while (buf.length() < padding) {
            buf.append('.');
        }
        buf.append(' ');
        for (String end : ends) {
            buf.append(end);
        }
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
                try {
                    Set<String> withVersionSpecified = findPluginsWithVersionsSpecified(
                            new StringBuilder(writer.toString()), getSafeProjectPathInfo(parentProject));

                    Map<String, String> map = getPluginManagement(interpolatedModel);
                    map.keySet().retainAll(withVersionSpecified);
                    parentPlugins.putAll(map);

                    map = getBuildPlugins(interpolatedModel, true);
                    map.keySet().retainAll(withVersionSpecified);
                    parentPlugins.putAll(map);

                    map = getReportPlugins(interpolatedModel, true);
                    map.keySet().retainAll(withVersionSpecified);
                    parentPlugins.putAll(map);
                } catch (XMLStreamException e) {
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

    private String getSafeProjectPathInfo(MavenProject project) {
        return ofNullable(project.getFile())
                .map(File::getAbsolutePath)
                // path is used only as information in error message,
                // we can fallback to project artifact info here
                .orElse(project.toString());
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
     * @param pomContents The project to getModel the plugins with versions specified.
     * @param path        Path that points to the source of the XML
     * @return a set of Strings which correspond to the plugin coordinates where there is a version specified.
     */
    private Set<String> findPluginsWithVersionsSpecified(StringBuilder pomContents, String path)
            throws XMLStreamException {
        Set<String> result = new HashSet<>();
        ModifiedPomXMLEventReader pom = newModifiedPomXER(pomContents, path);

        Pattern pathRegex = Pattern.compile(
                "/project(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin");
        Stack<StackState> pathStack = new Stack<>();
        StackState curState = null;
        while (pom.hasNext()) {
            XMLEvent event = pom.nextEvent();
            if (event.isStartDocument()) {
                curState = new StackState("");
                pathStack.clear();
            } else if (event.isStartElement()) {
                String elementName = event.asStartElement().getName().getLocalPart();
                if (curState != null && pathRegex.matcher(curState.path).matches()) {
                    if ("groupId".equals(elementName)) {
                        curState.groupId = pom.getElementText().trim();
                        continue;
                    } else if ("artifactId".equals(elementName)) {
                        curState.artifactId = pom.getElementText().trim();
                        continue;

                    } else if ("version".equals(elementName)) {
                        curState.version = pom.getElementText().trim();
                        continue;
                    }
                }

                pathStack.push(curState);
                curState = new StackState(curState.path + "/" + elementName);
            } else if (event.isEndElement()) {
                if (curState != null && pathRegex.matcher(curState.path).matches()) {
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
     * @see <a href="https://github.com/apache/maven-plugin-tools/blob/c8ddcdcb10d342a5a5e2f38245bb569af5730c7c/maven-plugin-plugin/src/main/java/org/apache/maven/plugin/plugin/PluginReport.java#L711">PluginReport</a>
     *
     * @param pluginProject the plugin for which to retrieve the minimum Maven version which is required
     * @return The minimally required Maven version (never {@code null})
     */
    private ArtifactVersion getPrerequisitesMavenVersion(MavenProject pluginProject) {
        return ofNullable(pluginProject.getPrerequisites())
                .map(Prerequisites::getMaven)
                .map(DefaultArtifactVersion::new)
                .orElse(new DefaultArtifactVersion(DEFAULT_MVN_VERSION));
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
     * @param model                the model to getModel the build plugins from.
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
     * Returns the lifecycle plugins of a specific project.
     *
     * @param project the project to getModel the lifecycle plugins from.
     * @return The map of effective plugin versions keyed by coordinates.
     * @throws org.apache.maven.plugin.MojoExecutionException if things go wrong.
     * @since 1.0-alpha-1
     */
    private Map<String, Plugin> getLifecyclePlugins(MavenProject project) throws MojoExecutionException {
        return getBoundPlugins(project).stream()
                .filter(Objects::nonNull)
                .filter(p -> p.getKey() != null)
                .collect(HashMap<String, Plugin>::new, (m, p) -> m.put(p.getKey(), p), Map::putAll);
    }

    /**
     * Gets the plugins that are bound to the defined phases. This does not find plugins bound in the pom to a phase
     * later than the plugin is executing.
     *
     * @param project   the project
     * @return the bound plugins
     */
    // pilfered this from enforcer-rules
    // TODO coordinate with Brian Fox to remove the duplicate code
    private Set<Plugin> getBoundPlugins(MavenProject project) {
        // we need to provide a copy with the version blanked out so that inferring from super-pom
        // works as for 2.x as 3.x fills in the version on us!
        return lifecycleExecutor.getPluginsBoundByDefaultToAllLifecycles(project.getPackaging()).stream()
                .map(p -> new Plugin() {
                    {
                        setGroupId(p.getGroupId());
                        setArtifactId(p.getArtifactId());
                    }
                })
                .collect(toSet());
    }

    /**
     * Returns all the parent projects of the specified project, with the root project first.
     *
     * @param project The maven project to getModel the parents of
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
     * @throws org.apache.maven.plugin.MojoExecutionException if things go wrong.
     */
    @SuppressWarnings("checkstyle:MethodLength")
    private Set<Plugin> getProjectPlugins(
            Map<String, String> superPomPluginManagement,
            Map<String, String> parentPluginManagement,
            Map<String, String> parentBuildPlugins,
            Map<String, String> parentReportPlugins,
            Set<String> pluginsWithVersionsSpecified)
            throws MojoExecutionException {
        Map<String, Plugin> plugins = new HashMap<>();

        getLog().debug("Building list of project plugins...");

        if (getLog().isDebugEnabled()) {
            StringWriter origModel = new StringWriter();

            try {
                origModel.write("Original model:\n");
                getProject().writeOriginalModel(origModel);
                getLog().debug(origModel.toString());
            } catch (IOException e) {
                // ignore
            }
        }

        debugVersionMap("super-pom version map", superPomPluginManagement);
        debugVersionMap("parent version map", parentPluginManagement);

        Map<String, String> excludePluginManagement = new HashMap<>(superPomPluginManagement);
        excludePluginManagement.putAll(parentPluginManagement);

        debugVersionMap("aggregate version map", excludePluginManagement);

        excludePluginManagement.keySet().removeAll(pluginsWithVersionsSpecified);

        debugVersionMap("final aggregate version map", excludePluginManagement);

        ModelBuildingRequest modelBuildingRequest = new DefaultModelBuildingRequest();
        modelBuildingRequest.setUserProperties(getProject().getProperties());
        Model originalModel = modelInterpolator.interpolateModel(
                getProject().getOriginalModel(),
                getProject().getBasedir(),
                modelBuildingRequest,
                new IgnoringModelProblemCollector());

        try {
            addProjectPlugins(
                    plugins, originalModel.getBuild().getPluginManagement().getPlugins(), excludePluginManagement);
        } catch (NullPointerException e) {
            // guess there are no plugins here
        }
        debugPluginMap("after adding local pluginManagement", plugins);

        try {
            List<Plugin> lifecyclePlugins =
                    new ArrayList<>(getLifecyclePlugins(getProject()).values());
            for (Iterator<Plugin> i = lifecyclePlugins.iterator(); i.hasNext(); ) {
                Plugin lifecyclePlugin = i.next();
                if (lifecyclePlugin.getVersion() != null) {
                    // version comes from lifecycle, therefore cannot modify
                    i.remove();
                } else {
                    // lifecycle leaves version open
                    String parentVersion = parentPluginManagement.get(lifecyclePlugin.getKey());
                    if (parentVersion != null) {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins(plugins, lifecyclePlugins, parentPluginManagement);

            debugPluginMap("after adding lifecycle plugins", plugins);
        } catch (NullPointerException e) {
            // using maven 3.x or newer
        }

        try {
            List<Plugin> buildPlugins = new ArrayList<>(originalModel.getBuild().getPlugins());
            for (Iterator<Plugin> i = buildPlugins.iterator(); i.hasNext(); ) {
                Plugin buildPlugin = i.next();
                if (buildPlugin.getVersion() == null) {
                    String parentVersion = parentPluginManagement.get(buildPlugin.getKey());
                    if (parentVersion != null) {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins(plugins, buildPlugins, parentBuildPlugins);
        } catch (NullPointerException e) {
            // guess there are no plugins here
        }
        debugPluginMap("after adding build plugins", plugins);

        try {
            List<ReportPlugin> reportPlugins =
                    new ArrayList<>(originalModel.getReporting().getPlugins());
            for (Iterator<ReportPlugin> i = reportPlugins.iterator(); i.hasNext(); ) {
                ReportPlugin reportPlugin = i.next();
                if (reportPlugin.getVersion() == null) {
                    String parentVersion = parentPluginManagement.get(reportPlugin.getKey());
                    if (parentVersion != null) {
                        // parent controls version
                        i.remove();
                    }
                }
            }
            addProjectPlugins(plugins, toPlugins(reportPlugins), parentReportPlugins);
        } catch (NullPointerException e) {
            // guess there are no plugins here
        }
        debugPluginMap("after adding reporting plugins", plugins);

        for (Profile profile : originalModel.getProfiles()) {
            try {
                addProjectPlugins(
                        plugins, profile.getBuild().getPluginManagement().getPlugins(), excludePluginManagement);
            } catch (NullPointerException e) {
                // guess there are no plugins here
            }
            debugPluginMap("after adding build pluginManagement for profile " + profile.getId(), plugins);

            try {
                addProjectPlugins(plugins, profile.getBuild().getPlugins(), parentBuildPlugins);
            } catch (NullPointerException e) {
                // guess there are no plugins here
            }
            debugPluginMap("after adding build plugins for profile " + profile.getId(), plugins);

            try {
                addProjectPlugins(plugins, toPlugins(profile.getReporting().getPlugins()), parentReportPlugins);
            } catch (NullPointerException e) {
                // guess there are no plugins here
            }
            debugPluginMap("after adding reporting plugins for profile " + profile.getId(), plugins);
        }
        Set<Plugin> result = new TreeSet<>(PluginComparator.INSTANCE);
        result.addAll(plugins.values());
        return result;
    }

    /**
     * Adds those project plugins which are not inherited from the parent definitions to the list of plugins.
     *
     * @param plugins           The list of plugins.
     * @param projectPlugins    The project's plugins.
     * @param parentDefinitions The parent plugin definitions.
     * @since 1.0-alpha-1
     */
    private void addProjectPlugins(
            Map<String, Plugin> plugins, Collection<Plugin> projectPlugins, Map<String, String> parentDefinitions) {
        for (Plugin plugin : projectPlugins) {
            String coord = plugin.getKey();
            String version = plugin.getVersion();
            String parentVersion = parentDefinitions.get(coord);
            if (version == null
                    && (!plugins.containsKey(coord) || plugins.get(coord).getVersion() == null)
                    && parentVersion != null) {
                Plugin parentPlugin = new Plugin();
                parentPlugin.setGroupId(plugin.getGroupId());
                parentPlugin.setArtifactId(plugin.getArtifactId());
                parentPlugin.setVersion(parentVersion);
                plugins.put(coord, parentPlugin);
            } else if (parentVersion == null || !parentVersion.equals(version)) {
                if (!plugins.containsKey(coord) || plugins.get(coord).getVersion() == null) {
                    plugins.put(coord, plugin);
                }
            }
            if (!plugins.containsKey(coord)) {
                plugins.put(coord, plugin);
            }
        }
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
            StringBuilder buf = new StringBuilder(description);
            for (Plugin plugin : sorted) {
                buf.append("\n    ");
                buf.append(plugin.getKey());
                buf.append(":");
                buf.append(plugin.getVersion());
            }
            getLog().debug(buf.toString());
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
            StringBuilder buf = new StringBuilder(description);
            for (Map.Entry<String, String> pluginVersion : pluginVersions.entrySet()) {
                buf.append("\n    ");
                buf.append(pluginVersion.getKey());
                buf.append(":");
                buf.append(pluginVersion.getValue());
            }
            getLog().debug(buf.toString());
        }
    }

    private static Plugin toPlugin(ReportPlugin reportPlugin) {
        Plugin plugin = new Plugin();
        plugin.setGroupId(reportPlugin.getGroupId());
        plugin.setArtifactId(reportPlugin.getArtifactId());
        plugin.setVersion(reportPlugin.getVersion());
        return plugin;
    }

    private static List<Plugin> toPlugins(List<ReportPlugin> reportPlugins) {
        List<Plugin> result = new ArrayList<>(reportPlugins.size());
        for (ReportPlugin reportPlugin : reportPlugins) {
            result.add(toPlugin(reportPlugin));
        }
        return result;
    }

    /**
     * Gets the report plugins of a specific project.
     *
     * @param model                the model to getModel the report plugins from.
     * @param onlyIncludeInherited <code>true</code> to only return the plugins definitions that will be inherited by
     *                             child projects.
     * @return The map of effective plugin versions keyed by coordinates.
     * @since 1.0-alpha-1
     */
    private Map<String, String> getReportPlugins(Model model, boolean onlyIncludeInherited) {
        Map<String, String> reportPlugins = new HashMap<>();
        try {
            for (ReportPlugin plugin : model.getReporting().getPlugins()) {
                String coord = plugin.getKey();
                String version = plugin.getVersion();
                if (version != null && (!onlyIncludeInherited || getPluginInherited(plugin))) {
                    reportPlugins.put(coord, version);
                }
            }
        } catch (NullPointerException e) {
            // guess there are no plugins here
        }
        try {
            for (Profile profile : model.getProfiles()) {
                try {
                    for (ReportPlugin plugin : profile.getReporting().getPlugins()) {
                        String coord = plugin.getKey();
                        String version = plugin.getVersion();
                        if (version != null && (!onlyIncludeInherited || getPluginInherited(plugin))) {
                            reportPlugins.put(coord, version);
                        }
                    }
                } catch (NullPointerException e) {
                    // guess there are no plugins here
                }
            }
        } catch (NullPointerException e) {
            // guess there are no profiles here
        }
        return reportPlugins;
    }

    /**
     * @param pom the pom to update.
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update(ModifiedPomXMLEventReader pom) {
        // do nothing
    }

    private static int compare(ArtifactVersion a, ArtifactVersion b) {
        return a.compareTo(b);
    }

    private static class IgnoringModelProblemCollector implements ModelProblemCollector {

        @Override
        public void add(ModelProblemCollectorRequest req) {
            // ignore
        }
    }
}
