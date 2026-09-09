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

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.TransformerException;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.lifecycle.LifecycleExecutor;
import org.apache.maven.model.Build;
import org.apache.maven.model.BuildBase;
import org.apache.maven.model.Extension;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
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
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ExtensionUtils;
import org.codehaus.mojo.versions.utils.PluginComparator;

import static java.util.Collections.emptyMap;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toMap;
import static javax.xml.stream.XMLStreamConstants.END_DOCUMENT;

/** Discovers plugin declarations without changing Maven's project models. */
final class PluginUpdatesDiscovery {
    static final Pattern PATTERN_PROJECT_PLUGIN = Pattern.compile(
            "/project(/profiles/profile)?" + "((/build(/pluginManagement)?)|(/reporting))" + "/plugins/plugin");
    static final String SUPERPOM_PATH = "org/apache/maven/model/pom-4.0.0.xml";
    private final MavenProject project;
    private final Log log;
    private final LifecycleExecutor lifecycleExecutor;
    private final ModelInterpolator modelInterpolator;
    private final boolean processUnboundPlugins;

    PluginUpdatesDiscovery(
            MavenProject project,
            Log log,
            LifecycleExecutor lifecycleExecutor,
            ModelInterpolator modelInterpolator,
            boolean processUnboundPlugins) {
        this.project = project;
        this.log = log;
        this.lifecycleExecutor = lifecycleExecutor;
        this.modelInterpolator = modelInterpolator;
        this.processUnboundPlugins = processUnboundPlugins;
    }

    private MavenProject getProject() {
        return project;
    }

    private Log getLog() {
        return log;
    }

    static final class DeclaredPlugins {
        final Set<String> specified;
        final Map<String, String> defaults;
        final Map<String, String> parents;
        final Set<Plugin> plugins;

        DeclaredPlugins(
                Set<String> specified, Map<String, String> defaults, Map<String, String> parents, Set<Plugin> plugins) {
            this.specified = specified;
            this.defaults = defaults;
            this.parents = parents;
            this.plugins = plugins;
        }
    }

    DeclaredPlugins declaredPlugins()
            throws MojoExecutionException, IOException, XMLStreamException, TransformerException {
        Set<String> specified = findPluginsWithVersionsSpecified(project.getOriginalModel());
        Map<String, String> defaults = getSuperPomPluginManagement();
        Map<String, String> parents = getParentsPlugins(getParentProjects(project));
        return new DeclaredPlugins(
                specified,
                defaults,
                parents,
                getPluginManagementPlugins(defaults, parents, new HashMap<>(), new HashMap<>(), specified));
    }

    static Set<String> findPluginsWithVersionsSpecified(Model model) {
        Set<String> result = new HashSet<>();
        Stream.concat(Stream.of(model), model.getProfiles().stream().map(PluginUpdatesDiscovery::profileModel))
                .forEach(m -> {
                    if (m.getBuild() != null) {
                        Stream.concat(m.getBuild().getPlugins().stream(), getPluginManagementPlugins(m.getBuild()))
                                .filter(plugin -> plugin.getVersion() != null)
                                .map(Plugin::getKey)
                                .forEach(result::add);
                    }
                    if (m.getReporting() != null) {
                        m.getReporting().getPlugins().stream()
                                .filter(plugin -> plugin.getVersion() != null)
                                .map(ReportPlugin::getKey)
                                .forEach(result::add);
                    }
                });
        return result;
    }

    static final class Declaration {
        final Plugin plugin;
        final String context;

        Declaration(Plugin plugin, String context) {
            this.plugin = plugin.clone();
            this.context = context;
        }
    }

    static List<Declaration> effectivePlugins(MavenProject project) {
        List<Declaration> result = new ArrayList<>();
        if (project.getBuild() != null) {
            project.getBuildPlugins().forEach(p -> result.add(new Declaration(p, "build")));
            getPluginManagementPlugins(project.getBuild())
                    .forEach(p -> result.add(new Declaration(p, "pluginManagement")));
        }
        if (project.getReporting() != null) {
            // Maven resolves unspecified reporting versions from build plugins before plugin management.
            Map<String, String> defaults = new HashMap<>();
            result.stream()
                    .map(d -> d.plugin)
                    .filter(p -> p.getVersion() != null)
                    .forEach(p -> defaults.putIfAbsent(p.getKey(), p.getVersion()));
            project.getReportPlugins().forEach(p -> {
                Plugin plugin = toPlugin(p);
                if (plugin.getVersion() == null) {
                    plugin.setVersion(defaults.get(plugin.getKey()));
                }
                result.add(new Declaration(plugin, "reporting"));
            });
        }
        return result;
    }

    private static Model profileModel(Profile profile) {
        Model model = new Model();
        if (profile.getBuild() != null) {
            Build build = new Build();
            build.setPlugins(profile.getBuild().getPlugins());
            build.setPluginManagement(profile.getBuild().getPluginManagement());
            model.setBuild(build);
        }
        model.setReporting(profile.getReporting());
        return model;
    }

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

    private Map<String, String> getParentsPlugins(List<MavenProject> parents) {
        Map<String, String> result = new HashMap<>();
        for (MavenProject parent : parents) {
            Model original = ofNullable(parent.getOriginalModel()).orElse(parent.getModel());
            Set<String> specified = findPluginsWithVersionsSpecified(original);
            ModelBuildingRequest request = new DefaultModelBuildingRequest();
            request.setUserProperties(project.getProperties());
            Model model = modelInterpolator.interpolateModel(
                    original.clone(), null, request, new IgnoringModelProblemCollector());
            for (Map<String, String> versions : Arrays.asList(
                    getPluginManagement(model), getBuildPlugins(model, true), getReportPlugins(model, true))) {
                versions.keySet().retainAll(specified);
                result.putAll(versions);
            }
        }
        return result;
    }

    private Map<String, String> getPluginsFromBuild(BuildBase build, boolean onlyIncludeInherited) {
        return ofNullable(build)
                .flatMap(b -> ofNullable(b.getPlugins())
                        .map(plugins -> plugins.stream()
                                .filter(plugin -> plugin.getVersion() != null)
                                .filter(plugin -> !onlyIncludeInherited || getPluginInherited(plugin))
                                .collect(toMap(Plugin::getKey, Plugin::getVersion))))
                .orElse(emptyMap());
    }

    private Map<String, String> getBuildPlugins(Model model, boolean onlyIncludeInherited) {
        Map<String, String> buildPlugins = new HashMap<>(getPluginsFromBuild(model.getBuild(), onlyIncludeInherited));
        ofNullable(model.getProfiles())
                .ifPresent(profiles -> profiles.stream()
                        .map(profile -> getPluginsFromBuild(profile.getBuild(), onlyIncludeInherited))
                        .forEach(buildPlugins::putAll));
        return buildPlugins;
    }

    private static boolean getPluginInherited(Object plugin) {
        return "true"
                .equalsIgnoreCase(
                        plugin instanceof ReportPlugin
                                ? ((ReportPlugin) plugin).getInherited()
                                : ((Plugin) plugin).getInherited());
    }

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

    private List<MavenProject> getParentProjects(MavenProject project) throws MojoExecutionException {
        List<MavenProject> parents = new ArrayList<>();
        while (project.getParent() != null) {
            project = project.getParent();
            parents.add(0, project);
        }
        return parents;
    }

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
                getProject().getOriginalModel().clone(),
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
                .map(PluginUpdatesDiscovery::getPluginManagementPlugins)
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
                    .map(PluginUpdatesDiscovery::getPluginManagementPlugins)
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
                .map(PluginUpdatesDiscovery::toPlugin);
    }

    private Stream<Plugin> getLifecyclePlugins(Map<String, String> parentPluginManagement) {
        return getBoundPlugins(getProject())
                .filter(Objects::nonNull)
                .filter(p -> p.getKey() != null)
                .filter(p -> p.getVersion() != null)
                .filter(p -> parentPluginManagement.get(p.getKey()) != null);
    }

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
        return reportPlugins.stream().map(PluginUpdatesDiscovery::toPlugin);
    }

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

    private static class IgnoringModelProblemCollector implements ModelProblemCollector {

        @Override
        public void add(ModelProblemCollectorRequest req) {
            // ignore
        }
    }
}
