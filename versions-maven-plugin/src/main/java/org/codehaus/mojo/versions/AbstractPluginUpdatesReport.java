package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import javax.inject.Inject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.mojo.versions.xml.PluginUpdatesXmlReportRenderer;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates a report of available updates for the plugins of a project.
 * Base class, abstracting functionality regardless of whether we're rendering an individual, or an aggregate report.
 */
public abstract class AbstractPluginUpdatesReport extends AbstractVersionsReport<PluginUpdatesModel> {

    @Inject
    protected ProjectBuilder projectBuilder;

    @Inject
    protected RuntimeInformation runtimeInformation;

    private final ArtifactFactory pluginArtifactFactory;

    private static final PluginComparator PLUGIN_COMPARATOR = PluginComparator.INSTANCE;

    /**
     * Report formats (html and/or xml). HTML by default.
     */
    @Parameter(property = "pluginUpdatesReportFormats", defaultValue = "html")
    private String[] formats = new String[] {"html"};

    /**
     * If <code>true</code>, only shows the subsection of the <code>pluginManagement</code> artifacts that
     * are actually used in the project's <code>plugin</code> graph. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter(property = "onlyProjectPlugins", defaultValue = "false")
    protected boolean onlyProjectPlugins;

    /**
     * If <code>true</code>, only shows upgradable plugins in the report. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter(property = "onlyUpgradable", defaultValue = "false")
    protected boolean onlyUpgradable;

    /**
     * Creates a new instance.
     *
     * @param i18n             {@link I18N} bean instance
     * @param artifactFactory  {@link ArtifactFactory} bean instance
     * @param repositorySystem {@link RepositorySystem} bean instance
     * @param wagonMap         map of {@link Wagon} instances per protocol
     * @param rendererFactory  {@link ReportRendererFactory} instance
     */
    protected AbstractPluginUpdatesReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
        this.pluginArtifactFactory = artifactFactory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isExternalReport() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean canGenerateReport() {
        return getProjectsToAnalyze().stream()
                .anyMatch(p -> !PluginUpdatesDiscovery.effectivePlugins(p).isEmpty());
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    @Override
    protected void doGenerateReport(Locale locale, Sink sink) throws MavenReportException {

        Map<Plugin, PluginUpdatesDetails> pluginUpdates = new TreeMap<>(PLUGIN_COMPARATOR);
        Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates = new TreeMap<>(PLUGIN_COMPARATOR);
        try {
            for (MavenProject project : getProjectsToAnalyze()) {
                VersionsHelper helper = project == getProject() ? getHelper() : createHelper(project);
                PluginUpdatesAnalyzer analyzer = new PluginUpdatesAnalyzer(
                        pluginArtifactFactory,
                        helper,
                        projectBuilder,
                        session,
                        project,
                        getLog(),
                        ArtifactVersionService.getArtifactVersion(runtimeInformation.getMavenVersion()),
                        getAllowSnapshots());
                List<PluginUpdatesDiscovery.Declaration> declarations =
                        PluginUpdatesDiscovery.effectivePlugins(project);
                Set<Plugin> plugins = new TreeSet<>(PLUGIN_COMPARATOR);
                Set<Plugin> management = new TreeSet<>(PLUGIN_COMPARATOR);
                declarations.forEach(d -> ("pluginManagement".equals(d.context) ? management : plugins).add(d.plugin));
                handleOnlyProjectPlugins(management, plugins);
                for (PluginUpdatesDiscovery.Declaration declaration : declarations) {
                    Plugin plugin = declaration.plugin;
                    boolean managed = "pluginManagement".equals(declaration.context);
                    boolean inSummary = (managed ? management : plugins).contains(plugin);
                    if (!inSummary) {
                        continue;
                    }
                    PluginUpdatesDetails details = analyzer.reportDetails(plugin);
                    Plugin reportPlugin = plugin.clone();
                    if (reportPlugin.getVersion() == null) {
                        reportPlugin.setVersion(details.getVersion());
                    }
                    (managed ? pluginManagementUpdates : pluginUpdates)
                            .merge(reportPlugin, details, AbstractPluginUpdatesReport::mergePluginUpdates);
                }
            }
            if (onlyUpgradable) {
                pluginUpdates.values().removeIf(details -> details.isEmpty(getAllowSnapshots()));
                pluginManagementUpdates.values().removeIf(details -> details.isEmpty(getAllowSnapshots()));
            }
            renderReport(locale, sink, new PluginUpdatesModel(pluginUpdates, pluginManagementUpdates));
        } catch (VersionRetrievalException | MojoExecutionException e) {
            throw new MavenReportException(e.getMessage(), e);
        }
    }

    private static PluginUpdatesDetails mergePluginUpdates(PluginUpdatesDetails left, PluginUpdatesDetails right) {
        left.addDependencyVersions(right.getDependencyVersions());
        return new PluginUpdatesDetails(
                new ArtifactVersions(
                        left.getArtifact(),
                        Stream.concat(Arrays.stream(left.getVersions(true)), Arrays.stream(right.getVersions(true)))
                                .collect(Collectors.toList())),
                left.getDependencyVersions(),
                left.isIncludeSnapshots());
    }

    /** Analyze aggregate projects separately to use each project's repositories and effective plugin versions. */
    protected List<MavenProject> getProjectsToAnalyze() {
        return Collections.singletonList(getProject());
    }

    private void renderReport(Locale locale, Sink sink, PluginUpdatesModel model) throws MavenReportException {
        for (String format : formats) {
            if ("html".equals(format)) {
                rendererFactory
                        .createReportRenderer(getOutputPath(), sink, locale, model, allowSnapshots)
                        .render();
            } else if ("xml".equals(format)) {
                Path outputDir = Paths.get(getProject().getBuild().getDirectory());
                if (!Files.exists(outputDir)) {
                    try {
                        Files.createDirectories(outputDir);
                    } catch (IOException e) {
                        throw new MavenReportException("Could not create the output directory");
                    }
                }
                Path outputFile = outputDir.resolve(getOutputPath() + ".xml");
                new PluginUpdatesXmlReportRenderer(model, outputFile, allowSnapshots).render();
            }
        }
    }

    private void handleOnlyProjectPlugins(Set<Plugin> pluginManagement, Set<Plugin> plugins) {

        if (!onlyProjectPlugins) {
            // Retains only plugins not present in pluginManagement
            plugins.removeIf(plugin ->
                    pluginManagement.stream().anyMatch(pmPlugin -> PLUGIN_COMPARATOR.compare(plugin, pmPlugin) == 0));
        } else {
            // Retain only plugins in pluginManagement that are also present in plugins
            pluginManagement.removeIf(
                    pmPlugin -> plugins.stream().noneMatch(plugin -> PLUGIN_COMPARATOR.compare(plugin, pmPlugin) == 0));
        }
    }
}
