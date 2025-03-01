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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BinaryOperator;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.PluginComparator;
import org.codehaus.mojo.versions.xml.PluginUpdatesXmlReportRenderer;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

import static org.codehaus.mojo.versions.utils.MiscUtils.filter;

/**
 * Generates a report of available updates for the plugins of a project.
 */
public abstract class AbstractPluginUpdatesReport extends AbstractVersionsReport<PluginUpdatesModel> {

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

    protected AbstractPluginUpdatesReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
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
        return haveBuildPlugins(getProject()) || haveBuildPluginManagementPlugins(getProject());
    }

    protected boolean haveBuildPluginManagementPlugins(MavenProject project) {
        return project.getBuild() != null
                && project.getBuild().getPluginManagement() != null
                && project.getBuild().getPluginManagement().getPlugins() != null
                && !project.getBuild().getPluginManagement().getPlugins().isEmpty();
    }

    protected boolean haveBuildPlugins(MavenProject project) {
        return project.getBuild() != null
                && project.getBuild().getPlugins() != null
                && !project.getBuild().getPlugins().isEmpty();
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    @Override
    protected void doGenerateReport(Locale locale, Sink sink) throws MavenReportException {

        Set<Plugin> pluginManagement = getPluginManagement();

        Set<Plugin> plugins = getPlugins();

        handleOnlyProjectPlugins(pluginManagement, plugins);

        try {

            Map<Plugin, PluginUpdatesDetails> pluginUpdates =
                    getHelper().lookupPluginsUpdates(plugins.stream(), getAllowSnapshots());
            Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates =
                    getHelper().lookupPluginsUpdates(pluginManagement.stream(), getAllowSnapshots());

            if (onlyUpgradable) {

                BinaryOperator<PluginUpdatesDetails> merger = (pluginUpdatesDetails, pluginUpdatesDetails2) -> {
                    pluginUpdatesDetails.addDependencyVersions(pluginUpdatesDetails2.getDependencyVersions());
                    return pluginUpdatesDetails;
                };
                pluginUpdates = filter(pluginUpdates, p -> !p.isEmpty(allowSnapshots), merger);
                pluginManagementUpdates = filter(pluginManagementUpdates, p -> !p.isEmpty(allowSnapshots));
            }

            renderReport(locale, sink, new PluginUpdatesModel(pluginUpdates, pluginManagementUpdates));
        } catch (VersionRetrievalException e) {
            throw new MavenReportException(e.getMessage(), e);
        }
    }

    /**
     * Constructs a instance of a {@link Set<Plugin>} with a {@link PluginComparator} comparator. This set can be
     * further populated by implementations and should contain plugins, that are present in projects pluginManagement
     * section.
     *
     * @return a {@link Set<Plugin>} that can be additionally populated by {@link #populatePluginManagement(Set)}}.
     * If not, an empty set is returned
     * */
    private Set<Plugin> getPluginManagement() {
        final Set<Plugin> pluginManagementCollector = new TreeSet<>(PLUGIN_COMPARATOR);
        populatePluginManagement(pluginManagementCollector);
        return pluginManagementCollector;
    }

    /**
     * Implementations of {@link AbstractPluginUpdatesReport} may use this to supply the main processing logic
     * with desired pluginManagement data, which will be used in the creation of the report.
     *
     * @param pluginManagementCollector, a set initialized with a {@link PluginComparator} comparator.
     * */
    protected abstract void populatePluginManagement(Set<Plugin> pluginManagementCollector);

    /**
     * Constructs a final instance of a {@link Set<Plugin>} with a {@link PluginComparator} comparator. This set can be
     * further populated by implementations, and should contain plugins, that are present in projects build section.
     *
     * @return a {@link Set<Plugin>} that can be additionally populated by {@link #populatePlugins(Set)}.
     * If not, an empty set is returned
     * */
    private Set<Plugin> getPlugins() {
        final Set<Plugin> pluginsCollector = new TreeSet<>(PLUGIN_COMPARATOR);
        populatePlugins(pluginsCollector);
        return pluginsCollector;
    }

    /**
     * Implementations of {@link  AbstractPluginUpdatesReport} may use this to supply the main processing logic
     * with desired build plugin information, which will be used to create the report.
     *
     *@param pluginsCollector, a set initialized with a {@link PluginComparator} comparator.
     * */
    protected abstract void populatePlugins(Set<Plugin> pluginsCollector);

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
