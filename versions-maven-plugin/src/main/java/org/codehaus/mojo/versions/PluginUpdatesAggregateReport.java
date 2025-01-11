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

import java.util.Map;
import java.util.Set;

import org.apache.maven.model.Plugin;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.util.AggregateReportUtils;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates an aggregated report of available updates for the plugins of a project.
 *
 * @since 2.14.0
 */
@Mojo(
        name = "plugin-updates-aggregate-report",
        requiresDependencyResolution = ResolutionScope.RUNTIME,
        threadSafe = true,
        aggregator = true)
public class PluginUpdatesAggregateReport extends AbstractPluginUpdatesReport {

    @Inject
    protected PluginUpdatesAggregateReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
    }

    @Override
    protected void populatePluginManagement(Set<Plugin> pluginManagementCollector) {
        for (MavenProject project : AggregateReportUtils.getProjectsToProcess(getProject())) {
            if (haveBuildPluginManagementPlugins(project)) {
                pluginManagementCollector.addAll(
                        project.getBuild().getPluginManagement().getPlugins());
            }
        }
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populatePlugins(Set<Plugin> pluginsCollector) {
        for (MavenProject project : AggregateReportUtils.getProjectsToProcess(getProject())) {
            if (haveBuildPluginManagementPlugins(project)) {
                pluginsCollector.addAll(project.getBuild().getPlugins());
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBundleName() {
        return "plugin-updates-aggregate-report";
    }
}
