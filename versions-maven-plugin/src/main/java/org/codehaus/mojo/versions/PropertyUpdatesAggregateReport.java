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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.util.AggregateReportUtils;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates an aggregate report of available updates for properties of a project which are linked to the dependencies
 * and/or plugins of a project.
 *
 * @since 2.14.0
 */
@Mojo(
        name = "property-updates-aggregate-report",
        requiresDependencyResolution = ResolutionScope.RUNTIME,
        threadSafe = true,
        aggregator = true)
public class PropertyUpdatesAggregateReport extends AbstractPropertyUpdatesReport {

    @Inject
    protected PropertyUpdatesAggregateReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populateUpdateSet(Map<Property, PropertyVersions> propertyCollector)
            throws MojoExecutionException, MavenReportException {
        VersionsHelper helper = getHelper();
        for (MavenProject project : AggregateReportUtils.getProjectsToProcess(getProject())) {
            propertyCollector.putAll(helper.getVersionPropertiesMap(getRequest(project)));
        }
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected boolean haveBuildProperties() {
        for (MavenProject project : AggregateReportUtils.getProjectsToProcess(getProject())) {
            if (project.getProperties() != null && !project.getProperties().isEmpty()) {
                return true;
            }
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBundleName() {
        return "property-updates-aggregate-report";
    }
}
