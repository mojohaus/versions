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
import java.util.TreeMap;

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper.VersionPropertiesMapRequest;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.PropertyUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.PropertyComparator;
import org.codehaus.mojo.versions.xml.PropertyUpdatesXmlReportRenderer;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates a report of available updates for properties of a project which are linked to the dependencies and/or
 * plugins of a project.
 */
public abstract class AbstractPropertyUpdatesReport extends AbstractVersionsReport<PropertyUpdatesModel> {

    private static final PropertyComparator PROPERTIES_COMPARATOR = PropertyComparator.INSTANCE;

    /**
     * Any restrictions that apply to specific properties.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private Property[] properties;

    /**
     * A comma separated list of properties to include in the report.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "includeProperties")
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not include in the report.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "excludeProperties")
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "autoLinkItems", defaultValue = "true")
    private boolean autoLinkItems;

    /**
     * <p>Whether to include property updates from parent.</p>
     *
     * @since 2.14.0
     */
    @Parameter(property = "includeParent", defaultValue = "true")
    private boolean includeParent = true;

    /**
     * Report formats (html and/or xml). HTML by default.
     *
     * @since 2.14.0
     */
    @Parameter(property = "propertyUpdatesReportFormats", defaultValue = "html")
    protected String[] formats = new String[] {"html"};

    protected AbstractPropertyUpdatesReport(
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
        return haveBuildProperties();
    }

    protected boolean haveBuildProperties() {
        return getProject().getProperties() != null
                && !getProject().getProperties().isEmpty();
    }

    protected void doGenerateReport(Locale locale, Sink sink) throws MavenReportException {
        try {
            final Map<Property, PropertyVersions> updateSet = new TreeMap<>(PROPERTIES_COMPARATOR);
            populateUpdateSet(updateSet);

            renderReport(locale, sink, getPropertyUpdatesModel(updateSet));
        } catch (MojoExecutionException e) {
            throw new MavenReportException(e.getMessage(), e);
        }
    }

    /**
     * Method used to supply {@link PropertyUpdatesModel} with data about updated properties.
     *
     * @param propertyCollector map for collecting properties in implementations
     *
     * @throws MavenReportException when things go wrong.
     * @throws MojoExecutionException if something goes wrong.
     * */
    protected abstract void populateUpdateSet(Map<Property, PropertyVersions> propertyCollector)
            throws MojoExecutionException, MavenReportException;

    private void renderReport(Locale locale, Sink sink, PropertyUpdatesModel propertyUpdatesModel)
            throws MavenReportException {

        for (String format : this.formats) {
            if ("html".equals(format)) {
                this.rendererFactory
                        .createReportRenderer(getBundleName(), sink, locale, propertyUpdatesModel, allowSnapshots)
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
                new PropertyUpdatesXmlReportRenderer(propertyUpdatesModel, outputFile, allowSnapshots).render();
            }
        }
    }

    private PropertyUpdatesModel getPropertyUpdatesModel(Map<Property, PropertyVersions> updateSet) {
        return new PropertyUpdatesModel(PROPERTIES_COMPARATOR, updateSet);
    }

    protected VersionPropertiesMapRequest getRequest(MavenProject project) {
        return VersionPropertiesMapRequest.builder()
                .withMavenProject(project)
                .withPropertyDefinitions(this.properties)
                .withIncludeProperties(this.includeProperties)
                .withExcludeProperties(this.excludeProperties)
                .withIncludeParent(this.includeParent)
                .withAutoLinkItems(this.autoLinkItems)
                .build();
    }
}
