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
import java.util.Arrays;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.DependencyUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.MavenProjectUtils;
import org.codehaus.mojo.versions.xml.DependencyUpdatesXmlReportRenderer;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.emptyMap;
import static org.codehaus.mojo.versions.utils.MiscUtils.filter;

/**
 * Generates a report of available updates for the dependencies of a project.
 */
public abstract class AbstractDependencyUpdatesReport extends AbstractVersionsReport<DependencyUpdatesModel> {

    private static final DependencyComparator DEPENDENCY_COMPARATOR = DependencyComparator.INSTANCE;

    /**
     * Whether to process the <code>dependencyManagement</code> in pom or not.
     *
     * @since 2.5
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    protected boolean processDependencyManagement;

    /**
     * Whether to process the dependencyManagement part transitive or not.
     * In case of <code>&lt;type&gt;pom&lt;/type&gt;</code>and
     * <code>&lt;scope&gt;import&lt;/scope&gt;</code> this means
     * by default to report also the imported dependencies.
     * If processTransitive is set to <code>false</code> the report will only show
     * updates of the imported pom itself.
     *
     * @since 2.5 Note: Currently in experimental state.
     */
    @Parameter(property = "processDependencyManagementTransitive", defaultValue = "true")
    protected boolean processDependencyManagementTransitive;

    /**
     * <p>Include dependencies with version set in a parent or in a BOM.</p>
     * <p>This is similar to {@code processDependencyManagementTransitive}, but will
     * report updates on dependencies.</p>
     *
     * @since 2.19.0
     */
    @Parameter(property = "showVersionless", defaultValue = "true")
    protected boolean showVersionless = true;

    /**
     * Report formats (html and/or xml). HTML by default.
     */
    @Parameter(property = "dependencyUpdatesReportFormats", defaultValue = "html")
    protected String[] formats = new String[] {"html"};

    /**
     * If <code>true</code>, only shows the subsection of the <code>dependencyManagement</code> artifacts that
     * are actually used in the project's <code>dependency</code> graph. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter(property = "onlyProjectDependencies", defaultValue = "false")
    protected boolean onlyProjectDependencies;

    /**
     * If <code>true</code>, only shows upgradable dependencies in the report. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter(property = "onlyUpgradable", defaultValue = "false")
    protected boolean onlyUpgradable;

    protected AbstractDependencyUpdatesReport(
            I18N i18n,
            ArtifactFactory artifactCreationService,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactCreationService, repositorySystem, wagonMap, rendererFactory);
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
        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    @Override
    protected void doGenerateReport(Locale locale, Sink sink) throws MavenReportException {

        Set<Dependency> dependencies = getDependencies();

        Set<Dependency> dependencyManagement;

        if (processDependencyManagement) {
            dependencyManagement = getDependencyManagement();
            handleOnlyProjectDependencies(dependencyManagement, dependencies);
        } else {
            dependencyManagement = Collections.emptySet();
        }

        try {
            Map<Dependency, ArtifactVersions> dependencyUpdates = getHelper()
                    .lookupDependenciesUpdates(
                            dependencies.stream()
                                    .filter(d -> d.getVersion() != null)
                                    .filter(d ->
                                            showVersionless || MavenProjectUtils.dependencyVersionLocalToReactor(d)),
                            false,
                            allowSnapshots);

            Map<Dependency, ArtifactVersions> dependencyManagementUpdates = processDependencyManagement
                    ? getHelper()
                            .lookupDependenciesUpdates(
                                    dependencyManagement.stream().filter(d -> d.getVersion() != null),
                                    false,
                                    allowSnapshots)
                    : emptyMap();

            if (onlyUpgradable) {
                dependencyUpdates = filter(dependencyUpdates, e -> !e.isEmpty(allowSnapshots));
                dependencyManagementUpdates = filter(dependencyManagementUpdates, e -> !e.isEmpty(allowSnapshots));
            }

            if (getLog().isDebugEnabled()) {
                getLog().debug("Dependency versions:");
                dependencyUpdates.forEach((key, value) -> getLog().debug(key.toString() + ": "
                        + Arrays.stream(value.getVersions(true /* already filtered */))
                                .map(ArtifactVersion::toString)
                                .collect(Collectors.joining(", "))));

                getLog().debug("Dependency management versions:");
                dependencyManagementUpdates.forEach((key, value) -> getLog().debug(key.toString() + ": "
                        + Arrays.stream(value.getVersions(true /* already filtered */))
                                .map(ArtifactVersion::toString)
                                .collect(Collectors.joining(", "))));
            }

            DependencyUpdatesModel model = new DependencyUpdatesModel(dependencyUpdates, dependencyManagementUpdates);

            renderReport(locale, sink, model);
        } catch (VersionRetrievalException e) {
            throw new RuntimeException(e);
        }
    }

    protected void handleDependencyManagementTransitive(
            MavenProject project, Set<Dependency> dependencyManagementCollector) {
        try {
            dependencyManagementCollector.addAll(MavenProjectUtils.extractDependenciesFromDependencyManagement(
                    project, processDependencyManagementTransitive, getLog()));
        } catch (VersionRetrievalException e) {
            throw new RuntimeException(e);
        }
    }

    private void handleOnlyProjectDependencies(Set<Dependency> dependencyManagement, Set<Dependency> dependencies) {
        if (!onlyProjectDependencies) {
            // Retains only dependencies not present in dependencyManagement
            dependencies.removeIf(dep -> dependencyManagement.stream().anyMatch(dmDep -> match(dep, dmDep)));
        } else {
            // Retain only dependencies in dependencyManagement that are also present in dependencies
            dependencyManagement.removeIf(dep -> dependencies.stream().noneMatch(dmDep -> match(dep, dmDep)));
        }
    }

    /**
     * Constructs a final instance of a {@link Set<Dependency>} with a {@link DependencyComparator} comparator.
     * This set can be further populated by implementations, and should contain dependencies, that are present
     * in projects direct dependencies section.
     *
     * @return a {@link Set<Dependency>} that can be additionally populated by {@link #populateDependencies(Set)}.
     * If not, an empty set is returned
     * */
    private Set<Dependency> getDependencies() {
        final Set<Dependency> dependenciesCollector = new TreeSet<>(DEPENDENCY_COMPARATOR);
        populateDependencies(dependenciesCollector);
        return dependenciesCollector;
    }

    /**
     * Implementations of {@link AbstractDependencyUpdatesReport} may use this to supply the main processing logic
     * (see {@link #getDependencyManagement()}) with desired dependency data, which will be used
     * in the creation of the report.
     *
     * @param dependenciesCollector, a Set, initialized with a DependencyComparator
     * comparator.
     * */
    protected abstract void populateDependencies(Set<Dependency> dependenciesCollector);

    /**
     * Constructs a final instance of a {@link Set<Dependency>} with a {@link DependencyComparator} comparator.
     * This set can be further populated by implementations, and should contain dependencies, that are present
     * in projects dependencyManagement section.
     *
     * @return a {@link Set<Dependency>} that can be additionally populated by
     * {@link #populateDependencyManagement(Set)}. If not, an empty set is returned
     * */
    private Set<Dependency> getDependencyManagement() throws MavenReportException {
        final Set<Dependency> dependencyManagementCollector = new TreeSet<>(DEPENDENCY_COMPARATOR);
        populateDependencyManagement(dependencyManagementCollector);
        return dependencyManagementCollector;
    }

    /**
     * Implementations of {@link AbstractDependencyUpdatesReport} may use this to supply the main processing logic
     * (see {@link #getDependencyManagement()}) with desired managed dependencies data, which will be used
     * in the creation of the report.
     *
     * @param dependencyManagementCollector, a Set initialized with a DependencyComparator
     * comparator.
     *
     * @throws MavenReportException when things go wrong.
     * */
    protected abstract void populateDependencyManagement(Set<Dependency> dependencyManagementCollector)
            throws MavenReportException;

    private void renderReport(Locale locale, Sink sink, DependencyUpdatesModel model) throws MavenReportException {
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
                new DependencyUpdatesXmlReportRenderer(model, outputFile, allowSnapshots).render();
            }
        }
    }

    /**
     * Compares two dependencies with each other
     *
     * @return true if the two dependencies match
     */
    private boolean match(Dependency dep, Dependency dmDep) {
        return dmDep.getGroupId().equals(dep.getGroupId())
                && dmDep.getArtifactId().equals(dep.getArtifactId())
                && (dmDep.getScope() == null || dmDep.getScope().equals(dep.getScope()))
                && (dmDep.getClassifier() == null || dmDep.getClassifier().equals(dep.getClassifier()))
                && (dep.getVersion() == null
                        || dmDep.getVersion() == null
                        || dmDep.getVersion().equals(dep.getVersion()));
    }

    protected boolean hasDependencyManagement(MavenProject project) {
        if (project == null) {
            return false;
        }
        return project.getDependencyManagement() != null
                && project.getDependencyManagement().getDependencies() != null;
    }
}
