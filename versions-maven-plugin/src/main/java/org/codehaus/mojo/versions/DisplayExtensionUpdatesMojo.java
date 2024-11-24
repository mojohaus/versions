package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.TransformerException;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Extension;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.filtering.DependencyFilter;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.internal.DependencyUpdatesLoggingHelper;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.ExtensionUtils;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

/**
 * Displays all build and core extensions that have newer versions available.
 *
 * @author Andrzej Jarmoniuk
 * @since 2.15.0
 */
@Mojo(name = "display-extension-updates", aggregator = true, threadSafe = true)
public class DisplayExtensionUpdatesMojo extends AbstractVersionsDisplayMojo {

    // ------------------------------ FIELDS ------------------------------

    /**
     * <p>Specifies a comma-separated list of GAV patterns to consider
     * when looking for updates. If the trailing parts of the GAV are omitted, then can assume any value.</p>
     * <p>The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     * Examples: {@code "mygroup:artifact:*"}, {@code "mygroup:artifact"}, {@code "mygroup"}
     *
     * @since 2.15.0
     */
    @Parameter(property = "extensionIncludes", defaultValue = WildcardMatcher.WILDCARD)
    private List<String> extensionIncludes;

    /**
     * <p>Specifies a comma-separated list of GAV patterns to <b>NOT</b> consider
     * when looking for updates. If the trailing parts of the GAV are omitted, then can assume any value.</p>
     * <p>This list is taken into account <u>after</u> {@link #extensionIncludes}</p>.
     * <p>The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     * Examples: {@code "mygroup:artifact:*"}, {@code "mygroup:artifact"}, {@code "mygroup"}
     *
     * @since 2.15.0
     */
    @Parameter(property = "extensionExcludes")
    private List<String> extensionExcludes;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    private boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * to be {@code false}</b></p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    private boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} to be {@code false}</b></p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    private boolean allowIncrementalUpdates = true;

    /**
     * <p>Whether to process core extensions. Default is {@code true}.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "processCoreExtensions", defaultValue = "true")
    private boolean processCoreExtensions = true;

    /**
     * <p>Whether to process build extensions. Default is {@code true}.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "processBuildExtensions", defaultValue = "true")
    private boolean processBuildExtensions = true;

    /**
     * If set to {@code false}, the plugin will not interpolate property values when looking for versions
     * to be changed, but will instead operate on raw model.
     *
     * @since 2.15.0
     */
    @Parameter(property = "interpolateProperties", defaultValue = "true")
    protected boolean interpolateProperties = true;

    /**
     * Whether to show additional information such as extensions that do not need updating. Defaults to false.
     *
     * @since 2.15.0
     */
    @Parameter(property = "verbose", defaultValue = "false")
    private boolean verbose;

    @Inject
    public DisplayExtensionUpdatesMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        logInit();
        validateInput();

        if (!processCoreExtensions && !processBuildExtensions) {
            getLog().info("Neither core nor build extensions are to be processed. Nothing to do.");
            return;
        }

        DependencyFilter includeFilter = DependencyFilter.parseFrom(extensionIncludes);
        DependencyFilter excludeFilter = DependencyFilter.parseFrom(extensionExcludes);

        try {
            Stream<Extension> coreExtensions =
                    processCoreExtensions ? ExtensionUtils.getCoreExtensions(project) : Stream.empty();
            Stream<Extension> buildExtensions = processBuildExtensions
                    ? ExtensionUtils.getBuildExtensions(getProject(), getLog(), interpolateProperties)
                    : Stream.empty();

            Collection<Dependency> dependencies = Stream.concat(coreExtensions, buildExtensions)
                    .map(e -> DependencyBuilder.newBuilder()
                            .withGroupId(e.getGroupId())
                            .withArtifactId(e.getArtifactId())
                            .withVersion(e.getVersion())
                            .build())
                    .filter(includeFilter::matchersMatch)
                    .filter(excludeFilter::matchersDontMatch)
                    .collect(Collectors.toSet());

            if (dependencies.isEmpty()) {
                getLog().info("Extensions set filtered by include- and exclude-filters is empty. Nothing to do.");
                return;
            }

            logUpdates(getHelper().lookupDependenciesUpdates(dependencies.stream(), true, true, allowSnapshots));
        } catch (IOException | XMLStreamException | TransformerException e) {
            throw new MojoExecutionException(e.getMessage());
        } catch (VersionRetrievalException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void logUpdates(Map<Dependency, ArtifactVersions> versionMap) {
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog());
        DependencyUpdatesLoggingHelper.DependencyUpdatesResult updates =
                DependencyUpdatesLoggingHelper.getDependencyUpdates(
                        getProject(),
                        versionMap,
                        allowSnapshots,
                        unchangedSegment,
                        INFO_PAD_SIZE + getOutputLineWidthOffset(),
                        verbose);

        if (verbose) {
            if (updates.getUsingLatest().isEmpty()) {
                if (!updates.getWithUpdates().isEmpty()) {
                    logLine(false, "No extensions are using the newest version.");
                    logLine(false, "");
                }
            } else {
                logLine(false, "The following extensions are using the newest version:");
                updates.getUsingLatest().forEach(s -> logLine(false, s));
                logLine(false, "");
            }
        }

        if (updates.getWithUpdates().isEmpty()) {
            if (!updates.getUsingLatest().isEmpty()) {
                logLine(false, "No extensions have newer versions.");
                logLine(false, "");
            }
        } else {
            logLine(false, "The following extensions have newer versions:");
            updates.getWithUpdates().forEach(s -> logLine(false, s));
            logLine(false, "");
        }
    }

    /**
     * @param pom the pom to update.
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     * @since 1.0-alpha-1
     */
    @Override
    protected void update(MutableXMLStreamReader pom) {
        // do nothing
    }
}
