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

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Extension;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.*;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.filtering.DependencyFilter;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.CoreExtensionUtils;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.ExtensionBuilder;
import org.codehaus.mojo.versions.utils.ModelNode;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.codehaus.plexus.util.StringUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;

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
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 72;

    /**
     * <p>Specifies a comma-separated list of GAV patterns to consider
     * when looking for updates. If the trailing parts of the GAV are omitted, then can assume any value.</p>
     *
     * <p>The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     *
     * Examples: {@code "mygroup:artifact:*"}, {@code "mygroup:artifact"}, {@code "mygroup"}
     *
     * @since 2.15.0
     */
    @Parameter(property = "extensionIncludes", defaultValue = WildcardMatcher.WILDCARD)
    private List<String> extensionIncludes;

    /**
     * <p>Specifies a comma-separated list of GAV patterns to <b>NOT</b> consider
     * when looking for updates. If the trailing parts of the GAV are omitted, then can assume any value.</p>
     *
     * <p>This list is taken into account <u>after</u> {@link #extensionIncludes}</p>.
     *
     * <p>The wildcard "*" can be used as the only, first, last or both characters in each token.
     * The version token does support version ranges.</p>
     *
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
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * to be {@code false}</b></p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    private boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} to be {@code false}</b></p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    private boolean allowIncrementalUpdates = true;

    /**
     * <p>Whether to process core extensions. Default is {@code true}.</p>
     * @since 2.15.0
     */
    @Parameter(property = "processCoreExtensions", defaultValue = "true")
    private boolean processCoreExtensions = true;

    /**
     * <p>Whether to process build extensions. Default is {@code true}.</p>
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
            RepositorySystem repositorySystem,
            org.eclipse.aether.RepositorySystem aetherRepositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders);
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

        Set<Dependency> dependencies;
        try {
            Stream<Extension> extensions;
            if (processCoreExtensions) {
                extensions = CoreExtensionUtils.getCoreExtensions(project);
            } else {
                extensions = Stream.empty();
            }
            if (processBuildExtensions) {
                if (!interpolateProperties) {
                    extensions = Stream.concat(
                            extensions,
                            PomHelper.getChildModels(session.getCurrentProject(), getLog()).values().stream()
                                    .map(Model::getBuild)
                                    .filter(Objects::nonNull)
                                    .map(Build::getExtensions)
                                    .map(List::stream)
                                    .reduce(Stream::concat)
                                    .orElse(Stream.empty()));
                } else {
                    List<ModelNode> rawModels = getRawModels();
                    for (ModelNode node : rawModels) {
                        if (node.getModel() == null) {
                            // unlikely
                            continue;
                        }
                        Map<String, String> properties = new HashMap<>();
                        for (ModelNode p = node; p != null; p = p.getParent().orElse(null)) {
                            p.getModel()
                                    .getProperties()
                                    .forEach((key, value) ->
                                            properties.putIfAbsent(String.valueOf(key), String.valueOf(value)));
                        }
                        extensions = Stream.concat(
                                extensions,
                                Optional.ofNullable(node.getModel().getBuild())
                                        .map(Build::getExtensions)
                                        .orElse(Collections.emptyList())
                                        .stream()
                                        .map(e -> ExtensionBuilder.newBuilder()
                                                .withGroupId(PomHelper.evaluate(e.getGroupId(), properties))
                                                .withArtifactId(PomHelper.evaluate(e.getArtifactId(), properties))
                                                .withVersion(PomHelper.evaluate(e.getVersion(), properties))
                                                .build()));
                    }
                }
            }

            dependencies = extensions
                    .map(e -> DependencyBuilder.newBuilder()
                            .withGroupId(e.getGroupId())
                            .withArtifactId(e.getArtifactId())
                            .withVersion(e.getVersion())
                            .build())
                    .filter(includeFilter::matchersMatch)
                    .filter(excludeFilter::matchersDontMatch)
                    .collect(Collectors.toSet());
        } catch (IOException | XmlPullParserException e) {
            throw new MojoExecutionException(e.getMessage());
        }
        if (dependencies.isEmpty()) {
            getLog().info("Extensions set filtered by include- and exclude-filters is empty. Nothing to do.");
            return;
        }

        try {
            logUpdates(getHelper().lookupDependenciesUpdates(dependencies, true, true, allowSnapshots));
        } catch (VersionRetrievalException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private List<ModelNode> getRawModels() throws MojoFailureException {
        List<ModelNode> rawModels;
        try {
            ModifiedPomXMLEventReader pomReader = newModifiedPomXER(
                    new StringBuilder(
                            new String(Files.readAllBytes(getProject().getFile().toPath()))),
                    getProject().getFile().toPath().toString());
            ModelNode rootNode = new ModelNode(PomHelper.getRawModel(pomReader), pomReader);
            rawModels = PomHelper.getRawModelTree(rootNode, getLog());
        } catch (IOException e) {
            throw new MojoFailureException(e.getMessage(), e);
        }
        return rawModels;
    }

    private Optional<Segment> calculateUpdateScope() {
        return of(SegmentUtils.determineUnchangedSegment(
                        allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog())
                .map(Segment::minorTo)
                .orElse(MAJOR));
    }

    private void logUpdates(Map<Dependency, ArtifactVersions> updates) {
        List<String> withUpdates = new ArrayList<>();
        List<String> usingCurrent = new ArrayList<>();
        for (ArtifactVersions versions : updates.values()) {
            String left = "  " + ArtifactUtils.versionlessKey(versions.getArtifact()) + " ";
            final String current;
            ArtifactVersion latest;
            if (versions.getCurrentVersion() != null) {
                current = versions.getCurrentVersion().toString();
                latest = versions.getNewestUpdateWithinSegment(calculateUpdateScope(), allowSnapshots);
            } else {
                ArtifactVersion newestVersion =
                        versions.getNewestVersion(versions.getArtifact().getVersionRange(), allowSnapshots);
                current = versions.getArtifact().getVersionRange().toString();
                latest = newestVersion == null
                        ? null
                        : versions.getNewestUpdateWithinSegment(newestVersion, calculateUpdateScope(), allowSnapshots);
                if (latest != null
                        && ArtifactVersions.isVersionInRange(
                                latest, versions.getArtifact().getVersionRange())) {
                    latest = null;
                }
            }
            String right = " " + (latest == null ? current : current + " -> " + latest);
            List<String> t = latest == null ? usingCurrent : withUpdates;
            if (right.length() + left.length() + 3 > INFO_PAD_SIZE + getOutputLineWidthOffset()) {
                t.add(left + "...");
                t.add(StringUtils.leftPad(right, INFO_PAD_SIZE + getOutputLineWidthOffset()));

            } else {
                t.add(StringUtils.rightPad(left, INFO_PAD_SIZE + getOutputLineWidthOffset() - right.length(), ".")
                        + right);
            }
        }

        if (verbose) {
            if (usingCurrent.isEmpty()) {
                if (!withUpdates.isEmpty()) {
                    logLine(false, "No extensions are using the newest version.");
                    logLine(false, "");
                }
            } else {
                logLine(false, "The following extensions are using the newest version:");
                for (String s : usingCurrent) {
                    logLine(false, s);
                }
                logLine(false, "");
            }
        }

        if (withUpdates.isEmpty()) {
            if (!usingCurrent.isEmpty()) {
                logLine(false, "No extensions have newer versions.");
                logLine(false, "");
            }
        } else {
            logLine(false, "The following extensions have newer versions:");
            for (String withUpdate : withUpdates) {
                logLine(false, withUpdate);
            }
            logLine(false, "");
        }
    }

    /**
     * @param pom the pom to update.
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    @Override
    protected void update(ModifiedPomXMLEventReader pom) {
        // do nothing
    }
}
