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

import java.io.IOException;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.ModelBase;
import org.apache.maven.model.Profile;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.mojo.versions.utils.ModelNode;
import org.eclipse.aether.RepositorySystem;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Optional.ofNullable;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.DEPENDENCY_MANAGEMENT_UPDATE;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.DEPENDENCY_UPDATE;
import static org.codehaus.mojo.versions.model.DependencyChangeKind.PARENT_UPDATE;

/**
 * Updates a dependency to a specific version.
 * This can be useful if you have to manage versions for a very large (100+ module) projects where you can’t always use
 * the most up-to-date version of a particular third party component.
 *
 * @author Dan Arcari
 * @since 2.3
 */
@Mojo(name = "use-dep-version", aggregator = true, threadSafe = true)
public class UseDepVersionMojo extends AbstractVersionsDependencyUpdaterMojo {

    /**
     * The exact version to be applied for the included dependencies
     */
    @Parameter(property = "depVersion", required = true)
    protected String depVersion;

    /**
     * If set to {@code true}, will use whatever version is supplied without attempting to validate that such
     * a version is obtainable from the repository chain.
     */
    @Parameter(property = "forceVersion", defaultValue = "false")
    protected boolean forceVersion;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter(property = "processParent", defaultValue = "false")
    private boolean processParent = false;

    @Override
    protected final boolean getProcessDependencies() {
        return processDependencies;
    }

    @Override
    protected final boolean getProcessDependencyManagement() {
        return processDependencyManagement;
    }

    @Override
    public final boolean getProcessParent() {
        return processParent;
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * <p>Will augment normal processing by, if a dependency value is set using a property, trying to update
     * the value of the property.</p>
     * <p>If the property value is specified directly, will process it normally (as with {@code processProperties} being
     * {@code false}. If the property being updated is redefined in the reactor tree, will only change the property
     * value which lies closest to the dependency being updated. If the same property is also used to set
     * the value of another dependency, will not update that property value, and log a warning instead.
     * Finally, if the property value is specified in a parent file which is outside of the project, will log
     * a message.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "processProperties", defaultValue = "false")
    protected boolean processProperties;

    /**
     * Creates a new instance.
     *
     * @param artifactFactory  the artifact factory
     * @param repositorySystem the repository system
     * @param wagonMap         the map of wagon implementations
     * @param changeRecorderFactories  the change recorder factories
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public UseDepVersionMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected void validateInput() throws MojoExecutionException {
        super.validateInput();
        if (StringUtils.isBlank(depVersion)) {
            throw new IllegalArgumentException(
                    "depVersion must be supplied with use-specific-version, and cannot be blank.");
        }

        if (!forceVersion && !hasIncludes()) {
            throw new IllegalArgumentException(
                    "The use-specific-version goal is intended to be used with a single artifact. "
                            + "Please specify a value for the 'includes' parameter, "
                            + "or use -DforceVersion=true to override this check.");
        }
    }

    @Override
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        // not used
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        validateInput();
        List<ModelNode> rawModels;

        try {
            MutableXMLStreamReader pomReader =
                    new MutableXMLStreamReader(getProject().getFile().toPath());
            ModelNode rootNode = new ModelNode(
                    PomHelper.getRawModel(pomReader.getSource(), getProject().getFile()), pomReader);
            rawModels = PomHelper.getRawModelTree(rootNode, getLog());
            // reversing to process depth-first
            Collections.reverse(rawModels);

            Set<String> propertyBacklog = new HashSet<>();
            Map<String, Set<Dependency>> propertyConflicts = new HashMap<>();
            for (ModelNode node : rawModels) {
                processModel(
                        node,
                        propertyBacklog,
                        propertyConflicts,
                        ofNullable(pomReader.getEncoding())
                                .map(Charset::forName)
                                .orElse(Charset.defaultCharset()));
            }
            propertyBacklog.forEach(p -> getLog().warn("Not updating property ${" + p + "}: defined in parent"));
        } catch (IOException | XMLStreamException e) {
            throw new MojoFailureException(e.getMessage(), e);
        } catch (RuntimeException e) {
            if (e.getCause() instanceof MojoFailureException) {
                throw (MojoFailureException) e.getCause();
            } else if (e.getCause() instanceof MojoExecutionException) {
                throw (MojoExecutionException) e.getCause();
            }
            throw e;
        }
    }

    /**
     * Processes a single model and POM file associated with it
     * @param node tree node to process
     * @param propertyBacklog a {@link Set} instance used to store dependencies to be updated, but which were not found
     *                        in the current subtree. These properties need to be carried over to the parent node for
     *                        processing.
     * @param propertyConflicts an {@link Map} instance to store properties
     *                          which are associated with dependencies which do not fit the filter and thus may not
     *                          be changed. This is then used for conflict detection if a dependency to be changed
     *                          used one of these properties. Such a change is not allowed and must be reported instead.
     * @param charset           charset for file writing
     * @return {@code true} if the file has been changed
     * @throws MojoFailureException thrown if a version may not be changed
     * @throws MojoExecutionException thrown if a version may not be changed
     */
    protected boolean processModel(
            ModelNode node,
            Set<String> propertyBacklog,
            Map<String, Set<Dependency>> propertyConflicts,
            Charset charset)
            throws MojoFailureException, MojoExecutionException {
        // 1) process the properties carried over from children
        propertyBacklog.removeIf(p -> updatePropertyValue(node, p));

        // 2) process dependencies and properties from this node
        try {
            if (getProcessDependencyManagement() && node.getModel().getDependencyManagement() != null) {
                useDepVersion(
                        node,
                        node.getModel().getDependencyManagement().getDependencies(),
                        DEPENDENCY_MANAGEMENT_UPDATE,
                        propertyBacklog,
                        propertyConflicts);
            }

            if (getProcessDependencies()) {
                useDepVersion(
                        node, getDependencies(node.getModel()), DEPENDENCY_UPDATE, propertyBacklog, propertyConflicts);
            }

            if (getProject().getParent() != null && getProcessParent()) {
                useDepVersion(
                        node, singletonList(getParentDependency()), PARENT_UPDATE, propertyBacklog, propertyConflicts);
            }
        } catch (XMLStreamException e) {
            throw new MojoFailureException(
                    "Unable to parse the pom " + node.getModel().getPomFile(), e);
        } catch (VersionRetrievalException e) {
            throw new MojoFailureException(
                    "Unable to retrieve dependency versions "
                            + e.getArtifact().map(a -> "of " + a).orElse("")
                            + " while processing "
                            + node.getModel().getPomFile(),
                    e);
        }

        if (node.getMutableXMLStreamReader().isModified()) {
            if (generateBackupPoms) {
                Objects.requireNonNull(node.getModel().getPomFile());
                Objects.requireNonNull(node.getModel().getPomFile().toPath().getParent());
                Path backupFile = node.getModel()
                        .getPomFile()
                        .toPath()
                        .getParent()
                        .resolve(node.getModel().getPomFile().toPath().getFileName() + ".versionsBackup");
                if (!Files.exists(backupFile)) {
                    if (getLog().isDebugEnabled()) {
                        getLog().debug("Backing up " + node.getModel().getPomFile() + " to " + backupFile);
                    }
                    try {
                        Files.copy(node.getModel().getPomFile().toPath(), backupFile, REPLACE_EXISTING);
                    } catch (IOException e) {
                        throw new MojoFailureException(
                                "Error backing up the " + node.getModel().getPomFile(), e);
                    }
                } else {
                    if (getLog().isDebugEnabled()) {
                        getLog().debug("Leaving existing backup " + backupFile + " unmodified");
                    }
                }
            } else {
                getLog().debug("Skipping the generation of a backup file");
            }
            try (Writer writer =
                    Files.newBufferedWriter(node.getModel().getPomFile().toPath(), charset)) {
                writer.write(node.getMutableXMLStreamReader().getSource());
            } catch (IOException e) {
                throw new MojoFailureException(
                        "Unable to write the changed file " + node.getModel().getPomFile(), e);
            }
        }

        try {
            saveChangeRecorderResults();
        } catch (IOException e) {
            getLog().warn(
                            "Cannot save the change recorder result for file "
                                    + node.getModel().getPomFile(),
                            e);
        }

        return node.getMutableXMLStreamReader().isModified();
    }

    private static List<Dependency> getDependencies(Model model) {
        List<Dependency> dependencies = ofNullable(model.getDependencies()).orElse(new ArrayList<>());
        dependencies.addAll(ofNullable(model.getProfiles())
                .flatMap(profiles -> profiles.stream()
                        .map(ModelBase::getDependencies)
                        .reduce((l1, l2) -> {
                            l1.addAll(l2);
                            return l1;
                        }))
                .orElse(emptyList()));
        return dependencies;
    }

    /**
     * <p>Will process the given module tree node, updating the {@link MutableXMLStreamReader} associated with the
     * node if it finds a dependency matching the filter that needs to be changed or, if {@link #processProperties}
     * is {@code true}, a property value that can be updated.</p>
     * <p>The method will use the set passed as the {@code backlog} argument to store the properties which it needs
     * to update, but which were not found in the current tree. These properties need to be carried over to the parent
     * node for processing.</p>
     * <p>Similarly, method will use the map passed as the {@code propertyConflicts} argument to store properties
     * which are associated with dependencies which do not fit the filter and thus may not be changed. This is then
     * used for conflict detection if a dependency to be changed used one of these properties. Such a change
     * is not allowed and must be reported instead.</p>
     *
     * @param node model tree node to process
     * @param dependencies collection of dependencies to process (can be taken from dependency management,
     *                     parent, or dependencies)
     * @param propertyBacklog a {@link Set} instance used to store dependencies to be updated, but which were not found
     *                        in the current subtree. These properties need to be carried over to the parent node for
     *                        processing.
     * @param propertyConflicts an {@link Map} instance to store properties
     *                          which are associated with dependencies which do not fit the filter and thus may not
     *                          be changed. This is then used for conflict detection if a dependency to be changed
     *                          used one of these properties. Such a change is not allowed and must be reported instead.
     * @throws MojoExecutionException thrown if a version may not be changed
     * @throws XMLStreamException thrown if a {@link MutableXMLStreamReader} can't be updated
     * @throws VersionRetrievalException thrown if dependency versions cannot be retrieved
     */
    private void useDepVersion(
            ModelNode node,
            Collection<Dependency> dependencies,
            DependencyChangeKind changeKind,
            Set<String> propertyBacklog,
            Map<String, Set<Dependency>> propertyConflicts)
            throws MojoExecutionException, XMLStreamException, VersionRetrievalException {
        // an additional pass is necessary to collect conflicts if processProperties is enabled
        if (processProperties) {
            dependencies.stream()
                    .filter(dep -> !isIncluded(toArtifact(dep)))
                    .forEach(dep ->
                            // if a dependency that is _not_ to be changed is set using a property, register that
                            // property
                            // in propertyConflicts; these are the properties that must not be changed
                            // the list in the value is the list of dependencies that use the said property
                            PomHelper.extractExpression(dep.getVersion())
                                    .ifPresent(p -> propertyConflicts.compute(p, (k, v) -> ofNullable(v)
                                            .map(set -> {
                                                set.add(dep);
                                                return set;
                                            })
                                            .orElseGet(() -> {
                                                Set<Dependency> set = new TreeSet<>(DependencyComparator.INSTANCE);
                                                set.add(dep);
                                                return set;
                                            }))));
        }

        // 2nd pass: check dependencies
        for (Dependency dep : dependencies) {
            if (getExcludeReactor() && isProducedByReactor(dep)) {
                getLog().info("Ignoring a reactor dependency: " + toString(dep));
                continue;
            }

            Optional<String> propertyName = PomHelper.extractExpression(dep.getVersion());
            if (propertyName.isPresent() && !processProperties) {
                getLog().info("Ignoring a dependency with the version set using a property: " + toString(dep));
                continue;
            }

            Artifact artifact = toArtifact(dep);
            if (isIncluded(artifact)) {
                if (dep.getVersion() == null) {
                    getLog().warn(String.format(
                            "Not updating %s:%s in dependencies: version defined " + "in dependencyManagement",
                            dep.getGroupId(), dep.getArtifactId()));
                } else {
                    if (!forceVersion) {
                        if (!getHelper().lookupArtifactVersions(artifact, false).containsVersion(depVersion)) {
                            throw new MojoExecutionException(String.format(
                                    "Version %s is not available for artifact %s:%s",
                                    depVersion, artifact.getGroupId(), artifact.getArtifactId()));
                        }
                    }
                    if (!propertyName.isPresent()) {
                        updateDependencyVersion(
                                node.getMutableXMLStreamReader(),
                                new DependencyVersionChange()
                                        .withKind(changeKind)
                                        .withGroupId(dep.getGroupId())
                                        .withArtifactId(dep.getArtifactId())
                                        .withOldVersion(dep.getVersion())
                                        .withNewVersion(depVersion));
                    } else {
                        // propertyName is present
                        ofNullable(propertyConflicts.get(propertyName.get()))
                                .map(conflict -> {
                                    getLog().warn("Cannot update property ${" + propertyName.get() + "}: "
                                            + "controls more than one dependency: "
                                            + conflict.stream()
                                                    .map(Dependency::getArtifactId)
                                                    .collect(Collectors.joining(", ")));
                                    return false;
                                })
                                .orElseGet(() -> {
                                    if (!updatePropertyValue(node, propertyName.get())) {
                                        propertyBacklog.add(propertyName.get());
                                    } else {
                                        if (getLog().isDebugEnabled()) {
                                            getLog().debug(String.format(
                                                    "Updated the %s property value to %s.",
                                                    propertyName.get(), depVersion));
                                        }
                                    }
                                    return true;
                                });
                    }
                }
            }
        }

        // third pass: if a property is defined at this node, it is not going to conflict with anything from parent
        propertyConflicts.keySet().removeIf(key -> ofNullable(node.getModel().getProperties())
                .filter(p -> p.containsKey(key))
                .isPresent());
        propertyConflicts.keySet().removeIf(key -> ofNullable(node.getModel().getProfiles())
                .map(list -> list.stream().anyMatch(p -> ofNullable(p.getProperties())
                        .filter(prop -> prop.containsKey(key))
                        .isPresent()))
                .orElse(false));
    }

    private boolean updatePropertyValue(ModelNode node, String property) {
        // concatenating properties from the main build section
        // with properties from profiles
        return Stream.concat(
                        Stream.of(node.getModel().getProperties().getProperty(property))
                                .filter(Objects::nonNull)
                                .map(value -> new ImmutablePair<Profile, String>(null, value)),
                        node.getModel().getProfiles().stream()
                                .map(profile -> new ImmutablePair<>(profile, profile.getProperties()))
                                .map(pair -> ofNullable(pair.getRight().getProperty(property))
                                        .map(value -> new ImmutablePair<>(pair.getLeft(), value))
                                        .orElse(null)))
                // and processing them
                .filter(Objects::nonNull)
                .map(pair -> {
                    try {
                        boolean result = PomHelper.setPropertyVersion(
                                node.getMutableXMLStreamReader(),
                                ofNullable(pair.getLeft()).map(Profile::getId).orElse(null),
                                property,
                                depVersion);
                        if (result) {
                            getChangeRecorder()
                                    .recordChange(new PropertyVersionChange()
                                            .withProperty(property)
                                            .withOldValue(pair.getRight())
                                            .withNewValue(depVersion));
                        }
                        return result;
                    } catch (XMLStreamException e) {
                        throw new RuntimeException(e);
                    }
                })
                .reduce(Boolean::logicalOr)
                .orElse(false);
    }
}
