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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.ScopeArtifactFilter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.recording.DefaultDependencyChangeRecord;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.eclipse.aether.RepositorySystem;

/**
 * Base class for a mojo that updates dependency versions.
 *
 * @author Paul Gier
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public abstract class AbstractVersionsDependencyUpdaterMojo extends AbstractVersionsUpdaterMojo {
    private static final Pattern VERSION_PROPERTY_REGEX = Pattern.compile("^\\$\\{([^}]+)}$");

    private static final String END_RANGE_CHARS = "])";

    private static final String START_RANGE_CHARS = "[(";

    /**
     * Pattern to match snapshot versions
     */
    protected static final Pattern SNAPSHOT_REGEX = Pattern.compile("^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$");

    /**
     * A comma separated list of artifact patterns to include. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifying the set of includes from the command
     * line. When specifying includes from the pom, use the {@link #includes} configuration instead. If this property is
     * specified then the {@link #includes} configuration is ignored.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "includes")
    private String includesList = null;

    /**
     * A comma separated list of artifact patterns to exclude. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifying the set of excludes from the command
     * line. When specifying excludes from the pom, use the {@link #excludes} configuration instead. If this property is
     * specified then the {@link #excludes} configuration is ignored.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "excludes")
    private String excludesList = null;

    /**
     * A list of artifact patterns to include. Follows the pattern "groupId:artifactId:type:classifier:version". This
     * configuration setting is ignored if {@link #includesList} is defined.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private String[] includes = null;

    /**
     * A list of artifact patterns to exclude. Follows the pattern "groupId:artifactId:type:classifier:version". This
     * configuration setting is ignored if {@link #excludesList} is defined.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private String[] excludes = null;

    /**
     * a scope to use to filter the artifacts matching the asked scope (as well as the ones implied by maven)
     *
     * @since 2.15
     */
    @Parameter(property = "scope")
    private String scope = null;

    /**
     * Whether to skip processing dependencies that are produced as part of the current reactor.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "excludeReactor", defaultValue = "true")
    private boolean excludeReactor = true;

    /**
     * The reactor projects for the current build.
     * @param artifactFactory {@link ArtifactFactory} bean instance
     * @param repositorySystem {@link RepositorySystem} bean instance
     * @param wagonMap a map of {@link Wagon} instances per protocol
     * @param changeRecorders a map of change recorders
     * @throws MojoExecutionException thrown if an error occurs
     */
    @Inject
    protected AbstractVersionsDependencyUpdaterMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * Should the project/dependencies section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/dependencies section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    protected abstract boolean getProcessDependencies();

    /**
     * Should the project/dependencyManagement section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/dependencyManagement section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    protected abstract boolean getProcessDependencyManagement();

    /**
     * Should the project/parent section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/parent section of the pom should be processed.
     * @since 2.3
     */
    public abstract boolean getProcessParent();

    /**
     * Should the artifacts produced in the current reactor be excluded from processing.
     *
     * @return returns <code>true</code> if the artifacts produced in the current reactor
     * should be excluded from processing.
     * @since 1.0-alpha-3
     */
    public boolean getExcludeReactor() {
        return excludeReactor;
    }

    /**
     * Should the dependency be updated itself or is it handled by properties.
     *
     * @param dependency Dependency
     * @return true if the version starts with '${'
     * @since 2.8
     */
    protected boolean isHandledByProperty(Dependency dependency) {
        String version = dependency.getVersion();
        return version != null && version.startsWith("${");
    }

    /**
     * Finds the properties of the given raw model that hold the version of dependencies and can safely be rewritten
     * as a whole: the property is defined in this pom, its value passes the filter, every dependency whose version is
     * exactly that property is processed by the goal, and nothing else in the pom refers to the property. Properties
     * that pass the value filter but are shared with something that is not processed are logged and skipped.
     *
     * @param pom the pom being updated
     * @param rawModel the raw (uninterpolated) model of the pom
     * @param valueFilter selects the property values of interest
     * @return the dependencies per property name, each with its version set to the property value
     * @throws MojoExecutionException when a dependency cannot be converted to an artifact
     */
    Map<String, List<Dependency>> findVersionProperties(
            MutableXMLStreamReader pom, Model rawModel, Predicate<String> valueFilter) throws MojoExecutionException {
        List<Dependency> candidates = new ArrayList<>();
        DependencyManagement dependencyManagement = rawModel.getDependencyManagement();
        if (getProcessDependencyManagement() && dependencyManagement != null) {
            candidates.addAll(dependencyManagement.getDependencies());
        }
        if (getProcessDependencies()) {
            candidates.addAll(rawModel.getDependencies());
        }

        Map<String, List<Dependency>> byProperty = new LinkedHashMap<>();
        for (Dependency candidate : candidates) {
            Matcher matcher = VERSION_PROPERTY_REGEX.matcher(String.valueOf(candidate.getVersion()));
            if (!matcher.matches()) {
                continue;
            }
            String value = rawModel.getProperties().getProperty(matcher.group(1));
            if (value == null || !valueFilter.test(value.trim())) {
                continue;
            }
            Dependency dep = candidate.clone();
            dep.setVersion(value.trim());
            byProperty.computeIfAbsent(matcher.group(1), k -> new ArrayList<>()).add(dep);
        }

        String source = pom.getSource();
        Map<String, List<Dependency>> result = new LinkedHashMap<>();
        for (Map.Entry<String, List<Dependency>> entry : byProperty.entrySet()) {
            boolean allProcessed = true;
            for (Dependency dep : entry.getValue()) {
                if ((getExcludeReactor() && isProducedByReactor(dep)) || !isIncluded(toArtifact(dep))) {
                    allProcessed = false;
                    break;
                }
            }
            String reference = "${" + entry.getKey() + "}";
            int references = 0;
            for (int i = source.indexOf(reference); i >= 0; i = source.indexOf(reference, i + 1)) {
                references++;
            }
            if (!allProcessed || references != entry.getValue().size()) {
                getLog().info("Leaving property " + entry.getKey()
                        + " unchanged: it is also used by something this goal does not process");
                continue;
            }
            result.put(entry.getKey(), entry.getValue());
        }
        return result;
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency Dependency
     * @return Artifact
     * @since 1.0-alpha-3
     */
    protected Optional<Artifact> findArtifact(Dependency dependency) {
        return Optional.ofNullable(getProject().getDependencyArtifacts())
                // no mutation, so parallelStream is safe
                .flatMap(artifacts -> artifacts.parallelStream()
                        .filter(artifact -> compare(artifact, dependency))
                        .findAny());
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency Dependency
     * @return Artifact
     * @throws MojoExecutionException Mojo execution exception
     * @since 1.0-alpha-3
     */
    protected Artifact toArtifact(Dependency dependency) throws MojoExecutionException {
        return findArtifact(dependency).orElse(artifactFactory.createArtifact(dependency));
    }

    /**
     * Try to find the artifact that matches the given parent model.
     * @param model Parent model
     * @return Artifact for the parent
     * @throws MojoExecutionException thrown if the dependency cannot be converted to an artifacted
     */
    protected Artifact toArtifact(Parent model) throws MojoExecutionException {
        return this.toArtifact(DependencyBuilder.newBuilder()
                .withGroupId(model.getGroupId())
                .withArtifactId(model.getArtifactId())
                .withVersion(model.getVersion())
                .withType("pom")
                .withScope(Artifact.SCOPE_COMPILE)
                .build());
    }

    /**
     * Returns the {@link Dependency} instance for the parent project
     * @return {@link Dependency} object for the parent
     */
    protected Dependency getParentDependency() {
        return DependencyBuilder.newBuilder()
                .withGroupId(getProject().getParent().getGroupId())
                .withArtifactId(getProject().getParent().getArtifactId())
                .withVersion(getProject().getParent().getVersion())
                .withType("pom")
                .build();
    }

    /**
     * Returns a string representation of the project in the form groupId:artifactId:version (omitting version if it is
     * null or empty).
     *
     * @param project MavenProject
     * @return String representation of the project
     */
    protected String toString(MavenProject project) {
        StringBuilder buf = new StringBuilder();

        buf.append(project.getGroupId());
        buf.append(':');
        buf.append(project.getArtifactId());

        if (project.getVersion() != null && !project.getVersion().isEmpty()) {
            buf.append(":");
            buf.append(project.getVersion());
        }

        return buf.toString();
    }

    /**
     * Returns a string representation of the dependency in the form groupId:artifactId:type:classifier:version
     * (omitting type and classifier if they are null or empty, and omitting version if it is null or empty).
     *
     * @param d Dependency
     * @return String representation of the dependency
     */
    protected String toString(Dependency d) {
        StringBuilder buf = new StringBuilder();
        buf.append(d.getGroupId());
        buf.append(':');
        buf.append(d.getArtifactId());
        if (d.getType() != null && !d.getType().isEmpty()) {
            buf.append(':');
            buf.append(d.getType());
        } else {
            buf.append(":jar");
        }
        if (d.getClassifier() != null && !d.getClassifier().isEmpty()) {
            buf.append(':');
            buf.append(d.getClassifier());
        }
        if (d.getVersion() != null && !d.getVersion().isEmpty()) {
            buf.append(":");
            buf.append(d.getVersion());
        }
        return buf.toString();
    }

    /**
     * Returns <code>true</code> if the dependency is produced by the current reactor.
     *
     * @param dependency the dependency to heck.
     * @return <code>true</code> if the dependency is produced by the current reactor.
     * @since 1.0-alpha-3
     */
    protected boolean isProducedByReactor(Dependency dependency) {
        return reactorProjects.stream().anyMatch(reactorProject -> compare(reactorProject, dependency));
    }

    /**
     * Compare a project to a dependency. Returns true only if the groupId and artifactId are all equal.
     *
     * @param project the project
     * @param dep     the dependency
     * @return true if project and dep refer to the same artifact
     */
    private boolean compare(MavenProject project, Dependency dep) {
        if (!Objects.equals(project.getGroupId(), dep.getGroupId())) {
            return false;
        }
        return project.getArtifactId().equals(dep.getArtifactId());
    }

    /**
     * Compare and artifact to a dependency. Returns true only if the groupId, artifactId, type, and classifier are all
     * equal.
     *
     * @param artifact Artifact
     * @param dep      Dependency
     * @return true if artifact and dep refer to the same artifact
     */
    private boolean compare(Artifact artifact, Dependency dep) {
        if (!Objects.equals(artifact.getGroupId(), dep.getGroupId())) {
            return false;
        }
        if (!Objects.equals(artifact.getArtifactId(), dep.getArtifactId())) {
            return false;
        }
        if (!Objects.equals(artifact.getType(), dep.getType())) {
            return false;
        }
        return Objects.equals(artifact.getClassifier(), dep.getClassifier());
    }

    /**
     * Determine if the artifact is included in the list of artifacts to be processed.
     *
     * @param artifact The artifact we want to check.
     * @return true if the artifact should be processed, false otherwise.
     */
    protected boolean isIncluded(Artifact artifact) {
        return includedArtifacts().include(artifact);
    }

    /**
     * Returns the includes, excludes and scope as one filter, for callers that check many artifacts.
     *
     * @return the filter {@link #isIncluded(Artifact)} applies
     */
    protected ArtifactFilter includedArtifacts() {
        ArtifactFilter includesFilter = getIncludesArtifactFilter();
        ArtifactFilter excludesFilter = getExcludesArtifactFilter();
        ArtifactFilter scopeFilter = getScopeArtifactFilter();
        return artifact -> (includesFilter == null || includesFilter.include(artifact))
                && (excludesFilter == null || excludesFilter.include(artifact))
                && (scopeFilter == null || scopeFilter.include(artifact));
    }

    /**
     * Indicates whether any includes were specified via the 'includes' or 'includesList' options.
     *
     * @return true if includes were specified, false otherwise.
     */
    protected boolean hasIncludes() {
        return includes != null || includesList != null;
    }

    private ArtifactFilter getIncludesArtifactFilter() {
        if (includes == null && includesList == null) {
            return null;
        }

        List<String> patterns = new ArrayList<>();
        if (this.includesList != null) {
            patterns.addAll(separatePatterns(includesList));
        } else {
            patterns.addAll(Arrays.asList(includes));
        }
        return new PatternIncludesArtifactFilter(patterns);
    }

    private ArtifactFilter getExcludesArtifactFilter() {
        if (excludes == null && excludesList == null) {
            return null;
        }

        List<String> patterns = new ArrayList<>();
        if (excludesList != null) {
            patterns.addAll(separatePatterns(excludesList));
        } else {
            patterns.addAll(Arrays.asList(excludes));
        }
        return new PatternExcludesArtifactFilter(patterns);
    }

    private ArtifactFilter getScopeArtifactFilter() {
        if (scope == null) {
            return null;
        }
        return new ScopeArtifactFilter(scope);
    }

    /**
     * To handle multiple includes with version range like "group:artifact:jar:[1.0.0,2.2)", we have to use a parsing a
     * little bit more complex than split().
     *
     * @param includeString the string to parse
     * @return list of patterns
     */
    protected List<String> separatePatterns(String includeString) {
        if (includeString == null) {
            return Collections.emptyList();
        }

        List<String> patterns = new ArrayList<>();
        int indexOf = nextCommaIndex(includeString);
        while (indexOf >= 0) {
            patterns.add(includeString.substring(0, indexOf));
            includeString = includeString.substring(indexOf + 1);
            indexOf = nextCommaIndex(includeString);
        }
        patterns.add(includeString);

        return patterns;
    }

    private int nextCommaIndex(final String includeString) {

        int indexOfComma = includeString.indexOf(',');
        int nextRangeStartDelimiterIndex = findFirstChar(includeString, START_RANGE_CHARS);
        if (nextRangeStartDelimiterIndex >= 0) {
            if (!(indexOfComma >= 0 && indexOfComma < nextRangeStartDelimiterIndex)) {
                int nextStopDelimiterIndex = findFirstChar(includeString, END_RANGE_CHARS);

                // recursive call
                int tmp = nextCommaIndex(includeString.substring(nextStopDelimiterIndex + 1));
                indexOfComma = (tmp >= 0) ? nextStopDelimiterIndex + 1 + tmp : -1;
            }
        }
        return indexOfComma;
    }

    private int findFirstChar(final String includeString, final String chars) {
        int nextRangeStartDelimiterIndex = -1;

        char[] delimiters = chars.toCharArray();
        for (char delimiter : delimiters) {
            int index = includeString.indexOf(delimiter);
            if (index >= 0 && nextRangeStartDelimiterIndex >= 0) {
                nextRangeStartDelimiterIndex = Math.min(index, nextRangeStartDelimiterIndex);
            } else {
                if (index >= 0) {
                    nextRangeStartDelimiterIndex = index;
                }
            }
        }
        return nextRangeStartDelimiterIndex;
    }

    /**
     * Attempts to update the dependency {@code dep} to the given {@code newVersion}. The dependency can either
     * be the parent project or any given dependency.
     *
     * @param project {@link MavenProject} being updated
     * @param pom {@link MutableXMLStreamReader} instance to update the POM XML document
     * @param dep dependency to be updated (can also be a dependency made from the parent)
     * @param newVersion new version to update the dependency to
     * @param changeKind title for the {@link ChangeRecorder} log
     * @return {@code true} if an update has been made, {@code false} otherwise
     * @throws XMLStreamException thrown if updating the XML doesn't succeed
     * @throws MojoExecutionException thrown if the dependency cannot be converted to an artifact
     */
    protected boolean updateDependencyVersion(
            MavenProject project,
            MutableXMLStreamReader pom,
            Dependency dep,
            String newVersion,
            DependencyChangeRecord.ChangeKind changeKind)
            throws XMLStreamException, MojoExecutionException {
        boolean updated = false;
        if (getProcessParent()
                && project.getParent() != null
                && (DependencyComparator.INSTANCE.compare(
                                        dep,
                                        DependencyBuilder.newBuilder()
                                                .withGroupId(getProject()
                                                        .getParentArtifact()
                                                        .getGroupId())
                                                .withArtifactId(getProject()
                                                        .getParentArtifact()
                                                        .getArtifactId())
                                                .withVersion(getProject()
                                                        .getParentArtifact()
                                                        .getVersion())
                                                .build())
                                == 0
                        || getProject().getParentArtifact().getBaseVersion() != null
                                && DependencyComparator.INSTANCE.compare(
                                                dep,
                                                DependencyBuilder.newBuilder()
                                                        .withGroupId(getProject()
                                                                .getParentArtifact()
                                                                .getGroupId())
                                                        .withArtifactId(getProject()
                                                                .getParentArtifact()
                                                                .getArtifactId())
                                                        .withVersion(getProject()
                                                                .getParentArtifact()
                                                                .getBaseVersion())
                                                        .build())
                                        == 0)) {
            if (PomHelper.setProjectParentVersion(pom, newVersion)) {
                if (getLog().isDebugEnabled()) {
                    getLog().debug("Made parent update from " + dep.getVersion() + " to " + newVersion);
                }
                getChangeRecorder()
                        .recordChange(DefaultDependencyChangeRecord.builder()
                                .withKind(changeKind)
                                .withDependency(dep)
                                .withNewVersion(newVersion)
                                .build());
                updated = true;
            } else {
                getLog().warn("Could not update parent: " + dep.toString() + " to " + newVersion);
            }
        }

        if (PomHelper.setDependencyVersion(
                pom,
                dep.getGroupId(),
                dep.getArtifactId(),
                dep.getVersion(),
                newVersion,
                getProject().getModel(),
                getLog())) {
            if (getLog().isInfoEnabled()) {
                getLog().info("Updated " + toString(dep) + " to version " + newVersion + " in " + project.getGroupId()
                        + ":" + project.getArtifactId() + ":" + project.getVersion());
            }
            getChangeRecorder()
                    .recordChange(DefaultDependencyChangeRecord.builder()
                            .withKind(changeKind)
                            .withDependency(dep)
                            .withNewVersion(newVersion)
                            .build());
            updated = true;
        }

        return updated;
    }

    // TODO: add an updatePropertyVersion as well??? (like in CompareDependenciesMojo)
}
