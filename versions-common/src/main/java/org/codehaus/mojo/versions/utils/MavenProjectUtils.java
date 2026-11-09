package org.codehaus.mojo.versions.utils;
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.model.Profile;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;

import static java.util.Collections.emptyList;
import static java.util.Optional.ofNullable;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.ARTIFACT_ID;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.VERSION;

/**
 * Utility methods for extracting dependencies from a {@link org.apache.maven.project.MavenProject}
 */
public class MavenProjectUtils {

    private static final Pattern EXPRESSION = Pattern.compile("\\$\\{([^}]+)}");

    /**
     * Upper bound on nested property references, guarding against cycles such as {@code a=${b}}, {@code b=${a}}.
     */
    private static final int MAX_INTERPOLATION_DEPTH = 32;

    private MavenProjectUtils() {
        // prevent instantiation
    }

    /**
     * Retrieves dependencies from the plugins section
     *
     * @param project {@link MavenProject} instance
     * @return set of {@link Dependency} objects
     *         or an empty set if none have been retrieveddependencies or an empty set if none have been retrieved
     */
    public static Set<Dependency> extractPluginDependenciesFromPluginsInPluginManagement(MavenProject project) {
        return ofNullable(project.getBuild())
                .map(Build::getPluginManagement)
                .map(PluginManagement::getPlugins)
                .orElse(emptyList())
                .stream()
                .filter(plugin -> plugin.getDependencies() != null)
                .flatMap(plugin -> plugin.getDependencies().stream())
                .collect(() -> new TreeSet<>(DependencyComparator.INSTANCE), Set::add, Set::addAll);
    }

    /**
     * Retrieves dependencies from plugin management
     *
     * @param project {@link MavenProject} instance
     * @return set of {@link Dependency} objects
     *         or an empty set if none have been retrieveddependencies or an empty set if none have been retrieved
     */
    public static Set<Dependency> extractDependenciesFromPlugins(MavenProject project) {
        return project.getBuildPlugins().stream()
                .filter(plugin -> plugin.getDependencies() != null)
                .flatMap(plugin -> plugin.getDependencies().stream())
                .collect(() -> new TreeSet<>(DependencyComparator.INSTANCE), Set::add, Set::addAll);
    }

    /**
     * Retrieves dependencies from the dependency management of the project
     * as well as its immediate parent project.
     *
     * @param project                               {@link MavenProject} instance
     * @param processDependencyManagementTransitive if {@code true}, the original model will be considered
     *                                              instead of the interpolated model, which does not contain
     *                                              imported dependencies
     * @param log                                   {@link Log} instance (may not be null)
     * @return set of {@link Dependency} objects
     * @throws VersionRetrievalException thrown if version information retrieval fails
     *                                   or an empty set if none have been retrieveddependencies or an empty set if none
     *                                   have been retrieved
     */
    public static Set<Dependency> extractDependenciesFromDependencyManagement(
            MavenProject project, boolean processDependencyManagementTransitive, Log log)
            throws VersionRetrievalException {
        Stream<Dependency> dependencies = processDependencyManagementTransitive
                ? ofNullable(project.getDependencyManagement())
                        .map(DependencyManagement::getDependencies)
                        .map(Collection::stream)
                        .orElse(Stream.empty())
                : Stream.concat(
                        ofNullable(project.getOriginalModel().getDependencyManagement())
                                .map(DependencyManagement::getDependencies)
                                .map(Collection::stream)
                                .orElse(Stream.empty()),
                        ofNullable(project.getOriginalModel().getProfiles())
                                .flatMap(profiles -> profiles.stream()
                                        .map(Profile::getDependencyManagement)
                                        .filter(Objects::nonNull)
                                        .map(DependencyManagement::getDependencies)
                                        .map(Collection::stream)
                                        .reduce(Stream::concat))
                                .orElse(Stream.empty()));

        if (!processDependencyManagementTransitive) {
            // the raw model is not interpolated; the resolver of Maven 3.10+ rejects coordinates like
            // ${project.groupId}, so resolve them the way the effective model would
            dependencies = dependencies.map(dependency -> interpolateCoordinates(dependency, project));
        }

        // log and try to correct versions where they don't appear in the original pom.xml
        return dependencies
                .filter(dependency -> {
                    if (isInterpolated(dependency.getGroupId()) && isInterpolated(dependency.getArtifactId())) {
                        return true;
                    }
                    log.warn("Skipping dependency management entry " + dependency.getGroupId() + ":"
                            + dependency.getArtifactId() + " whose coordinates could not be interpolated");
                    return false;
                })
                .peek(dependency -> log.debug("dependency from pom: "
                        + dependency.getGroupId() + ":" + dependency.getArtifactId() + ":" + dependency.getVersion()
                        + ":" + dependency.getScope()))
                .map(
                        dependency -> { // resolve version from model properties if necessary (e.g.
                            // "${mycomponent.myversion}"
                            return dependency.getVersion() != null
                                    ? interpolateVersion(dependency.getVersion(), project)
                                            .map(v -> {
                                                Dependency result = dependency.clone();
                                                result.setVersion(v);
                                                return result;
                                            })
                                            .orElse(dependency)
                                    : getVersionFromParent(
                                                    dependency, project, processDependencyManagementTransitive, log)
                                            .orElse(dependency);
                        })
                .collect(() -> new TreeSet<>(DependencyComparator.INSTANCE), Set::add, Set::addAll);
    }

    /**
     * Tries to retrieve dependency version from parent
     * @param dependency                            {@link Dependency} for which we're trying to retrieve version
     * @param project                               {@link MavenProject} instance
     * @param processDependencyManagementTransitive if {@code true}, the original model will be considered
     *                                              instead of the interpolated model, which does not contain
     *                                              imported dependencies
     * @param log                                   {@link Log} instance (may not be null)
     * @return a {@link Optional} object containing a dependency from parent containing version filled in,
     * if that could be retrieved or {@link Optional#empty()}
     */
    private static Optional<Dependency> getVersionFromParent(
            Dependency dependency, MavenProject project, boolean processDependencyManagementTransitive, Log log) {
        if (project.hasParent()) {
            log.debug("Reading parent dependencyManagement information");
            return ofNullable(
                            processDependencyManagementTransitive
                                    ? project.getParent().getDependencyManagement()
                                    : project.getParent().getOriginalModel().getDependencyManagement())
                    .map(DependencyManagement::getDependencies)
                    .map(Collection::stream)
                    .flatMap(s -> s.filter(d -> dependency.getGroupId().equals(d.getGroupId()))
                            .filter(d -> dependency.getArtifactId().equals(d.getArtifactId()))
                            .filter(d -> dependency.getType().equals(d.getType()))
                            .findAny())
                    .map(d ->
                            processDependencyManagementTransitive ? d : interpolateCoordinates(d, project.getParent()));
        }

        String message = "We can't getModel the version for the dependency " + dependency.getGroupId() + ":"
                + dependency.getArtifactId() + " because there does not exist a parent.";
        log.warn(message);
        return Optional.empty();
    }

    /**
     * Attempts to interpolate the version from model properties.
     *
     * @param versionString string value of the version to interpolate
     * @param project    the maven project
     * @return an {@link Optional} object containing the interpolated string or {@link Optional#empty()} if no
     * interpolation could take place
     * @since 2.19.1
     */
    public static Optional<String> interpolateVersion(String versionString, final MavenProject project) {
        // resolve version from model properties if necessary (e.g. "${mycomponent.myversion}"
        while (versionString != null && versionString.startsWith("${")) {
            String propertyName = versionString.substring(2, versionString.length() - 1);
            versionString = ofNullable(project.getOriginalModel())
                    .map(Model::getProperties)
                    .map(p -> p.getProperty(propertyName))
                    .orElse(null);
        }
        return ofNullable(versionString);
    }

    /**
     * Interpolates the coordinates of a dependency read from a raw (uninterpolated) model, resolving
     * {@code ${project.*}} expressions and properties the way the effective model would.
     *
     * @param dependency dependency read from {@link MavenProject#getOriginalModel()}
     * @param project    the project whose raw model declares the dependency
     * @return the dependency itself if nothing needed interpolating, otherwise an interpolated copy
     */
    static Dependency interpolateCoordinates(Dependency dependency, MavenProject project) {
        Function<String, String> interpolate = value -> interpolate(value, project);
        String groupId = interpolate.apply(dependency.getGroupId());
        String artifactId = interpolate.apply(dependency.getArtifactId());
        String version = interpolate.apply(dependency.getVersion());
        String type = interpolate.apply(dependency.getType());
        String classifier = interpolate.apply(dependency.getClassifier());
        if (Objects.equals(groupId, dependency.getGroupId())
                && Objects.equals(artifactId, dependency.getArtifactId())
                && Objects.equals(version, dependency.getVersion())
                && Objects.equals(type, dependency.getType())
                && Objects.equals(classifier, dependency.getClassifier())) {
            return dependency;
        }
        Dependency result = dependency.clone();
        result.setGroupId(groupId);
        result.setArtifactId(artifactId);
        result.setVersion(version);
        result.setType(type);
        result.setClassifier(classifier);
        return result;
    }

    /**
     * Replaces every {@code ${...}} expression in {@code value} that can be resolved against the project;
     * unresolvable expressions are left in place.
     */
    private static String interpolate(String value, MavenProject project) {
        for (int depth = 0; value != null && value.contains("${") && depth < MAX_INTERPOLATION_DEPTH; depth++) {
            Matcher matcher = EXPRESSION.matcher(value);
            StringBuffer buffer = new StringBuffer();
            boolean changed = false;
            while (matcher.find()) {
                String replacement = resolveExpression(matcher.group(1), project);
                changed |= replacement != null;
                matcher.appendReplacement(
                        buffer, Matcher.quoteReplacement(replacement != null ? replacement : matcher.group()));
            }
            matcher.appendTail(buffer);
            if (!changed) {
                break;
            }
            value = buffer.toString();
        }
        return value;
    }

    private static String resolveExpression(String expression, MavenProject project) {
        String key = expression.startsWith("pom.") ? "project." + expression.substring(4) : expression;
        switch (key) {
            case "project.groupId":
                return project.getGroupId();
            case "project.artifactId":
                return project.getArtifactId();
            case "project.version":
                return project.getVersion();
            case "project.parent.groupId":
                return project.getModel().getParent() != null
                        ? project.getModel().getParent().getGroupId()
                        : null;
            case "project.parent.version":
                return project.getModel().getParent() != null
                        ? project.getModel().getParent().getVersion()
                        : null;
            case "project.basedir":
                return project.getBasedir() != null ? project.getBasedir().getPath() : null;
            default:
                break;
        }
        return Stream.of(
                        project.getProperties(),
                        ofNullable(project.getOriginalModel())
                                .map(Model::getProperties)
                                .orElse(null),
                        System.getProperties())
                .filter(Objects::nonNull)
                .map((Properties properties) -> properties.getProperty(expression))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }

    private static boolean isInterpolated(String value) {
        return value == null || !EXPRESSION.matcher(value).find();
    }

    /**
     * Retrieves dependencies from the dependencies of the project as well as its immediate parent project.
     * @param dependency {@link Dependency} instance
     * @return {@code true} if the version of the dependency is definned locally in the same project
     */
    public static boolean dependencyVersionLocalToReactor(Dependency dependency) {
        return dependency.getLocation(VERSION.toString()).getSource()
                == dependency.getLocation(ARTIFACT_ID.toString()).getSource();
    }
}
