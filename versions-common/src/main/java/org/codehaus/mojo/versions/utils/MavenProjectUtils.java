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

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;

import static java.util.Collections.emptyList;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.startsWith;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.ARTIFACT_ID;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.VERSION;

/**
 * Utility methods for extracting dependencies from a {@link org.apache.maven.project.MavenProject}
 */
public class MavenProjectUtils {
    /**
     * Retrieves dependencies from the plugins section
     *
     * @param project {@link MavenProject} instance
     * @return set of {@link Dependency} objects
     * or an empty set if none have been retrieveddependencies or an empty set if none have been retrieved
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
        Set<Dependency> dependencyManagement = new TreeSet<>(DependencyComparator.INSTANCE);
        DependencyManagement projectDependencyManagement = processDependencyManagementTransitive
                ? project.getDependencyManagement()
                : project.getOriginalModel().getDependencyManagement();
        if (projectDependencyManagement != null) {

            List<Dependency> dependenciesFromPom = projectDependencyManagement.getDependencies();
            for (Dependency dependency : dependenciesFromPom) {
                log.debug("dependency from pom: " + dependency.getGroupId() + ":" + dependency.getArtifactId() + ":"
                        + dependency.getVersion() + ":" + dependency.getScope());
                if (dependency.getVersion() == null) {
                    // getModel parent and getModel the information from there.
                    if (project.hasParent()) {
                        log.debug("Reading parent dependencyManagement information");
                        DependencyManagement parentProjectDependencyManagement = processDependencyManagementTransitive
                                ? project.getParent().getDependencyManagement()
                                : project.getParent().getOriginalModel().getDependencyManagement();
                        if (parentProjectDependencyManagement != null) {
                            List<Dependency> parentDeps = parentProjectDependencyManagement.getDependencies();
                            for (Dependency parentDep : parentDeps) {
                                // only groupId && artifactId needed cause version is null
                                if (dependency.getGroupId().equals(parentDep.getGroupId())
                                        && dependency.getArtifactId().equals(parentDep.getArtifactId())
                                        && dependency.getType().equals(parentDep.getType())) {
                                    dependencyManagement.add(parentDep);
                                }
                            }
                        }
                    } else {
                        String message = "We can't getModel the version for the dependency " + dependency.getGroupId()
                                + ":" + dependency.getArtifactId() + " because there does not exist a parent.";
                        log.error(message);
                        // Throw error because we will not able to getModel a version for a dependency.
                        throw new VersionRetrievalException(message);
                    }
                } else {
                    dependencyManagement.remove(dependency);
                    dependencyManagement.add(interpolateVersion(dependency, project));
                }
            }
        }
        return dependencyManagement;
    }

    /**
     * Attempts to interpolate the version from model properties.
     *
     * @param dependency the dependency
     * @param project    the maven project
     * @return the dependency with interpolated property (as far as possible)
     * @since 2.14.0
     */
    public static Dependency interpolateVersion(final Dependency dependency, final MavenProject project) {
        // resolve version from model properties if necessary (e.g. "${mycomponent.myversion}"
        if (startsWith(dependency.getVersion(), "${")) {
            return ofNullable(project.getOriginalModel()
                            .getProperties()
                            .getProperty(dependency
                                    .getVersion()
                                    .substring(2, dependency.getVersion().length() - 1)))
                    .map(v -> {
                        Dependency result = dependency.clone();
                        result.setVersion(v);
                        return result;
                    })
                    .orElse(dependency);
        }
        return dependency;
    }

    /**
     * @return {@code true} if the version of the dependency is definned locally in the same project
     */
    public static boolean dependencyVersionLocalToReactor(Dependency dependency) {
        return dependency.getLocation(VERSION.toString()).getSource()
                == dependency.getLocation(ARTIFACT_ID.toString()).getSource();
    }
}
