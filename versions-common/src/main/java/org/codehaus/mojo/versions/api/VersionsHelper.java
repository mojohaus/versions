package org.codehaus.mojo.versions.api;

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

import java.util.Map;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public interface VersionsHelper {

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     * <b>The resulting {@link ArtifactVersions} instance will contain all versions, including snapshots.</b>
     *
     * @param artifact The artifact to look for versions of.
     * @param usePluginRepositories {@code true} will consult the pluginRepositories, while {@code false} will
     *            consult the repositories for normal dependencies.
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-alpha-3
     */
    ArtifactVersions lookupArtifactVersions(Artifact artifact, boolean usePluginRepositories)
            throws VersionRetrievalException;

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     * <b>The resulting {@link ArtifactVersions} instance will contain all versions, including snapshots.</b>
     *
     * @param artifact The artifact to look for versions of.
     * @param versionRange versionRange to restrict the search, may be {@code null}
     * @param usePluginRepositories {@code true} will consult the pluginRepositories
     * @param useProjectRepositories {@code true} will consult regular project repositories
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 2.15.0
     */
    ArtifactVersions lookupArtifactVersions(
            Artifact artifact, VersionRange versionRange, boolean usePluginRepositories, boolean useProjectRepositories)
            throws VersionRetrievalException;

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     * <b>The resulting {@link ArtifactVersions} instance will contain all versions, including snapshots.</b>
     *
     * @param artifact The artifact to look for versions of.
     * @param versionRange versionRange to restrict the search, may be {@code null}
     * @param usePluginRepositories <code>true</code> will consult the pluginRepositories, while <code>false</code> will
     *            consult the repositories for normal dependencies.
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-alpha-3
     */
    ArtifactVersions lookupArtifactVersions(Artifact artifact, VersionRange versionRange, boolean usePluginRepositories)
            throws VersionRetrievalException;

    /**
     * Returns a map of all possible updates per dependency. The lookup is done in parallel using
     * {@code LOOKUP_PARALLEL_THREADS} threads.
     *
     * @param dependencyStream a stream of {@link Dependency} instances to look up.
     * @param usePluginRepositories Search the plugin repositories.
     * @param allowSnapshots whether snapshots should be included
     * @return map containing the ArtifactVersions object per dependency
     * @throws VersionRetrievalException thrown if a version cannot be retrieved
     */
    Map<Dependency, ArtifactVersions> lookupDependenciesUpdates(
            Stream<Dependency> dependencyStream, boolean usePluginRepositories, boolean allowSnapshots)
            throws VersionRetrievalException;

    /**
     * Returns a map of all possible updates per dependency. The lookup is done in parallel using
     * {@code LOOKUP_PARALLEL_THREADS} threads.
     *
     * @param dependencies stream of {@link Dependency} instances to look up.
     * @param usePluginRepositories Search the plugin repositories.
     * @param useProjectRepositories whether to use regular project repositories
     * @param allowSnapshots whether snapshots should be included
     * @return map containing the ArtifactVersions object per dependency
     * @throws VersionRetrievalException thrown if a version cannot be retrieved
     */
    Map<Dependency, ArtifactVersions> lookupDependenciesUpdates(
            Stream<Dependency> dependencies,
            boolean usePluginRepositories,
            boolean useProjectRepositories,
            boolean allowSnapshots)
            throws VersionRetrievalException;

    /**
     * Creates an {@link org.codehaus.mojo.versions.api.ArtifactVersions} instance from a dependency.
     *
     * @param dependency The dependency.
     * @param usePluginRepositories Search the plugin repositories.
     * @param useProjectRepositories whether to use regular project repositories
     * @param allowSnapshots whether snapshots should be included
     * @return The details of updates to the dependency.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    ArtifactVersions lookupDependencyUpdates(
            Dependency dependency,
            boolean usePluginRepositories,
            boolean useProjectRepositories,
            boolean allowSnapshots)
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a set of plugins.
     *
     * @param plugins A stream of {@link Plugin} instances to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return A map, keyed by plugin, with values of type {@link org.codehaus.mojo.versions.api.PluginUpdatesDetails}.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    Map<Plugin, PluginUpdatesDetails> lookupPluginsUpdates(Stream<Plugin> plugins, boolean allowSnapshots)
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a plugin.
     *
     * @param plugin The {@link Plugin} instance to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return The plugin update details.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    PluginUpdatesDetails lookupPluginUpdates(Plugin plugin, boolean allowSnapshots) throws VersionRetrievalException;

    /**
     * Returns a map of {@link org.codehaus.mojo.versions.api.PropertyVersions} values keyed by
     * {@link Property} instances consisting of the properties defined in the project which
     * are associated with version information.
     *
     * @param request {@link VersionPropertiesMapRequest} instance containing the arguments
     * @return a map of {@link org.codehaus.mojo.versions.api.PropertyVersions} values keyed by
     *         {@link Property} instances.
     * @throws MojoExecutionException if something goes wrong.
     */
    Map<Property, PropertyVersions> getVersionPropertiesMap(VersionPropertiesMapRequest request)
            throws MojoExecutionException;

    /**
     * Argument builder class for
     * {@link VersionsHelper#getVersionPropertiesMap(VersionPropertiesMapRequest)}.
     */
    class VersionPropertiesMapRequest {
        private MavenProject mavenProject;
        private Property[] propertyDefinitions;
        private String includeProperties;
        private String excludeProperties;
        private boolean includeParent;
        private boolean autoLinkItems;

        /**
         * Returns the {@link MavenProject} object
         * @return {@link MavenProject} object
         */
        protected MavenProject getMavenProject() {
            return mavenProject;
        }

        /**
         * Returns the {@link Property} array
         * @return {@link Property} array
         */
        protected Property[] getPropertyDefinitions() {
            return propertyDefinitions;
        }

        /**
         * Returns the value of {@link #includeProperties}
         * @return value of {@link #includeProperties}
         */
        protected String getIncludeProperties() {
            return includeProperties;
        }

        /**
         * Returns the value of {@link #excludeProperties}
         * @return value of {@link #excludeProperties}
         */
        protected String getExcludeProperties() {
            return excludeProperties;
        }

        /**
         * Returns the value of {@link #includeParent}.
         * If not set, it is assumed to be {@code true}
         *
         * @return value of {@link #includeParent}
         */
        protected boolean isIncludeParent() {
            return includeParent;
        }

        /**
         * Returns the value of {@link #autoLinkItems}
         * If not set, it is assumed to be {@code true}
         * @return value of {@link #autoLinkItems}
         */
        protected boolean isAutoLinkItems() {
            return autoLinkItems;
        }

        /**
         * Returns a new {@link Builder} instance
         * @return new {@link Builder} instance
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * Builder class for {@link VersionPropertiesMapRequest}
         */
        public static class Builder {
            private MavenProject mavenProject;
            private Property[] propertyDefinitions;
            private String includeProperties;
            private String excludeProperties;
            private Boolean includeParent;
            private Boolean autoLinkItems;

            private Builder() {}

            /**
             * Supplies the {@link MavenProject} instance
             * @param mavenProject {@link MavenProject} instance
             * @return {@link Builder} instance
             */
            public Builder withMavenProject(MavenProject mavenProject) {
                this.mavenProject = mavenProject;
                return this;
            }

            /**
             * Supplies the {@link MavenProject} instance
             * @param propertyDefinitions array of property definitions
             * @return {@link Builder} instance
             */
            public Builder withPropertyDefinitions(Property[] propertyDefinitions) {
                this.propertyDefinitions = propertyDefinitions;
                return this;
            }

            /**
             * Supplies the properties to include
             * @param includeProperties comma-delimited properties to include
             * @return {@link Builder} instance
             */
            public Builder withIncludeProperties(String includeProperties) {
                this.includeProperties = includeProperties;
                return this;
            }

            /**
             * Supplies the properties to exclude
             * @param excludeProperties comma-delimited properties to exclude
             * @return {@link Builder} instance
             */
            public Builder withExcludeProperties(String excludeProperties) {
                this.excludeProperties = excludeProperties;
                return this;
            }

            /**
             * Supplies the includeParent parameter (whether parent POMs should be included)
             * @param includeParent whether parent POMs should be included
             * @return {@link Builder} instance
             */
            public Builder withIncludeParent(boolean includeParent) {
                this.includeParent = includeParent;
                return this;
            }

            /**
             * Supplies the information whether to automatically infer associations
             * @param autoLinkItems whether to automatically infer associations
             * @return {@link Builder} instance
             */
            public Builder withAutoLinkItems(boolean autoLinkItems) {
                this.autoLinkItems = autoLinkItems;
                return this;
            }

            /**
             * Returns the {@link VersionPropertiesMapRequest} instance
             * @return {@link VersionPropertiesMapRequest} instance
             */
            public VersionPropertiesMapRequest build() {
                VersionPropertiesMapRequest instance = new VersionPropertiesMapRequest();
                instance.mavenProject = this.mavenProject;
                instance.propertyDefinitions = propertyDefinitions;
                instance.includeProperties = includeProperties;
                instance.excludeProperties = excludeProperties;
                instance.includeParent = includeParent == null || includeParent;
                instance.autoLinkItems = autoLinkItems == null || autoLinkItems;
                return instance;
            }
        }
    }

    /**
     * Attempts to resolve the artifact.
     *
     * @param artifact The artifact to resolve.
     * @param usePluginRepositories whether to resolve from the plugin repositories or the regular repositories.
     * @throws ArtifactResolutionException if resolution is unsuccessful
     * @since 1.3
     */
    void resolveArtifact(Artifact artifact, boolean usePluginRepositories) throws ArtifactResolutionException;
}
