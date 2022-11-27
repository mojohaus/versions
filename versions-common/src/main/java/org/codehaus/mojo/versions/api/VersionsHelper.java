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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public interface VersionsHelper
{
    /**
     * Gets the logger used by this helper.
     *
     * @return the logger used by this helper.
     */
    Log getLog();

    /**
     * Returns the version comparator to use for the specified artifact.
     *
     * @param artifact the artifact.
     * @return the version comparator to use.
     * @since 1.0-alpha-3
     */
    VersionComparator getVersionComparator( Artifact artifact );

    /**
     * Returns the version comparator to use for the specified groupId and artifactId.
     *
     * @param groupId the groupId.
     * @param artifactId the artifactId.
     * @return the version comparator to use.
     * @since 1.0-alpha-3
     */
    VersionComparator getVersionComparator( String groupId, String artifactId );

    /**
     * Shorthand method for <code>repositorySystem.createPluginArtifact(...)</code>.
     *
     * @param groupId The group Id.
     * @param artifactId The artifact Id.
     * @param version The version range.
     * @return the corresponding plugin artifact.
     * @since 1.0-alpha-3
     */
    Artifact createPluginArtifact( String groupId, String artifactId, String version );

    /**
     * Shorthand method for <code>repositorySystem.createDependencyArtifact(...)</code>.
     *
     * @param groupId The group id.
     * @param artifactId The artifact id.
     * @param version The version (possibly a range)
     * @param type The type.
     * @param classifier The classifier.
     * @param scope The scope.
     * @param optional If optional or not.
     * @return The corresponding dependency artifact.
     * @since 1.0-alpha-3
     */
    Artifact createDependencyArtifact( String groupId, String artifactId, String version, String type,
                                       String classifier, String scope, boolean optional );

    /**
     * Shorthand method for <code>getArtifactFactory().createDependencyArtifact(...)</code>.
     *
     * @param groupId The group id.
     * @param artifactId The artifact id.
     * @param version The version.
     * @param type The type.
     * @param classifier The classifier.
     * @param scope The scope.
     * @return The corresponding dependency artifact.
     * @since 1.0-beta-1
     */
    Artifact createDependencyArtifact( String groupId, String artifactId, String version, String type,
                                       String classifier, String scope );

    /**
     * Shorthand method for <code>repositorySystem.createDependencyArtifact(...)</code> which extracts the
     * parameters from the Dependency instance.
     *
     * @param dependency The dependency to create the artifact for.
     * @return The corresponding dependency artifact.
     * @since 1.0-alpha-3
     */
    Artifact createDependencyArtifact( Dependency dependency );

    /**
     * Takes a {@link List} of {@link org.apache.maven.project.MavenProject} instances and converts it into a
     * {@link Set} of {@link Artifact} instances.
     *
     * @param mavenProjects the {@link List} of {@link org.apache.maven.project.MavenProject} instances.
     * @return a {@link Set} of {@link Artifact} instances.
     * @since 1.0-alpha-3
     */
    Set<Artifact> extractArtifacts( Collection<MavenProject> mavenProjects );

    /**
     * Creates an {@link ArtifactVersion} instance from a string.
     *
     * @param version the string representation of the version.
     * @return The artifact version.
     * @since 1.0-beta-1
     */
    ArtifactVersion createArtifactVersion( String version );

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     *
     * @param artifact The artifact to look for versions of.
     * @param usePluginRepositories <code>true</code> will consult the pluginRepositories, while <code>false</code> will
     *            consult the repositories for normal dependencies.
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-alpha-3
     */
    ArtifactVersions lookupArtifactVersions( Artifact artifact, boolean usePluginRepositories )
            throws VersionRetrievalException;

    /**
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     *
     * @param artifact The artifact to look for versions of.
     * @param versionRange versionRange to restrict the search
     * @param usePluginRepositories <code>true</code> will consult the pluginRepositories, while <code>false</code> will
     *            consult the repositories for normal dependencies.
     * @return The details of the available artifact versions.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-alpha-3
     */
    ArtifactVersions lookupArtifactVersions( Artifact artifact, VersionRange versionRange,
                                             boolean usePluginRepositories )
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a set of dependencies.
     *
     * @param dependencies The set of {@link Dependency} instances to look up.
     * @param usePluginRepositories Search the plugin repositories.
     * @return A map, keyed by dependency, with values of type {@link org.codehaus.mojo.versions.api.ArtifactVersions}.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    Map<Dependency, ArtifactVersions> lookupDependenciesUpdates( Set<Dependency> dependencies,
                                                                 boolean usePluginRepositories )
            throws VersionRetrievalException;

    /**
     * Creates an {@link org.codehaus.mojo.versions.api.ArtifactVersions} instance from a dependency.
     *
     * @param dependency The dependency.
     * @param usePluginRepositories Search the plugin repositories.
     * @return The details of updates to the dependency.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    ArtifactVersions lookupDependencyUpdates( Dependency dependency, boolean usePluginRepositories )
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a set of plugins.
     *
     * @param plugins The set of {@link Plugin} instances to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return A map, keyed by plugin, with values of type {@link org.codehaus.mojo.versions.api.PluginUpdatesDetails}.
     * @throws VersionRetrievalException thrown if version resolution fails
     * @since 1.0-beta-1
     */
    Map<Plugin, PluginUpdatesDetails> lookupPluginsUpdates( Set<Plugin> plugins, boolean allowSnapshots )
            throws VersionRetrievalException;

    /**
     * Looks up the updates for a plugin.
     *
     * @param plugin The {@link Plugin} instance to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return The plugin update details.
     * @since 1.0-beta-1
     */
    PluginUpdatesDetails lookupPluginUpdates( Plugin plugin, boolean allowSnapshots ) throws VersionRetrievalException;

    /**
     * Returns an {@link ExpressionEvaluator} for the specified project.
     *
     * @param project The project.
     * @return an {@link ExpressionEvaluator} for the specified project.
     * @since 1.0-beta-1
     */
    ExpressionEvaluator getExpressionEvaluator( MavenProject project );

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
    Map<Property, PropertyVersions> getVersionPropertiesMap( VersionPropertiesMapRequest request )
        throws MojoExecutionException;

    /**
     * Argument builder class for
     * {@link VersionsHelper#getVersionPropertiesMap(VersionPropertiesMapRequest)}.
     */
    class VersionPropertiesMapRequest
    {
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
        protected MavenProject getMavenProject()
        {
            return mavenProject;
        }

        /**
         * Returns the {@link Property} array
         * @return {@link Property} array
         */
        protected Property[] getPropertyDefinitions()
        {
            return propertyDefinitions;
        }

        /**
         * Returns the value of {@link #includeProperties}
         * @return value of {@link #includeProperties}
         */
        protected String getIncludeProperties()
        {
            return includeProperties;
        }

        /**
         * Returns the value of {@link #excludeProperties}
         * @return value of {@link #excludeProperties}
         */
        protected String getExcludeProperties()
        {
            return excludeProperties;
        }

        /**
         * Returns the value of {@link #includeParent}.
         * If not set, it is assumed to be {@code true}
         *
         * @return value of {@link #includeParent}
         */
        protected boolean isIncludeParent()
        {
            return includeParent;
        }

        /**
         * Returns the value of {@link #autoLinkItems}
         * If not set, it is assumed to be {@code true}
         * @return value of {@link #autoLinkItems}
         */
        protected boolean isAutoLinkItems()
        {
            return autoLinkItems;
        }

        /**
         * Returns a new {@link Builder} instance
         * @return new {@link Builder} instance
         */
        public static Builder builder()
        {
            return new Builder();
        }

        /**
         * Builder class for {@link VersionPropertiesMapRequest}
         */
        public static class Builder
        {
            private MavenProject mavenProject;
            private Property[] propertyDefinitions;
            private String includeProperties;
            private String excludeProperties;
            private Boolean includeParent;
            private Boolean autoLinkItems;

            private Builder()
            {
            }

            /**
             * Supplies the {@link MavenProject} instance
             * @param mavenProject {@link MavenProject} instance
             * @return {@link Builder} instance
             */
            public Builder withMavenProject( MavenProject mavenProject )
            {
                this.mavenProject = mavenProject;
                return this;
            }

            /**
             * Supplies the {@link MavenProject} instance
             * @param propertyDefinitions array of property definitions
             * @return {@link Builder} instance
             */
            public Builder withPropertyDefinitions( Property[] propertyDefinitions )
            {
                this.propertyDefinitions = propertyDefinitions;
                return this;
            }

            /**
             * Supplies the properties to include
             * @param includeProperties comma-delimited properties to include
             * @return {@link Builder} instance
             */
            public Builder withIncludeProperties( String includeProperties )
            {
                this.includeProperties = includeProperties;
                return this;
            }

            /**
             * Supplies the properties to exclude
             * @param excludeProperties comma-delimited properties to exclude
             * @return {@link Builder} instance
             */
            public Builder withExcludeProperties( String excludeProperties )
            {
                this.excludeProperties = excludeProperties;
                return this;
            }

            /**
             * Supplies the includeParent parameter (whether parent POMs should be included)
             * @param includeParent whether parent POMs should be included
             * @return {@link Builder} instance
             */
            public Builder withIncludeParent( boolean includeParent )
            {
                this.includeParent = includeParent;
                return this;
            }

            /**
             * Supplies the information whether to automatically infer associations
             * @param autoLinkItems whether to automatically infer associations
             * @return {@link Builder} instance
             */
            public Builder withAutoLinkItems( boolean autoLinkItems )
            {
                this.autoLinkItems = autoLinkItems;
                return this;
            }

            /**
             * Returns the {@link VersionPropertiesMapRequest} instance
             * @return {@link VersionPropertiesMapRequest} instance
             */
            public VersionPropertiesMapRequest build()
            {
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
    void resolveArtifact( Artifact artifact, boolean usePluginRepositories )
        throws ArtifactResolutionException;
}
