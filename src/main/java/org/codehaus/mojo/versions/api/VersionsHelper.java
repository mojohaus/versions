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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.ArtifactUpdatesDetails;
import org.codehaus.mojo.versions.PluginUpdatesDetails;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
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
     * Looks up the versions of the specified artifact that are available in either the local repository, or the
     * appropriate remote repositories.
     *
     * @param artifact              The artifact to look for versions of.
     * @param usePluginRepositories <code>true</code> will consult the pluginRepositories, while <code>false</code>
     *                              will consult the repositories for normal dependencies.
     * @return The details of the available artifact versions.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          When things go wrong.
     * @since 1.0-alpha-3
     */
    ArtifactVersions lookupArtifactVersions( Artifact artifact, boolean usePluginRepositories )
        throws MojoExecutionException;

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
     * @param groupId    the groupId.
     * @param artifactId the artifactId.
     * @return the version comparator to use.
     * @since 1.0-alpha-3
     */
    VersionComparator getVersionComparator( String groupId, String artifactId );

    /**
     * Returns the artifact factory to use.
     *
     * @return the artifact factory to use.
     * @since 1.0-alpha-3
     */
    ArtifactFactory getArtifactFactory();

    /**
     * Shorthand method for <code>getArtifactFactory().createPluginArtifact(...)</code>.
     *
     * @param groupId    The group Id.
     * @param artifactId The artifact Id.
     * @param version    The version range.
     * @return the corresponding plugin artifact.
     * @since 1.0-alpha-3
     */
    Artifact createPluginArtifact( String groupId, String artifactId, VersionRange version );

    /**
     * Shorthand method for <code>getArtifactFactory().createDependencyArtifact(...)</code>.
     *
     * @param groupId    The group id.
     * @param artifactId The artifact id.
     * @param version    The version (possibly a range)
     * @param type       The type.
     * @param classifier The classifier.
     * @param scope      The scope.
     * @param optional   If optional or not.
     * @return The corresponding dependency artifact.
     * @since 1.0-alpha-3
     */
    Artifact createDependencyArtifact( String groupId, String artifactId, VersionRange version, String type,
                                       String classifier, String scope, boolean optional );

    /**
     * Shorthand method for <code>getArtifactFactory().createDependencyArtifact(...)</code>.
     *
     * @param groupId    The group id.
     * @param artifactId The artifact id.
     * @param versionRange    The version range.
     * @param type       The type.
     * @param classifier The classifier.
     * @param scope      The scope.
     * @return The corresponding dependency artifact.
     * @since 1.0-beta-1
     */
    Artifact createDependencyArtifact( String groupId, String artifactId, VersionRange versionRange, String type,
                                   String classifier, String scope );
    /**
     * Shorthand method for <code>getArtifactFactory().createDependencyArtifact(...)</code> which extracts the
     * parameters from the Dependency instance.
     *
     * @param dependency The dependency to create the artifact for.
     * @return The corresponding dependency artifact.
     * @throws InvalidVersionSpecificationException
     *          if the version specified in the dependency is invalid.
     * @since 1.0-alpha-3
     */
    Artifact createDependencyArtifact( Dependency dependency )
        throws InvalidVersionSpecificationException;

    /**
     * Takes a {@link List} of {@link org.apache.maven.project.MavenProject} instances and converts it into a 
     * {@link Set} of {@link Artifact} instances.
     *
     * @param mavenProjects the {@link List} of {@link org.apache.maven.project.MavenProject} instances.
     * @return a {@link Set} of {@link Artifact} instances.
     * @since 1.0-alpha-3
     */
    Set/*<Artifact>*/ extractArtifacts( Collection/*<MavenProject>*/ mavenProjects );

    /**
     * Creates an {@link ArtifactVersion} instance from a string.
     *
     * @param version the string representation of the version.
     * @return The artifact version.
     * @since 1.0-beta-1
     */
    ArtifactVersion createArtifactVersion( String version );

    /**
     * Looks up the updates of an artifact.
     * @param artifact The artifact to look up
     * @param current The current version.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @param usePluginRepositories Search the plugin repositories.
     * @return The artifact update details.
     * @throws MojoExecutionException If things go wrong.
     */
    ArtifactUpdatesDetails lookupArtifactUpdates( Artifact artifact, ArtifactVersion current, Boolean allowSnapshots,
                                                  boolean usePluginRepositories )
        throws MojoExecutionException;
    
    /**
     * Looks up the updates for a set of dependencies.
     *
     * @param dependencies The set of {@link Dependency} instances to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @param usePluginRepositories Search the plugin repositories.
     * @return A map, keyed by dependency, with values of type {@link org.codehaus.mojo.versions.ArtifactUpdatesDetails}.
     * @throws MojoExecutionException If things go wrong.
     * @since 1.0-beta-1
     */
    Map/*<Dependency,ArtifactUpdatesDetails>*/ lookupDependenciesUpdates( Set dependencies, Boolean allowSnapshots,
                                                                          boolean usePluginRepositories )
        throws MojoExecutionException;
    
    /**
     * Creates an {@link org.codehaus.mojo.versions.ArtifactUpdatesDetails} instance from a dependency.
     *
     * @param dependency The dependency.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @param usePluginRepositories Search the plugin repositories.
     * @return The details of updates to the dependency.
     * @throws MojoExecutionException If things go wrong.
     * @since 1.0-beta-1
     */
    ArtifactUpdatesDetails lookupDependencyUpdates( Dependency dependency, Boolean allowSnapshots,
                                                    boolean usePluginRepositories )
        throws MojoExecutionException;
        
    /**
     * Looks up the updates for a set of plugins.
     *
     * @param plugins The set of {@link Plugin} instances to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return A map, keyed by plugin, with values of type {@link org.codehaus.mojo.versions.PluginUpdatesDetails}.
     * @throws MojoExecutionException If things go wrong.
     * @since 1.0-beta-1
     */
    Map/*<Plugin,PluginUpdateDetails>*/ lookupPluginsUpdates( Set plugins, Boolean allowSnapshots )
        throws MojoExecutionException;
    
    /**
     * Looks up the updates for a plugin.
     *
     * @param plugin The {@link Plugin} instance to look up.
     * @param allowSnapshots Include snapshots in the list of updates.
     * @return The plugin update details.
     * @throws MojoExecutionException If things go wrong.
     * @since 1.0-beta-1
     */
    PluginUpdatesDetails lookupPluginUpdates( Plugin plugin, Boolean allowSnapshots)
        throws MojoExecutionException;

    /**
     * Returns an {@link ExpressionEvaluator} for the specified project.
     * @param project The project.
     * @return an {@link ExpressionEvaluator} for the specified project.
     * @since 1.0-beta-1
     */
    ExpressionEvaluator getExpressionEvaluator( MavenProject project);

    Map/*<Property,PropertyVersions>*/ getVersionProperties( MavenProject project, Property[] propertyDefinitions,
                                                             String includeProperties, String excludeProperties, Boolean autoLinkItems )
        throws MojoExecutionException;
    
    
}
