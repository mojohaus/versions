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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.Collection;

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
    Comparator getVersionComparator( Artifact artifact );

    /**
     * Returns the version comparator to use for the specified groupId and artifactId.
     *
     * @param groupId    the groupId.
     * @param artifactId the artifactId.
     * @return the version comparator to use.
     * @since 1.0-alpha-3
     */
    Comparator getVersionComparator( String groupId, String artifactId );

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
     * Takes a {@link List} of {@link org.apache.maven.project.MavenProject} instances and converts it into a {@link Set} of {@link Artifact} instances.
     *
     * @param mavenProjects the {@link List} of {@link org.apache.maven.project.MavenProject} instances.
     * @return a {@link Set} of {@link Artifact} instances.
     * @since 1.0-alpha-3
     */
    Set/*<Artifact>*/ extractArtifacts( Collection/*<MavenProject>*/ mavenProjects );

}
