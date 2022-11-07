package org.codehaus.mojo.versions;

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

import javax.xml.stream.XMLStreamException;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Common base class for {@link UseLatestVersionsMojo}
 * and {@link UseLatestReleasesMojo}
 */
public abstract class UseLatestVersionsMojoBase
    extends AbstractVersionsDependencyUpdaterMojo
{
    public UseLatestVersionsMojoBase( RepositorySystem repositorySystem,
                                  MavenProjectBuilder projectBuilder,
                                  ArtifactMetadataSource artifactMetadataSource,
                                  WagonManager wagonManager,
                                  ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * Updates the pom, given a set of dependencies, a function retrieving the newest version,
     * and an optional array of filters against which the input dependencies are matched.
     *
     * @param pom POM to be modified
     * @param dependencies collection of dependencies with the dependency versions before the change
     * @param newestVersionProducer function providing the newest version given a dependency and
     *                              an {@link ArtifactVersions} instance
     * @param changeRecorderTitle title for the change recorder records
     * @param filters optional array of filters
     * @throws XMLStreamException thrown if the POM update doesn't succeed
     * @throws ArtifactMetadataRetrievalException thrown if an artifact cannot be retried
     */
    @SafeVarargs
    protected final void useLatestVersions( ModifiedPomXMLEventReader pom,
                                            Collection<Dependency> dependencies,
                                            BiFunction<Dependency, ArtifactVersions, Optional<ArtifactVersion>>
                                                    newestVersionProducer,
                                            String changeRecorderTitle,
                                            Predicate<Dependency>... filters )
            throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        for ( Dependency dep : dependencies )
        {
            if ( !Arrays.stream( filters )
                    .map( f -> f.test( dep ) )
                    .reduce( Boolean::logicalAnd )
                    .orElse( true ) )
            {
                continue;
            }

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            if ( isHandledByProperty( dep ) )
            {
                getLog().debug( "Ignoring dependency with property as version: " + toString( dep ) );
                continue;
            }

            Artifact artifact = toArtifact( dep );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            ArtifactVersion selectedVersion = new DefaultArtifactVersion( dep.getVersion() );
            getLog().debug( "Selected version:" + selectedVersion );
            getLog().debug( "Looking for newer versions of " + toString( dep ) );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            Optional<ArtifactVersion> newestVer = newestVersionProducer.apply( dep, versions );
            if ( newestVer.isPresent() )
            {
                updateDependencyVersion( pom, dep, newestVer.get().toString(), changeRecorderTitle );
            }
        }
    }
}
