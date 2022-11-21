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
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecord;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Common base class for {@link UseLatestVersionsMojo}
 * and {@link UseLatestReleasesMojo}
 */
public abstract class UseLatestVersionsMojoBase
    extends AbstractVersionsDependencyUpdaterMojo
{
    public UseLatestVersionsMojoBase( RepositorySystem repositorySystem,
                                  org.eclipse.aether.RepositorySystem aetherRepositorySystem,
                                  Map<String, Wagon> wagonMap,
                                  Map<String, ChangeRecorder> changeRecorders )
    {
        super( repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders );
    }

    /**
     * Updates the pom, given a set of dependencies, a function retrieving the newest version,
     * and an optional array of filters against which the input dependencies are matched.
     *
     * @param pom POM to be modified
     * @param dependencies collection of dependencies with the dependency versions before the change
     * @param newestVersionProducer function providing the newest version given a dependency and
     *                              an {@link ArtifactVersions} instance
     * @param changeKind title for the change recorder records
     * @param filters optional array of filters
     * @throws XMLStreamException thrown if the POM update doesn't succeed
     * @throws MojoExecutionException if something goes wrong.
     * @throws VersionRetrievalException thrown if an artifact versions cannot be retrieved
     */
    @SafeVarargs
    protected final void useLatestVersions( ModifiedPomXMLEventReader pom,
                                            Collection<Dependency> dependencies,
                                            BiFunction<Dependency, ArtifactVersions, Optional<ArtifactVersion>>
                                                    newestVersionProducer,
                                            ChangeRecord.ChangeKind changeKind,
                                            Predicate<Dependency>... filters )
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException
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
                updateDependencyVersion( pom, dep, newestVer.get().toString(), changeKind );
            }
        }
    }
}
