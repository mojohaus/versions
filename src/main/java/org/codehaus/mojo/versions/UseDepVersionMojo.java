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

import java.util.Collection;
import java.util.Iterator;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * @author Dan Arcari
 * @since 2.3
 */
@Mojo( name = "use-dep-version", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseDepVersionMojo
    extends ParentUpdatingDependencyUpdateMojo
{

    /**
     * The exact version to be applied for the included dependencies
     */
    @Parameter( property = "depVersion", required = true )
    protected String depVersion;

    /**
     * If set to true, will use whatever version is supplied without attempting to validate that such a version is
     * obtainable from the repository chain.
     */
    @Parameter( property = "forceVersion", defaultValue = "false" )
    protected boolean forceVersion;

    @Override
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {

        if ( depVersion == null || depVersion.equals( "" ) )
        {
            throw new IllegalArgumentException( "depVersion must be supplied with use-specific-version, and cannot be blank." );
        }

        if ( !forceVersion && !hasIncludes() )
        {
            throw new IllegalArgumentException( "The use-specific-version goal is intended to be used with a single artifact. Please specify a value for the 'includes' parameter, or use -DforceVersion=true to override this check." );
        }

        super.update(pom);
    }

    @Override
    protected void setVersions(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        useDepVersion(pom, dependencies);
    }

    private void useDepVersion( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws MojoExecutionException, XMLStreamException, ArtifactMetadataRetrievalException
    {
        Iterator<Dependency> itr = dependencies.iterator();

        while ( itr.hasNext() )
        {
            Dependency dep = itr.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            Artifact artifact = this.toArtifact( dep );

            if ( isIncluded( artifact ) )
            {
                if ( !forceVersion )
                {
                    ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );

                    if ( !versions.containsVersion( depVersion ) )
                    {
                        throw new MojoExecutionException( String.format( "Version %s is not available for artifact %s:%s",
                                                                         depVersion, artifact.getGroupId(),
                                                                         artifact.getArtifactId() ) );
                    }
                }

                String version = dep.getVersion();
                setVersion(pom, dep, version, artifact, new DefaultArtifactVersion(version));
            }
        }
    }
}