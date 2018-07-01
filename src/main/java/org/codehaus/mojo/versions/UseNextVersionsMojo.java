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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-next-versions", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseNextVersionsMojo
    extends ParentUpdatingDependencyUpdateMojo
{

    // ------------------------------ METHODS --------------------------


    @Override
    protected void setVersions(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws ArtifactMetadataRetrievalException, XMLStreamException, MojoExecutionException
    {
        useNextVersions(pom, dependencies);
    }

    private void useNextVersions( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        Iterator<Dependency> i = dependencies.iterator();

        while ( i.hasNext() )
        {
            Dependency dep = i.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            String version = dep.getVersion();
            Artifact artifact = this.toArtifact( dep );
            if ( !isIncluded( artifact ) )
            {
                continue;
            }

            getLog().debug( "Looking for newer versions of " + toString( dep ) );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            ArtifactVersion[] newer = versions.getNewerVersions( version, Boolean.TRUE.equals( allowSnapshots ) );
            if ( newer.length > 0 )
            {
                setVersion(pom, dep, version, artifact, newer[0]);
            }
        }
    }

}