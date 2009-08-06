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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;

import java.util.Set;
import java.util.TreeSet;

/**
 * Builds {@link org.codehaus.mojo.versions.api.PropertyVersions} instances.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
class PropertyVersionsBuilder
{
    private final String name;

    private final String profileId;

    private final Set/*<ArtifactAssocation*/ associations;

    private final VersionsHelper helper;

    /**
     * Constructs a new {@link org.codehaus.mojo.versions.api.PropertyVersions}.
     *
     * @param profileId The profileId.
     * @param name      The property name.
     * @param helper    The {@link org.codehaus.mojo.versions.api.DefaultVersionsHelper}.
     */
    public PropertyVersionsBuilder( String profileId, String name, VersionsHelper helper )
    {
        this.profileId = profileId;
        this.name = name;
        this.associations = new TreeSet();
        this.helper = helper;
    }

    public void addAssociation( Artifact artifact, boolean usePluginRepositories )
    {
        associations.add( new DefaultArtifactAssociation( artifact, usePluginRepositories ) );
    }

    public void removeAssociation( Artifact artifact, boolean usePluginRepositories )
    {
        associations.remove( new DefaultArtifactAssociation( artifact, usePluginRepositories ) );
    }

    public void clearAssociations()
    {
        associations.clear();
    }

    public boolean isAssociated()
    {
        return !associations.isEmpty();
    }

    public ArtifactAssocation[] getAssociations()
    {
        return (ArtifactAssocation[]) associations.toArray( new ArtifactAssocation[associations.size()] );
    }

    public PropertyVersions newPropertyVersions()
        throws ArtifactMetadataRetrievalException
    {
        return new PropertyVersions( profileId, name, helper, associations );
    }

    public String getName()
    {
        return name;
    }
}
