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

import java.util.Map;
import java.util.Objects;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.ArtifactVersions;

import static java.util.Optional.empty;

/**
 * Details of a plugin's updates.
 */
public class PluginUpdatesDetails extends ArtifactVersions
{
    private final Map<Dependency, ArtifactVersions> dependencyVersions;

    private final boolean includeSnapshots;

    public PluginUpdatesDetails( ArtifactVersions artifactVersions,
                                 Map<Dependency, ArtifactVersions> dependencyVersions, boolean includeSnapshots )
    {
        super( artifactVersions );
        Objects.requireNonNull( artifactVersions );
        Objects.requireNonNull( dependencyVersions );

        this.dependencyVersions = dependencyVersions;
        this.includeSnapshots = includeSnapshots;
    }

    public Map<Dependency, ArtifactVersions> getDependencyVersions()
    {
        return dependencyVersions;
    }

    /**
     * Returns true if a new version of the artifact fulfilling the criteria (whether to include snapshots) can be found
     *
     * @return true if a new version can be found
     */
    public boolean isArtifactUpdateAvailable()
    {
        ArtifactVersion[] updates = getAllUpdates( empty(), includeSnapshots );
        return updates != null && updates.length > 0;
    }

    /**
     * Returns true if a new version of the dependency can be found
     *
     * @return true if a new version can be found
     */
    public boolean isDependencyUpdateAvailable()
    {
        return dependencyVersions.values().stream().anyMatch( versions ->
        {
            ArtifactVersion[] dependencyUpdates = versions.getAllUpdates( empty(), includeSnapshots );
            return dependencyUpdates != null && dependencyUpdates.length > 0;
        } );
    }

    /**
     * Returns true if a new version of the dependency can be found
     *
     * @return true if a new version can be found
     */
    public boolean isUpdateAvailable()
    {
        return isArtifactUpdateAvailable() || isDependencyUpdateAvailable();
    }
}
