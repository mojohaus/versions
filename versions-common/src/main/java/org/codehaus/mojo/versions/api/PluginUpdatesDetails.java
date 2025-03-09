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

import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.Objects;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;

/**
 * Details of a plugin's updates.
 */
public class PluginUpdatesDetails extends ArtifactVersions {
    private final Map<Dependency, ArtifactVersions> dependencyVersions;

    private final boolean includeSnapshots;

    public PluginUpdatesDetails(
            ArtifactVersions artifactVersions,
            Map<Dependency, ArtifactVersions> dependencyVersions,
            boolean includeSnapshots) {
        super(artifactVersions);
        Objects.requireNonNull(artifactVersions);
        Objects.requireNonNull(dependencyVersions);

        this.dependencyVersions = dependencyVersions;
        this.includeSnapshots = includeSnapshots;
    }

    public boolean isIncludeSnapshots() {
        return includeSnapshots;
    }

    public Map<Dependency, ArtifactVersions> getDependencyVersions() {
        return dependencyVersions;
    }

    public void addDependencyVersions(Map<Dependency, ArtifactVersions> dependencyVersions) {
        this.dependencyVersions.putAll(dependencyVersions);
    }

    /**
     * Returns true if a new version of the artifact fulfilling the criteria (whether to include snapshots) can be found
     *
     * @return true if a new version can be found
     */
    public boolean isArtifactUpdateAvailable() {
        ArtifactVersion[] updates = getAllUpdates(empty(), includeSnapshots);
        return updates != null && updates.length > 0;
    }

    /**
     * Returns true if a new version of the dependency can be found
     *
     * @return true if a new version can be found
     */
    public boolean isDependencyUpdateAvailable() {
        return dependencyVersions.values().stream().anyMatch(versions -> {
            ArtifactVersion[] dependencyUpdates = versions.getAllUpdates(empty(), includeSnapshots);
            return dependencyUpdates != null && dependencyUpdates.length > 0;
        });
    }

    /**
     * Returns true if a new version of the dependency can be found
     *
     * @return true if a new version can be found
     */
    public boolean isUpdateAvailable() {
        return isArtifactUpdateAvailable() || isDependencyUpdateAvailable();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof PluginUpdatesDetails)) {
            return false;
        }
        PluginUpdatesDetails other = (PluginUpdatesDetails) o;
        return includeSnapshots == other.includeSnapshots
                && Objects.equals(dependencyVersions, other.dependencyVersions)
                && super.equals(o);
    }

    public int hashCode() {
        return Objects.hash(getArtifact(), Arrays.hashCode(getVersions(true)), includeSnapshots, dependencyVersions);
    }

    // added an arbitrary comparison just to be able to differentiate objects having different includeSnapshots
    // and dependencyVersions while their super.compareTo() returns 0
    @SuppressWarnings("checkstyle:InnerAssignment")
    public int compareTo(PluginUpdatesDetails that) {
        int r;
        return (r = super.compareTo(that)) != 0
                ? r
                : Comparator.comparing(PluginUpdatesDetails::isIncludeSnapshots)
                        .thenComparing(p -> ofNullable(p.dependencyVersions)
                                .map(Map::values)
                                .map(c -> ofNullable(that.dependencyVersions)
                                        .map(Map::values)
                                        .map(c::containsAll)
                                        .orElse(true))
                                .orElse(false))
                        .compare(this, that);
    }
}
