package org.codehaus.mojo.versions.reporting.model;

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
import java.util.TreeMap;
import java.util.function.Function;

import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.utils.DependencyComparator;

/**
 * Base class for using with the {@linkplain org.codehaus.mojo.versions.api.ReportRenderer} API
 * @param <V> class extending ArtifactVersion in the constructor
 */
public abstract class AbstractUpdatesModel<V extends ArtifactVersions> {
    private final Map<Dependency, V> artifactUpdates;
    private final Map<Dependency, V> artifactManagementUpdates;
    private final Map<Dependency, V> allUpdates;

    public <K> AbstractUpdatesModel(
            Map<K, V> artifactUpdates, Map<K, V> artifactManagementUpdates, Function<K, Dependency> supplier) {
        this.artifactUpdates = artifactUpdates.entrySet().stream()
                .collect(
                        () -> new TreeMap<>(DependencyComparator.INSTANCE),
                        (map, entry) -> map.put(supplier.apply(entry.getKey()), entry.getValue()),
                        Map::putAll);
        this.artifactManagementUpdates = artifactManagementUpdates.entrySet().stream()
                .collect(
                        () -> new TreeMap<>(DependencyComparator.INSTANCE),
                        (map, entry) -> map.put(supplier.apply(entry.getKey()), entry.getValue()),
                        Map::putAll);
        allUpdates = new TreeMap<>(DependencyComparator.INSTANCE);
        // overriding entries from dependencyManagementUpdates with dependencyUpdates
        allUpdates.putAll(this.artifactManagementUpdates);
        allUpdates.putAll(this.artifactUpdates);
    }

    public Map<Dependency, V> getArtifactUpdates() {
        return artifactUpdates;
    }

    public Map<Dependency, V> getArtifactManagementUpdates() {
        return artifactManagementUpdates;
    }

    public Map<Dependency, V> getAllUpdates() {
        return allUpdates;
    }
}
