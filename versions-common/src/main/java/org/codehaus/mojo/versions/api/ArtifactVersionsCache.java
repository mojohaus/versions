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
 * under the License.T
 */

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.function.TriFunction;
import org.apache.commons.lang3.tuple.Triple;

/**
 * Utility providing a cached {@link ArtifactVersions#getNewestUpdateWithinSegment(Optional, boolean)} API
 */
public class ArtifactVersionsCache {
    private TriFunction<AbstractVersionDetails, Optional<Segment>, Boolean, ?> cachedFunction;
    private Map<Triple<? extends AbstractVersionDetails, Optional<Segment>, Boolean>, Object> updateCache =
            new ConcurrentHashMap<>();

    /**
     * Constructs a new instance given the concrete function for obtaining the details
     *
     * @param cachedFunction reference to the function computing the required information
     */
    public ArtifactVersionsCache(TriFunction<AbstractVersionDetails, Optional<Segment>, Boolean, ?> cachedFunction) {
        this.cachedFunction = cachedFunction;
    }

    /**
     * Returns the required information for the given {@link ArtifactVersions} object and the given update scope.
     * If the information is already present in cache, the cached version is returned. Otherwise,
     * the {@code artifactVersions} object is queried and the response is cached.
     *
     * @param <V> concrete implementation of {@linkplain AbstractVersionDetails}
     * @param <R> return type of the cached function
     * @param artifactVersions {@linkplain ArtifactVersions} object referring to the given dependency
     * @param updateScope      update scope
     * @param allowSnapshots   whether snapshots should be included
     * @return last retrieved update information
     */
    @SuppressWarnings("unchecked")
    public <V extends AbstractVersionDetails, R> R get(
            V artifactVersions, Optional<Segment> updateScope, boolean allowSnapshots) {
        return (R) updateCache.computeIfAbsent(
                Triple.of(artifactVersions, updateScope, allowSnapshots),
                triple -> cachedFunction.apply(triple.getLeft(), triple.getMiddle(), triple.getRight()));
    }
}
