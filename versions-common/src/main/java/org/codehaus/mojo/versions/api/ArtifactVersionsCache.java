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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import org.apache.commons.lang3.tuple.Pair;

/**
 * Utility providing a cached {@link ArtifactVersions#getNewestUpdate(Optional)} API
 */
public class ArtifactVersionsCache
{
    private BiFunction<AbstractVersionDetails, Optional<Segment>, ?> cachedFunction;

    private Map<Pair<? extends AbstractVersionDetails, Optional<Segment>>, Object> updateCache = new HashMap<>();

    /**
     * Constructs a new instance given the concrete function for obtaining the details
     *
     * @param cachedFunction reference to the function computing the required information
     */
    public ArtifactVersionsCache( BiFunction<AbstractVersionDetails, Optional<Segment>, ?>
                                          cachedFunction )
    {
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
     * @return last retrieved update information
     */
    @SuppressWarnings( "unchecked" )
    public <V extends AbstractVersionDetails, R> R get( V artifactVersions,
                                                        Optional<Segment> updateScope )
    {
        return (R) updateCache.computeIfAbsent( Pair.of( artifactVersions, updateScope ),
                pair -> cachedFunction.apply( pair.getLeft(), pair.getRight() ) );
    }
}
