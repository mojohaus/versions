package org.codehaus.mojo.versions.utils;
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
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.commons.collections4.map.LRUMap;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 * Simple cache for {@link org.apache.maven.artifact.versioning.ArtifactVersion}
 */
public class ArtifactVersionService {
    private static final int MAX_CACHE_SIZE = 512;
    private static final Map<String, DefaultArtifactVersion> CACHE = new LRUMap<>(MAX_CACHE_SIZE);
    private static final ReentrantReadWriteLock CACHE_LOCK = new ReentrantReadWriteLock();

    /**
     * Get a ComparableVersion representing the version in a string.
     */
    public static ArtifactVersion getArtifactVersion(String version) {
        try {
            CACHE_LOCK.readLock().lock();
            DefaultArtifactVersion result = CACHE.get(version);
            if (result != null) {
                return result;
            }
        } finally {
            CACHE_LOCK.readLock().unlock();
        }
        try {
            CACHE_LOCK.writeLock().lock();
            return CACHE.computeIfAbsent(version, DefaultArtifactVersion::new);
        } finally {
            CACHE_LOCK.writeLock().unlock();
        }
    }
}
