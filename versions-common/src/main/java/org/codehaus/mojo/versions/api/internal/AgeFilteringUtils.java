package org.codehaus.mojo.versions.api.internal;

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

import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.ArtifactAgeService;
import org.eclipse.aether.repository.ArtifactRepository;
import org.eclipse.aether.repository.RemoteRepository;

/**
 * Utility class for shared age-filtering functionality.
 *
 * @since 2.18.0
 */
public class AgeFilteringUtils {

    private static ThreadLocal<ArtifactAgeService> ageServiceCache = new ThreadLocal<>();

    private AgeFilteringUtils() {
        // utility class
    }

    /**
     * Lazily creates and caches an {@link ArtifactAgeService} per thread.
     *
     * @param log Maven build log
     * @return the {@link ArtifactAgeService}
     */
    public static ArtifactAgeService getArtifactAgeService(Log log) {
        ArtifactAgeService service = ageServiceCache.get();
        if (service == null) {
            service = new ArtifactAgeService(log);
            ageServiceCache.set(service);
        }
        return service;
    }

    /**
     * Converts an {@link ArtifactRepository} to a {@link RemoteRepository}, or returns
     * null if the repository is not a remote repository.
     *
     * @param repository the artifact repository
     * @return the remote repository, or null
     */
    public static RemoteRepository toRemoteRepository(ArtifactRepository repository) {
        return repository instanceof RemoteRepository ? (RemoteRepository) repository : null;
    }

    /**
     * Clears the cached {@link ArtifactAgeService}. Useful for testing.
     */
    public static void clearCache() {
        ageServiceCache.remove();
    }
}
