package org.codehaus.mojo.versions.utils;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.Arrays;

import org.apache.maven.artifact.versioning.ArtifactVersion;

/**
 * Misc utilities for manipulating {@link org.apache.maven.artifact.versioning.ArtifactVersion} objects
 */
public class ArtifactVersionUtils {

    /**
     * Creates an {@link ArtifactVersion} from the given version string
     * @param version string representation of the version
     * @return {@link ArtifactVersion} from the given version string
     */
    public static ArtifactVersion version(String version) {
        return ArtifactVersionService.getArtifactVersion(version);
    }

    /**
     * Produces an array of {@link ArtifactVersion} objects based on the array of version strings
     * @param versions array of version strings
     * @return array of {@link ArtifactVersion} objects based on the input array of version strings
     */
    public static ArtifactVersion[] versions(String... versions) {
        return Arrays.stream(versions)
                .map(ArtifactVersionService::getArtifactVersion)
                .toArray(ArtifactVersion[]::new);
    }
}
