package org.codehaus.mojo.versions.api;

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
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.eclipse.aether.version.Version;

/**
 * Helper class for {@link IgnoreVersion}
 */
public class IgnoreVersionHelper {

    static class IgnoreVersionException extends RuntimeException {
        IgnoreVersionException(Throwable cause) {
            super(cause);
        }
    }

    /**
     * Provides an instance of a list of valid types of {@link IgnoreVersion}
     */
    public static final List<String> VALID_TYPES = Arrays.stream(IgnoreVersionType.values())
            .map(IgnoreVersionType::getType)
            .collect(Collectors.toList());

    public enum IgnoreVersionType {
        TYPE_EXACT(IgnoreVersion.TYPE_EXACT, IgnoreVersionHelper::isVersionIgnoredExact),
        TYPE_REGEX(IgnoreVersion.TYPE_REGEX, IgnoreVersionHelper::isVersionIgnoredRegex),
        TYPE_RANGE(IgnoreVersion.TYPE_RANGE, IgnoreVersionHelper::isVersionIgnoredRange);

        private final String type;

        private final BiFunction<String, IgnoreVersion, Boolean> predicate;

        IgnoreVersionType(String type, BiFunction<String, IgnoreVersion, Boolean> predicate) {
            this.type = type;
            this.predicate = predicate;
        }

        public String getType() {
            return type;
        }

        public BiFunction<String, IgnoreVersion, Boolean> getPredicate() {
            return predicate;
        }

        public static Optional<IgnoreVersionType> forType(String type) {
            for (IgnoreVersionType v : IgnoreVersionType.values()) {
                if (v.getType().equals(type)) {
                    return Optional.of(v);
                }
            }
            return Optional.empty();
        }
    }

    private IgnoreVersionHelper() {}

    /**
     * Check if type for given ignoredVersion is valid.
     *
     * @param ignoreVersion an ignored version to check
     * @return {@code true} if type is valid
     */
    public static boolean isValidType(IgnoreVersion ignoreVersion) {
        return IgnoreVersionType.forType(ignoreVersion.getType()).isPresent();
    }

    /**
     * Check if the provided version is ignored by the provided {@link IgnoreVersion} instance
     *
     * @param v version string to be checked
     * @param ignoreVersion {@link IgnoreVersion} instance providing the filter
     * @return {@code true} if the provided version is ignored
     */
    public static boolean isVersionIgnored(String v, IgnoreVersion ignoreVersion) {
        return IgnoreVersionType.forType(ignoreVersion.getType())
                .map(t -> t.getPredicate().apply(v, ignoreVersion))
                .orElseThrow(() -> new IllegalArgumentException("Invalid version type: " + ignoreVersion.getType()));
    }

    /**
     * Check if the provided version is ignored by the provided {@link IgnoreVersion} instance
     *
     * @param v version string to be checked
     * @param ignoreVersion {@link IgnoreVersion} instance providing the filter
     * @return {@code true} if the provided version is ignored
     */
    public static boolean isVersionIgnored(Version v, IgnoreVersion ignoreVersion) {
        return isVersionIgnored(v.toString(), ignoreVersion);
    }

    private static boolean isVersionIgnoredExact(String v, IgnoreVersion ignoreVersion) {
        return ignoreVersion.getVersion().equals(v);
    }

    private static boolean isVersionIgnoredRegex(String v, IgnoreVersion ignoreVersion) {
        return Pattern.compile(ignoreVersion.getVersion()).matcher(v).matches();
    }

    private static boolean isVersionIgnoredRange(String v, IgnoreVersion ignoreVersion) {
        try {
            ArtifactVersion aVersion = ArtifactVersionService.getArtifactVersion(v);
            VersionRange versionRange = VersionRange.createFromVersionSpec(ignoreVersion.getVersion());
            if (versionRange.hasRestrictions()) {
                return versionRange.containsVersion(aVersion);
            } else {
                return versionRange.getRecommendedVersion().equals(aVersion);
            }
        } catch (InvalidVersionSpecificationException e) {
            throw new IgnoreVersionException(e);
        }
    }
}
