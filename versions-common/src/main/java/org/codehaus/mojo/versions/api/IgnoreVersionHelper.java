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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Pattern;

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

    public static final List<String> VALID_TYPES = Collections.unmodifiableList(
            Arrays.asList(IgnoreVersion.TYPE_EXACT, IgnoreVersion.TYPE_REGEX, IgnoreVersion.TYPE_RANGE));

    private static final Map<String, BiFunction<Version, IgnoreVersion, Boolean>> VERSION_MATCHERS;

    static {
        VERSION_MATCHERS = new HashMap<>();
        VERSION_MATCHERS.put(IgnoreVersion.TYPE_EXACT, IgnoreVersionHelper::isVersionIgnoredExact);
        VERSION_MATCHERS.put(IgnoreVersion.TYPE_REGEX, IgnoreVersionHelper::isVersionIgnoredRegex);
        VERSION_MATCHERS.put(IgnoreVersion.TYPE_RANGE, IgnoreVersionHelper::isVersionIgnoredRange);
    }

    private IgnoreVersionHelper() {}

    /**
     * Check if type for given ignoredVersion is valid.
     *
     * @param ignoreVersion an ignored version to check
     * @return true if type is valid
     */
    public static boolean isValidType(IgnoreVersion ignoreVersion) {
        return VALID_TYPES.contains(ignoreVersion.getType());
    }

    public static boolean isVersionIgnored(Version version, IgnoreVersion ignoreVersion) {
        return VERSION_MATCHERS.get(ignoreVersion.getType()).apply(version, ignoreVersion);
    }

    private static boolean isVersionIgnoredExact(Version version, IgnoreVersion ignoreVersion) {
        return ignoreVersion.getVersion().equals(version.toString());
    }

    private static boolean isVersionIgnoredRegex(Version version, IgnoreVersion ignoreVersion) {
        return Pattern.compile(ignoreVersion.getVersion())
                .matcher(version.toString())
                .matches();
    }

    private static boolean isVersionIgnoredRange(Version version, IgnoreVersion ignoreVersion) {
        try {
            ArtifactVersion aVersion = ArtifactVersionService.getArtifactVersion(version.toString());
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
