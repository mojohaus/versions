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

import java.util.stream.Stream;

import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.VersionScheme;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.params.provider.Arguments.of;

class IgnoreVersionHelperTest {

    private static final VersionScheme VERSION_SCHEME = new GenericVersionScheme();

    static Stream<Arguments> ignoredTypeExact() {
        return Stream.of(of("1.0.0", "1.0.0", true), of("2.0.0", "1.0.0", false));
    }

    @ParameterizedTest
    @MethodSource
    void ignoredTypeExact(String version, String ignore, boolean expected) throws Exception {

        IgnoreVersion ignoreVersion = aIgnoreVersion(ignore, IgnoreVersion.TYPE_EXACT);

        assertEquals(
                expected, IgnoreVersionHelper.isVersionIgnored(VERSION_SCHEME.parseVersion(version), ignoreVersion));
    }

    public static Stream<Arguments> ignoredTypeRegex() {
        return Stream.of(
                of("1.0.0", "1\\.0\\..*", true),
                of("1.0.0-SNAPSHOT", ".*-SNAPSHOT", true),
                of("1.0.0", "2\\.0\\..*", false));
    }

    @ParameterizedTest
    @MethodSource
    void ignoredTypeRegex(String version, String ignore, boolean expected) throws Exception {

        IgnoreVersion ignoreVersion = aIgnoreVersion(ignore, IgnoreVersion.TYPE_REGEX);

        assertEquals(
                expected, IgnoreVersionHelper.isVersionIgnored(VERSION_SCHEME.parseVersion(version), ignoreVersion));
    }

    public static Stream<Arguments> ignoredTypeRange() {
        return Stream.of(
                of("1.0.0", "[1.0,)", true),
                of("1.0.0", "(,2.0.0]", true),
                of("2.2.0", "[1.0,3)", true),
                of("2.2.0", "[1.0,2.0)", false),
                of("2.2.0", "(,2.0.0]", false),
                of("1.0.0", "1.0.0", true),
                of("1.0.0", "1.0", true),
                of("1.0.1", "1.0", false),
                of("1.0.0", "2.0.0", false));
    }

    @ParameterizedTest
    @MethodSource
    void ignoredTypeRange(String version, String ignore, boolean expected) throws Exception {

        IgnoreVersion ignoreVersion = aIgnoreVersion(ignore, IgnoreVersion.TYPE_RANGE);

        assertEquals(
                expected, IgnoreVersionHelper.isVersionIgnored(VERSION_SCHEME.parseVersion(version), ignoreVersion));
    }

    @Test
    void invalidRangeShouldThrowException() throws Exception {
        IgnoreVersion ignoreVersion = aIgnoreVersion("[1,,", IgnoreVersion.TYPE_RANGE);

        assertThrows(
                IgnoreVersionHelper.IgnoreVersionException.class,
                () -> IgnoreVersionHelper.isVersionIgnored(VERSION_SCHEME.parseVersion("1.0.0"), ignoreVersion));
    }

    private IgnoreVersion aIgnoreVersion(String version, String type) {
        IgnoreVersion ignoreVersion = new IgnoreVersion();
        ignoreVersion.setVersion(version);
        ignoreVersion.setType(type);
        return ignoreVersion;
    }
}
