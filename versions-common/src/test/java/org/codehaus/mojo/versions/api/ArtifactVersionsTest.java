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
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.codehaus.mojo.versions.utils.ArtifactVersionUtils.version;
import static org.codehaus.mojo.versions.utils.ArtifactVersionUtils.versions;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ArtifactVersionsTest {

    @Test
    void test4DigitVersionsMaven() throws Exception {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(versions(
                        "1.0.0.1", "1.0.0.2", "2.121.2.1", "2.100.0.1", "3.1.0.1", "1.1.1", "2.0.0-SNAPSHOT")));
        assertEquals("artifact", instance.getArtifactId());
        assertEquals("group", instance.getGroupId());
        assertArrayEquals(
                versions("1.0.0.1", "1.0.0.2", "1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(false));
        assertArrayEquals(
                versions("1.0.0.1", "1.0.0.2", "1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(true));
        assertArrayEquals(
                versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(new Restriction(version("1.1"), false, null, false), false));
        assertArrayEquals(
                versions("1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(new Restriction(version("1.1"), false, null, false), true));
        assertArrayEquals(
                versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(new Restriction(version("1.0.0.2"), false, null, false), false));
        assertArrayEquals(
                versions("1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1"),
                instance.getVersions(new Restriction(version("1.0.0.2"), false, null, false), true));

        assertEquals(
                version("2.121.2.1"),
                instance.getNewestVersion(new Restriction(version("1.0"), false, version("3.0"), false), false));

        assertNull(instance.getNewestVersion(new Restriction(version("1.1.1"), false, version("2.0"), false), false));
        assertEquals(
                version("2.0.0-SNAPSHOT"),
                instance.getNewestVersion(new Restriction(version("1.1.1"), false, version("2.0"), false), true));
    }

    @Test
    void testIsEmpty() throws Exception {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(versions("1.0.1-SNAPSHOT", "1.0.2-SNAPSHOT")));
        assertEquals(true, instance.isEmpty(false));
        assertEquals(false, instance.isEmpty(true));
    }

    @Test
    void testSmokes() throws Exception {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(versions("1.0", "3.0", "1.1", "1.0", "1.0.1")));
        assertEquals("artifact", instance.getArtifactId());
        assertEquals("group", instance.getGroupId());
        assertArrayEquals(versions("1.0", "1.0.1", "1.1", "3.0"), instance.getVersions(true));
        assertArrayEquals(
                versions("3.0"), instance.getVersions(new Restriction(version("1.1"), false, null, false), true));
        assertArrayEquals(
                versions("1.1", "3.0"),
                instance.getVersions(new Restriction(version("1.0.1"), false, null, false), true));
        assertEquals(
                version("1.1"),
                instance.getNewestVersion(new Restriction(version("1.0"), false, version("3.0"), false), true));
        assertNull(instance.getNewestVersion(new Restriction(version("1.1"), false, version("3.0"), false), true));
    }

    @Test
    void testReportLabels() {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.1", "foo", "bar", "jar", null),
                Arrays.asList(versions(
                        "1.0.1",
                        "1.0",
                        "1.1.0-2",
                        "1.1.1",
                        "1.1.1-2",
                        "1.1.2",
                        "1.1.2-SNAPSHOT",
                        "1.1.3",
                        "1.1",
                        "1.1-SNAPSHOT",
                        "1.2.1",
                        "1.2.2",
                        "1.2",
                        "1.3",
                        "1.9.1-SNAPSHOT",
                        "2.0",
                        "2.1.1-SNAPSHOT",
                        "2.1",
                        "3.0",
                        "3.1.1-SNAPSHOT",
                        "3.1.5-SNAPSHOT",
                        "3.4.0-SNAPSHOT")));

        assertEquals("1.1.0-2", String.valueOf(instance.getNewestUpdateWithinSegment(of(SUBINCREMENTAL), false)));
        assertEquals("1.1.3", String.valueOf(instance.getNewestUpdateWithinSegment(of(INCREMENTAL), false)));
    }

    @Test
    void testGetNewerVersionsWithSnapshot() throws InvalidSegmentException {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.0.0-SNAPSHOT", "foo", "bar", "jar", null),
                Arrays.asList(versions("1.0.0-SNAPSHOT", "1.0.0")));

        assertArrayEquals(
                new ArtifactVersion[] {version("1.0.0")},
                instance.getNewerVersions("1.0.0-SNAPSHOT", of(SUBINCREMENTAL), false, false));
    }

    private static ArtifactVersions createInstance(ArtifactVersion[] versions) {
        return new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.0.0", "foo", "bar", "jar", null),
                Arrays.asList(versions));
    }

    @Test
    void testAllVersionsForIgnoreScopeSubIncremental() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(SUBINCREMENTAL)), false);
        assertEquals(1, filteredVersions.length);
        assertArrayEquals(new ArtifactVersion[] {version("1.0.1")}, filteredVersions);
    }

    @Test
    void testAllVersionsForIgnoreScopeIncremental() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(INCREMENTAL)), false);
        assertEquals(1, filteredVersions.length);
        assertArrayEquals(new ArtifactVersion[] {version("1.1.0")}, filteredVersions);
    }

    @Test
    void testAllVersionsForIgnoreScopeMinor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(MINOR)), false);
        assertEquals(1, filteredVersions.length);
        assertArrayEquals(new ArtifactVersion[] {version("2.0.0")}, filteredVersions);
    }

    @Test
    void testAllVersionsForIgnoreScopeMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(MAJOR)), false);
        assertEquals(0, filteredVersions.length);
    }

    @Test
    void testGetReportNewestUpdateWithOnlyMajorUpdate() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "2.0.0"));
        assertEquals(
                "2.0.0", instance.getReportNewestUpdate(Optional.empty(), true).toString());
        assertEquals("2.0.0", String.valueOf(instance.getReportNewestUpdate(of(MAJOR), true)));
        assertNull(instance.getReportNewestUpdate(of(MINOR), true));
        assertNull(instance.getReportNewestUpdate(of(INCREMENTAL), true));
        assertNull(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true));
    }

    @Test
    void testGetReportNewestUpdateWithMinorAndMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.1.0", "2.0.0"));
        assertEquals(
                "2.0.0", instance.getReportNewestUpdate(Optional.empty(), true).toString());
        assertEquals("2.0.0", String.valueOf(instance.getReportNewestUpdate(of(MAJOR), true)));
        assertEquals("1.1.0", String.valueOf(instance.getReportNewestUpdate(of(MINOR), true)));
        assertNull(instance.getReportNewestUpdate(of(INCREMENTAL), true));
        assertNull(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true));
    }

    @Test
    void testGetReportNewestUpdateWithIncrementalAndMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.1", "2.0.0"));
        assertEquals(
                "2.0.0", instance.getReportNewestUpdate(Optional.empty(), true).toString());
        assertEquals("2.0.0", String.valueOf(instance.getReportNewestUpdate(of(MAJOR), true)));
        assertNull(instance.getReportNewestUpdate(of(MINOR), true));
        assertEquals("1.0.1", String.valueOf(instance.getReportNewestUpdate(of(INCREMENTAL), true)));
        assertNull(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true));
    }

    @Test
    void testGetNewestVersionWithLesserSegment() throws InvalidSegmentException {
        ArtifactVersions instance = createInstance(versions("1.0.0-1"));
        assertEquals(
                "1.0.0-1",
                String.valueOf(instance.getNewestVersion("1.0.0", of(MAJOR), false, false)
                        .get()));
        assertEquals(
                "1.0.0-1",
                String.valueOf(instance.getNewestVersion("1.0.0", of(MINOR), false, false)
                        .get()));
        assertEquals(
                "1.0.0-1",
                String.valueOf(instance.getNewestVersion("1.0.0", of(INCREMENTAL), false, false)
                        .get()));
    }

    @Test
    void testGetNewestVersionWithLesserSegmentWithSnapshots() throws InvalidSegmentException {
        ArtifactVersions instance = createInstance(versions("1.0.0-1-SNAPSHOT"));
        assertEquals(
                "1.0.0-1-SNAPSHOT",
                String.valueOf(instance.getNewestVersion("1.0.0", of(MAJOR), true, false)
                        .get()));
        assertEquals(
                "1.0.0-1-SNAPSHOT",
                String.valueOf(instance.getNewestVersion("1.0.0", of(MINOR), true, false)
                        .get()));
        assertEquals(
                "1.0.0-1-SNAPSHOT",
                String.valueOf(instance.getNewestVersion("1.0.0", of(INCREMENTAL), true, false)
                        .get()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"(1, 3]", "(0, 1], (2, 3]"})
    void testAllUpdatesWithCurrentVersionRange(String versionRange) throws InvalidVersionSpecificationException {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "default-group",
                        "dummy-api",
                        VersionRange.createFromVersionSpec(versionRange),
                        "foo",
                        "bar",
                        "jar",
                        null),
                Stream.of("1", "2", "3", "4")
                        .map(ArtifactVersionService::getArtifactVersion)
                        .collect(Collectors.toList()));
        List<String> allUpdates = Arrays.stream(instance.getAllUpdates(false))
                .map(Object::toString)
                .collect(Collectors.toList());
        assertTrue(allUpdates.contains("4"), () -> String.valueOf(allUpdates));
        assertFalse(allUpdates.contains("1"), () -> String.valueOf(allUpdates));
        assertFalse(allUpdates.contains("2"), () -> String.valueOf(allUpdates));
        assertFalse(allUpdates.contains("3"), () -> String.valueOf(allUpdates));
    }

    @Test
    void testPredicate() {
        ArtifactVersions original = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.1.2-SNAPSHOT", "foo", "bar", "jar", null),
                Arrays.asList(versions(
                        "1.0.1",
                        "1.0",
                        "1.1.0-2",
                        "1.1.1",
                        "1.1.1-2",
                        "1.1.2",
                        "1.1.2-SNAPSHOT",
                        "1.1.3",
                        "1.1",
                        "1.1-SNAPSHOT",
                        "1.2.1",
                        "1.2.2",
                        "1.2",
                        "1.3",
                        "1.9.1-SNAPSHOT",
                        "2.0",
                        "2.1.1-SNAPSHOT",
                        "2.1",
                        "3.0",
                        "3.1.1-SNAPSHOT",
                        "3.1.5-SNAPSHOT",
                        "3.4.0-SNAPSHOT")));
        ArtifactVersions onlySnapshots = new ArtifactVersions(original).filter(ArtifactUtils::isSnapshot);
        assertEquals(0, onlySnapshots.getVersions(false).length);
        assertEquals(7, onlySnapshots.getVersions(true).length);
        assertEquals(original.getCurrentVersion(), onlySnapshots.getCurrentVersion());

        ArtifactVersions noSnapshots = new ArtifactVersions(original).filter(v -> !ArtifactUtils.isSnapshot(v));
        assertEquals(15, noSnapshots.getVersions(false).length);
        assertEquals(15, noSnapshots.getVersions(true).length);
        assertNull(noSnapshots.getCurrentVersion());
    }

    @Test
    void testRestrictionForUnchangedSegmentIfActualVersionNullAndUpperRestrictionNull() throws InvalidSegmentException {
        ArtifactVersions versions = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "(,2)", "foo", "bar", "jar", null),
                Arrays.asList(versions("1.0.0")));
        assertEquals(
                new Restriction(null, false, null, false),
                versions.restrictionForUnchangedSegment(null, Optional.of(MAJOR), false));
    }

    @Test
    void testRestrictionForSelectedSegmentIfLowerBoundNull() throws InvalidSegmentException {
        ArtifactVersions versions = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "(,)", "foo", "bar", "jar", null),
                Arrays.asList(versions("1.0.0")));
        assertEquals(
                new Restriction(null, false, null, false),
                versions.restrictionForSelectedSegment(null, Optional.of(MAJOR)));
    }

    @Test
    void testRestrictionForIgnoreScopeLowerBoundNull() throws InvalidSegmentException {
        ArtifactVersions versions = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "(,)", "foo", "bar", "jar", null),
                Arrays.asList(versions("1.0.0")));
        assertEquals(
                new Restriction(null, false, null, false),
                versions.restrictionForIgnoreScope(null, Optional.of(MAJOR)));
    }
}
