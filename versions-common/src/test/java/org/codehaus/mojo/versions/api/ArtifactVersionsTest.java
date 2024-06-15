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
import java.util.Optional;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.ordering.MercuryVersionComparator;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.junit.Test;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.codehaus.mojo.versions.utils.ArtifactVersionUtils.version;
import static org.codehaus.mojo.versions.utils.ArtifactVersionUtils.versions;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.arrayContaining;
import static org.hamcrest.Matchers.arrayWithSize;
import static org.hamcrest.Matchers.hasToString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ArtifactVersionsTest {

    private void test4DigitVersion(VersionComparator comparator) throws InvalidVersionSpecificationException {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(
                        versions("1.0.0.1", "1.0.0.2", "2.121.2.1", "2.100.0.1", "3.1.0.1", "1.1.1", "2.0.0-SNAPSHOT")),
                comparator);
        assertEquals("artifact", instance.getArtifactId());
        assertEquals("group", instance.getGroupId());
        assertThat(
                instance.getVersions(false),
                arrayContaining(versions("1.0.0.1", "1.0.0.2", "1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1")));
        assertThat(
                instance.getVersions(true),
                arrayContaining(versions(
                        "1.0.0.1", "1.0.0.2", "1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1")));
        assertThat(
                instance.getVersions(new Restriction(version("1.1"), false, null, false), false),
                arrayContaining(versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1")));
        assertThat(
                instance.getVersions(new Restriction(version("1.1"), false, null, false), true),
                arrayContaining(versions("1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1")));
        assertThat(
                instance.getVersions(new Restriction(version("1.0.0.2"), false, null, false), false),
                // Matchers.arrayContaining(versions("1.1.1", "2.121.2.1", "2.100.0.1", "3.1.0.1")));
                arrayContaining(versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1")));
        assertThat(
                instance.getVersions(new Restriction(version("1.0.0.2"), false, null, false), true),
                // Matchers.arrayContaining(versions("1.1.1", "2.121.2.1", "2.100.0.1", "3.1.0.1")));
                arrayContaining(versions("1.1.1", "2.0.0-SNAPSHOT", "2.100.0.1", "2.121.2.1", "3.1.0.1")));

        assertEquals(
                version("2.121.2.1"),
                instance.getNewestVersion(new Restriction(version("1.0"), false, version("3.0"), false), false));

        assertNull(instance.getNewestVersion(new Restriction(version("1.1.1"), false, version("2.0"), false), false));
        assertEquals(
                version("2.0.0-SNAPSHOT"),
                instance.getNewestVersion(new Restriction(version("1.1.1"), false, version("2.0"), false), true));
    }

    @Test
    public void test4DigitVersionsMercury() throws Exception {
        test4DigitVersion(new MercuryVersionComparator());
    }

    @Test
    public void test4DigitVersionsMaven() throws Exception {
        test4DigitVersion(new MavenVersionComparator());
    }

    @Test
    public void testIsEmpty() throws Exception {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(versions("1.0.1-SNAPSHOT", "1.0.2-SNAPSHOT")),
                new MavenVersionComparator());
        assertThat(instance.isEmpty(false), is(true));
        assertThat(instance.isEmpty(true), is(false));
    }

    @Test
    public void testSmokes() throws Exception {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact(
                        "group",
                        "artifact",
                        VersionRange.createFromVersionSpec("[1.0,3.0]"),
                        "foo",
                        "bar",
                        "jar",
                        new DefaultArtifactHandler()),
                Arrays.asList(versions("1.0", "3.0", "1.1", "1.0", "1.0.1")),
                new MavenVersionComparator());
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
    public void testReportLabels() {
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
                        "3.4.0-SNAPSHOT")),
                new MavenVersionComparator());

        assertThat(instance.getNewestUpdateWithinSegment(of(SUBINCREMENTAL), false), hasToString("1.1.0-2"));
        assertThat(instance.getNewestUpdateWithinSegment(of(INCREMENTAL), false), hasToString("1.1.3"));
    }

    @Test
    public void testGetNewerVersionsWithSnapshot() throws InvalidSegmentException {
        ArtifactVersions instance = new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.0.0-SNAPSHOT", "foo", "bar", "jar", null),
                Arrays.asList(versions("1.0.0-SNAPSHOT", "1.0.0")),
                new MavenVersionComparator());

        assertThat(
                instance.getNewerVersions("1.0.0-SNAPSHOT", of(SUBINCREMENTAL), false, false),
                arrayContaining(version("1.0.0")));
    }

    private static ArtifactVersions createInstance(ArtifactVersion[] versions) {
        return new ArtifactVersions(
                new DefaultArtifact("default-group", "dummy-api", "1.0.0", "foo", "bar", "jar", null),
                Arrays.asList(versions),
                new MavenVersionComparator());
    }

    @Test
    public void testAllVersionsForIgnoreScopeSubIncremental() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(SUBINCREMENTAL)), false);
        assertThat(filteredVersions, arrayWithSize(1));
        assertThat(filteredVersions, arrayContaining(version("1.0.1")));
    }

    @Test
    public void testAllVersionsForIgnoreScopeIncremental() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(INCREMENTAL)), false);
        assertThat(filteredVersions, arrayWithSize(1));
        assertThat(filteredVersions, arrayContaining(version("1.1.0")));
    }

    @Test
    public void testAllVersionsForIgnoreScopeMinor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(MINOR)), false);
        assertThat(filteredVersions, arrayWithSize(1));
        assertThat(filteredVersions, arrayContaining(version("2.0.0")));
    }

    @Test
    public void testAllVersionsForIgnoreScopeMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"));
        ArtifactVersion[] filteredVersions = instance.getVersions(
                instance.restrictionForIgnoreScope(instance.getCurrentVersion(), of(MAJOR)), false);
        assertThat(filteredVersions, arrayWithSize(0));
    }

    @Test
    public void testGetReportNewestUpdateWithOnlyMajorUpdate() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "2.0.0"));
        assertThat(instance.getReportNewestUpdate(Optional.empty(), true).toString(), is("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MAJOR), true), hasToString("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MINOR), true), nullValue());
        assertThat(instance.getReportNewestUpdate(of(INCREMENTAL), true), nullValue());
        assertThat(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true), nullValue());
    }

    @Test
    public void testGetReportNewestUpdateWithMinorAndMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.1.0", "2.0.0"));
        assertThat(instance.getReportNewestUpdate(Optional.empty(), true).toString(), is("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MAJOR), true), hasToString("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MINOR), true), hasToString("1.1.0"));
        assertThat(instance.getReportNewestUpdate(of(INCREMENTAL), true), nullValue());
        assertThat(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true), nullValue());
    }

    @Test
    public void testGetReportNewestUpdateWithIncrementalAndMajor() {
        ArtifactVersions instance = createInstance(versions("1.0.0", "1.0.1", "2.0.0"));
        assertThat(instance.getReportNewestUpdate(Optional.empty(), true).toString(), is("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MAJOR), true), hasToString("2.0.0"));
        assertThat(instance.getReportNewestUpdate(of(MINOR), true), nullValue());
        assertThat(instance.getReportNewestUpdate(of(INCREMENTAL), true), hasToString("1.0.1"));
        assertThat(instance.getReportNewestUpdate(of(SUBINCREMENTAL), true), nullValue());
    }

    @Test
    public void testGetNewestVersionWithLesserSegment() throws InvalidSegmentException {
        ArtifactVersions instance = createInstance(versions("1.0.0-1"));
        assertThat(instance.getNewestVersion("1.0.0", of(MAJOR), false, false).get(), hasToString("1.0.0-1"));
        assertThat(instance.getNewestVersion("1.0.0", of(MINOR), false, false).get(), hasToString("1.0.0-1"));
        assertThat(
                instance.getNewestVersion("1.0.0", of(INCREMENTAL), false, false)
                        .get(),
                hasToString("1.0.0-1"));
    }

    @Test
    public void testGetNewestVersionWithLesserSegmentWithSnapshots() throws InvalidSegmentException {
        ArtifactVersions instance = createInstance(versions("1.0.0-1-SNAPSHOT"));
        assertThat(instance.getNewestVersion("1.0.0", of(MAJOR), true, false).get(), hasToString("1.0.0-1-SNAPSHOT"));
        assertThat(instance.getNewestVersion("1.0.0", of(MINOR), true, false).get(), hasToString("1.0.0-1-SNAPSHOT"));
        assertThat(
                instance.getNewestVersion("1.0.0", of(INCREMENTAL), true, false).get(),
                hasToString("1.0.0-1-SNAPSHOT"));
    }
}
