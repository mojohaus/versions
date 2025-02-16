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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.utils.ArtifactVersionUtils.version;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Unit tests for {@link AbstractVersionDetails}
 */
class AbstractVersionDetailsTest {

    private AbstractVersionDetails instance;

    @BeforeEach
    void setUp() {
        instance = new AbstractVersionDetails() {
            @Override
            public ArtifactVersion[] getVersions(boolean includeSnapshots) {
                return new ArtifactVersion[0];
            }
        };
    }

    @Test
    void testRestrictionForUnchangedSegmentWithSimpleVersion() throws InvalidSegmentException {
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")),
                is(false));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")),
                is(true));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.1")),
                is(true));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0-1")),
                is(true));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithRange()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")),
                is(false));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")),
                is(true));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.1")),
                is(true));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0-1")),
                is(true));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoRanges()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0],(1.0.0,2.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")),
                is(false));
        assertThat(
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")),
                is(false));
        assertThat(
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("2.0.0")),
                is(false));
        assertThat(
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("2.0.0-1")),
                is(true));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges1()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("0.9.0"), empty(), false)
                        .containsVersion(version("1.0.0")),
                is(true));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges2()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.1"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")),
                is(false));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges3()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                        .containsVersion(version("2.0.0")),
                is(false));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges4()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                        .containsVersion(version("2.0.0-1")),
                is(true));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithoutVersionInformation()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertThat(
                instance.restrictionForUnchangedSegment(null, empty(), false).containsVersion(version("1.0.0")),
                is(true));
    }

    @Test
    void testGetSelectedRestrictionForNoVersion() throws InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertThrows(NullPointerException.class, () -> instance.getSelectedRestriction(null));
    }
}
