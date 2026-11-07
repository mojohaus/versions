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
import static org.junit.jupiter.api.Assertions.assertEquals;
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
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.1")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithRange()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0]"));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.1")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoRanges()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0],(1.0.0,2.0.0]"));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.0.0")));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("2.0.0")));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                        .containsVersion(version("2.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges1()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("0.9.0"), empty(), false)
                        .containsVersion(version("1.0.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges2()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("1.0.1"), of(MAJOR), false)
                        .containsVersion(version("1.1.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges3()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertEquals(
                false,
                instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                        .containsVersion(version("2.0.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges4()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                        .containsVersion(version("2.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithoutVersionInformation()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertEquals(
                true,
                instance.restrictionForUnchangedSegment(null, empty(), false).containsVersion(version("1.0.0")));
    }

    @Test
    void testGetSelectedRestrictionForNoVersion() throws InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertThrows(NullPointerException.class, () -> instance.getSelectedRestriction(null));
    }

    @Test
    void testIsPreReleaseVersion() {
        // Stable versions should not be pre-release
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("6.2.3"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("6.2.4"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("2.0.0"));

        // Alpha versions
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha12"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-ALPHA"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-a1"));

        // Beta versions
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-beta"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-beta1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-BETA"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-b1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("2.0.0-beta"));

        // Milestone versions
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-M1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-M2"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("7.0.0-M3"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-milestone"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-milestone1"));

        // Release candidate versions
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-rc"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-rc1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-RC"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-RC1"));

        // Preview versions
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-preview"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0.0-PREVIEW"));

        // Release candidates spelled CR, as in 3.34.0.CR1
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("3.34.0.CR1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("2.0.0-cr1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("5.0.0.Beta1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("1.0-alpha-100"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("4.0.0-beta-4"));

        // Qualifiers Maven does not know, but which mark a pre-release
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("7.0.0-preview.1"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("18-ea"));
        assertEquals(true, AbstractVersionDetails.isPreReleaseVersion("2.0.0-pre"));

        // A bare a, b or m is not alpha, beta or milestone to Maven, which orders it after the release
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0-a"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0-b"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0-m"));

        // Releases, snapshots and date-like versions are not pre-releases
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("3.1.0.Final"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0-GA"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("20240303"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0.0-SNAPSHOT"));
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion("1.0-alphabet"));

        // Null should return false
        assertEquals(false, AbstractVersionDetails.isPreReleaseVersion(null));
    }
}
