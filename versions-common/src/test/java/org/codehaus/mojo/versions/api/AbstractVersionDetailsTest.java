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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
        assertFalse(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.0")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.1.0")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.1")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithRange()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0]"));
        assertFalse(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.0")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.1.0")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.1")));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoRanges()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0],(1.0.0,2.0.0]"));
        assertFalse(instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                .containsVersion(version("1.0.0")));
        assertFalse(instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                .containsVersion(version("1.1.0")));
        assertFalse(instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                .containsVersion(version("2.0.0")));
        assertTrue(instance.restrictionForUnchangedSegment(version("2.0.0"), of(MAJOR), false)
                .containsVersion(version("2.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges1()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertTrue(instance.restrictionForUnchangedSegment(version("0.9.0"), empty(), false)
                .containsVersion(version("1.0.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges2()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertFalse(instance.restrictionForUnchangedSegment(version("1.0.1"), of(MAJOR), false)
                .containsVersion(version("1.1.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges3()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertFalse(instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                .containsVersion(version("2.0.0")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithTwoNotConnectingRanges4()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("(0.0.1, 1.0.0),(1.0.0,2.0.0]"));
        assertTrue(instance.restrictionForUnchangedSegment(version("1.0.1"), empty(), false)
                .containsVersion(version("2.0.0-1")));
    }

    @Test
    void testRestrictionForUnchangedSegmentWithoutVersionInformation()
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertTrue(instance.restrictionForUnchangedSegment(null, empty(), false).containsVersion(version("1.0.0")));
    }

    @Test
    void testGetSelectedRestrictionForNoVersion() throws InvalidVersionSpecificationException {
        instance.setCurrentVersionRange(VersionRange.createFromVersionSpec("[,0]"));
        assertThrows(NullPointerException.class, () -> instance.getSelectedRestriction(null));
    }

    /**
     * Maven 3 and Maven 4 write the canonical form differently; the classification must not depend on which
     * Maven runs the plugin.
     */
    @Test
    void testIsPreReleaseCanonicalOfMaven3AndMaven4() {
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("3-milestone-2"));
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("3-milestone2"));
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("4-beta-1"));
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("4-beta1"));
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("3.34-rc1"));
        assertTrue(AbstractVersionDetails.isPreReleaseCanonical("7-preview.1"));
        assertFalse(AbstractVersionDetails.isPreReleaseCanonical("3.32.3"));
        assertFalse(AbstractVersionDetails.isPreReleaseCanonical("1-snapshot"));
        assertFalse(AbstractVersionDetails.isPreReleaseCanonical("1-alphabet"));
    }

    @Test
    void testIsPreReleaseVersion() {
        // Stable versions should not be pre-release
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("6.2.3"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("6.2.4"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("2.0.0"));

        // Alpha versions
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-alpha12"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-ALPHA"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-a1"));

        // Beta versions
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-beta"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-beta1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-BETA"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-b1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("2.0.0-beta"));

        // Milestone versions
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-M1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-M2"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("7.0.0-M3"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-milestone"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-milestone1"));

        // Release candidate versions
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-rc"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-rc1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-RC"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-RC1"));

        // Preview versions
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-preview"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0.0-PREVIEW"));

        // Release candidates spelled CR, as in 3.34.0.CR1
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("3.34.0.CR1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("2.0.0-cr1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("5.0.0.Beta1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("1.0-alpha-100"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("4.0.0-beta-4"));

        // Qualifiers Maven does not know, but which mark a pre-release
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("7.0.0-preview.1"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("18-ea"));
        // the JDK and OpenJFX early-access form; the canonical form keeps the plus sign: 28-ea+-12
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("28-ea+12"));
        assertTrue(AbstractVersionDetails.isPreReleaseVersion("2.0.0-pre"));

        // A bare a, b or m is not alpha, beta or milestone to Maven, which orders it after the release
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0-a"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0-b"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0-m"));

        // Releases, snapshots and date-like versions are not pre-releases
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("3.1.0.Final"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0-GA"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("20240303"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0.0-SNAPSHOT"));
        assertFalse(AbstractVersionDetails.isPreReleaseVersion("1.0-alphabet"));

        // Null should return false
        assertFalse(AbstractVersionDetails.isPreReleaseVersion(null));
    }
}
