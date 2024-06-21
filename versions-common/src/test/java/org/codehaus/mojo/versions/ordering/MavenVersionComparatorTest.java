package org.codehaus.mojo.versions.ordering;

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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.core.Is.is;

class MavenVersionComparatorTest extends VersionComparatorTestBase {
    MavenVersionComparatorTest() {
        super(new MavenVersionComparator());
    }

    @Test
    void testSegmentCounting() {
        assertThat(3, is(instance.getSegmentCount(version("5"))));
        assertThat(3, is(instance.getSegmentCount(version("5.0"))));
        assertThat(4, is(instance.getSegmentCount(version("5-0"))));
        assertThat(1, is(instance.getSegmentCount(version("5.3.a"))));
        assertThat(1, is(instance.getSegmentCount(version("5.0.a.1.4.5"))));
        assertThat(3, is(instance.getSegmentCount(version(""))));
    }

    @Test
    void testUpperBoundaryCustom() {
        assertThat(
                instance.compare(new BoundArtifactVersion(version("1.2.3"), INCREMENTAL), version("1.2.3-ANDRZEJ")),
                greaterThan(0));
    }

    @Test
    void testUpperBoundaryRelease() {
        assertThat(
                instance.compare(new BoundArtifactVersion(version("1.1.0"), INCREMENTAL), version("1.1.0")),
                greaterThan(0));
    }

    @Test
    void testUpperBoundarySnapshot() {
        assertThat(
                instance.compare(new BoundArtifactVersion(version("1.1.0"), INCREMENTAL), version("1.1.0-SNAPSHOT")),
                greaterThan(0));
    }

    @Test
    void testScopeLessThanNumSegmentsUpper() {
        ArtifactVersion artifactVersion = new BoundArtifactVersion(version("1.1"), SUBINCREMENTAL);
        assertThat(artifactVersion.compareTo(version("1.0.1")), greaterThan(0));
        assertThat(artifactVersion.compareTo(version("1.1-SNAPSHOT")), greaterThan(0));
        assertThat(artifactVersion.compareTo(version("1.1")), greaterThan(0));
        assertThat(artifactVersion.compareTo(version("1.1.0-2")), greaterThan(0));
        assertThat(artifactVersion.compareTo(version("1.1.1-2")), lessThan(0));
    }
}
