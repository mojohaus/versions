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
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;

/**
 * Unit tests for {@link BoundArtifactVersion}
 */
class BoundArtifactVersionTest {
    @Test
    void testMajorUpperBoundGreaterThanNextMajor() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.2.3", MAJOR);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("2.0.0");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testSubIncrementalUpperBoundGreaterThanNextSubIncremental() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.2.3-2", SUBINCREMENTAL);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.2.3-3");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testVersionShorterThanSegment() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.1", INCREMENTAL);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.1.3");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testVersionBoundArtifactVersionShorterThanConcreteVersionAndSegment() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.1", SUBINCREMENTAL);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.1.3");
        assertThat(bound.compareTo(artifactVersion), lessThan(0));
    }

    @Test
    void testVersionSubIncrementalBoundGreaterThanSubIncremental() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.1", SUBINCREMENTAL);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.1.0-2");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testVersionSubIncrementalBoundGreaterThanIncremental() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.1", INCREMENTAL);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.1.3");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testVersionSubIncrementalBoundGreaterThanMinor() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.1", MINOR);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.3");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }

    @Test
    void testSnapshotWithSubIncremental() {
        BoundArtifactVersion bound = new BoundArtifactVersion("1.0.0-SNAPSHOT", MINOR);
        ArtifactVersion artifactVersion = ArtifactVersionService.getArtifactVersion("1.0.0");
        assertThat(bound.compareTo(artifactVersion), greaterThan(0));
    }
}
