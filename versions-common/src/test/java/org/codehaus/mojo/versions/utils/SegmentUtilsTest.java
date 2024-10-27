package org.codehaus.mojo.versions.utils;
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

import org.junit.jupiter.api.Test;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.utils.SegmentUtils.determineUnchangedSegment;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * Unit tests for {@link SegmentUtils}
 */
class SegmentUtilsTest {
    @Test
    void testIncremental() {
        assertThat(determineUnchangedSegment(false, false, false, null), is(of(INCREMENTAL)));
        assertThat(determineUnchangedSegment(true, false, false, null), is(of(INCREMENTAL)));
        assertThat(determineUnchangedSegment(true, true, false, null), is(of(INCREMENTAL)));
    }

    @Test
    void testMinor() {
        assertThat(determineUnchangedSegment(false, false, true, null), is(of(MINOR)));
        assertThat(determineUnchangedSegment(true, false, true, null), is(of(MINOR)));
    }

    @Test
    void testMajor() {
        assertThat(determineUnchangedSegment(false, true, true, null), is(of(MAJOR)));
    }
}
