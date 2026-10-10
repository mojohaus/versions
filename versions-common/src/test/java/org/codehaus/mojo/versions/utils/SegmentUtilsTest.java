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
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for {@link SegmentUtils}
 */
class SegmentUtilsTest {
    @Test
    void testIncremental() {
        assertEquals(of(INCREMENTAL), determineUnchangedSegment(false, false, false, null));
        assertEquals(of(INCREMENTAL), determineUnchangedSegment(true, false, false, null));
        assertEquals(of(INCREMENTAL), determineUnchangedSegment(true, true, false, null));
    }

    @Test
    void testMinor() {
        assertEquals(of(MINOR), determineUnchangedSegment(false, false, true, null));
        assertEquals(of(MINOR), determineUnchangedSegment(true, false, true, null));
    }

    @Test
    void testMajor() {
        assertEquals(of(MAJOR), determineUnchangedSegment(false, true, true, null));
    }
}
