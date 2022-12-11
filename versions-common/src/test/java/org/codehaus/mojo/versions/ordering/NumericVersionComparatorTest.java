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

import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class NumericVersionComparatorTest extends VersionComparatorTestBase {
    public NumericVersionComparatorTest() {
        super(new NumericVersionComparator());
    }

    private int instanceCompare(String v1, String v2) {
        return instance.compare(new DefaultArtifactVersion(v1), new DefaultArtifactVersion(v2));
    }

    @Test
    public void testSmokes() {
        assertTrue(instanceCompare("1.0.0.0.0", "1.0.0.0.1") < 0);
        assertTrue(instanceCompare("1.0.0.0.0", "2.0.0.0.1") < 0);
        assertTrue(instanceCompare("1.0.0.0.0", "1.0.0.0") < 0);
        assertTrue(instanceCompare("1.0.0.0.0", "1.0.0.0.0") == 0);
        assertTrue(instanceCompare("1.0.0.0", "1.0.0.0.0") > 0);
    }

    @Test
    public void testBigValues() {
        assertTrue(instanceCompare("1.92.0", "1.100000000000000000000000.0") < 0);
        assertTrue(instanceCompare("1.100000000000000000000000.0", "1.92.0") > 0);
        assertTrue(instanceCompare("1.100000000000000000000000.0", "1.100000000000000000000000.0") == 0);
    }

    @Test
    public void testStringValues() {
        assertTrue(instanceCompare("1.a20.0", "1.a3.0") < 0);
        assertTrue(instanceCompare("1.a20.0", "1.b10.0") < 0);
        assertTrue(instanceCompare("1.a.0.b.0", "1.a.0.b.1") < 0);
        assertTrue(instanceCompare("1.162%a.0.b.0", "1.162%a.0.b.1") < 0);
        assertTrue(instanceCompare("1.162%a.0.b.0", "1.2.0.b.1") < 0);
        assertTrue(instanceCompare("1.0a.0.b.0", "1.162%.0.b.1") < 0);
        assertTrue(instanceCompare("1.a.0.b.0", "2.a.0.b.1") < 0);
        assertTrue(instanceCompare("1.a.0.b.0", "1.a.0.b") < 0);
        assertTrue(instanceCompare("1.a.0.b.0", "1.a.0.b.0") == 0);
        assertTrue(instanceCompare("1.a.0.b", "1.a.0.b.0") > 0);
        assertTrue(instanceCompare("1.a.0.0", "1.a.0") < 0);
        assertTrue(instanceCompare("1.a.0", "1.a.0.0") > 0);
        assertTrue(instanceCompare("1.a.0.1", "1.a.0") > 0);
        assertTrue(instanceCompare("1.a.0", "1.a.0.1") < 0);
        assertTrue(instanceCompare("1.a.0.b", "1.a.0") > 0);
        assertTrue(instanceCompare("1.a.0", "1.a.0.b") < 0);
    }

    @Test
    public void testQualifiers() {
        assertTrue(instanceCompare("1.0-alpha.10", "1.0-alpha.20") < 0);
        assertTrue(instanceCompare("1.0-alpha.10", "1.0-beta.1") < 0);
        assertTrue(instanceCompare("1.0", "1.0-alpha.2") > 0);
        assertTrue(instanceCompare("1.0-alpha.10", "1.0") < 0);
        assertTrue(instanceCompare("1.0.10", "1.0-alpha.10") > 0);
    }

    @Test
    public void testSegmentCounting() {
        assertEquals(1, instance.getSegmentCount(new DefaultArtifactVersion("5")));
        assertEquals(2, instance.getSegmentCount(new DefaultArtifactVersion("5.0")));
        assertEquals(1, instance.getSegmentCount(new DefaultArtifactVersion("5-0")));
        assertEquals(3, instance.getSegmentCount(new DefaultArtifactVersion("5.3.a")));
        assertEquals(6, instance.getSegmentCount(new DefaultArtifactVersion("5.0.a.1.4.5")));
        assertEquals(0, instance.getSegmentCount(new DefaultArtifactVersion("")));
    }

    @Override
    public void testVersionComparatorRow5() {
        // non-numeric -- does not apply
    }

    @Override
    public void testVersionComparatorRow6() {
        // non-numeric -- does not apply
    }
}
