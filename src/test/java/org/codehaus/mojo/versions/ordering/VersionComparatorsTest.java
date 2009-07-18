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

import junit.framework.TestCase;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.ArtifactVersion;

public class VersionComparatorsTest
    extends TestCase
{
    private final String[] versionDataset = {
        "1",
        "1.0",
        "1.0.0",
        "1.0.0-1",
        "1.0.0.sp1",
        "foobar",
        "1-alpha-1",
    };

    public void testMavenVersionComparator() {
        assertVersions( new MavenVersionComparator() );
    }
    public void testMercuryVersionComparator() {
        assertVersions( new MercuryVersionComparator());
    }
    public void testNumericVersionComparator() {
        assertVersions( new NumericVersionComparator() );
    }

    public void assertVersions(VersionComparator instance) {
        for (int i = 0; i < versionDataset.length; i++) {
            assertLater( versionDataset[i], instance );
            assertLater( versionDataset[i]+"-SNAPSHOT", instance );
        }
    }

    public void assertLater(String version, VersionComparator instance) {
        ArtifactVersion v1 = new DefaultArtifactVersion( version);
        int count = instance.getSegmentCount( v1 );
        for (int i = 0; i < count; i++) {
            ArtifactVersion v2 = instance.incrementSegment( v1, i );
            assertTrue(v1.toString() + " < " + v2.toString(), instance.compare( v1, v2 ) < 0);
        }
    }
}
