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
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.codehaus.mojo.versions.api.Segment;
import org.junit.Test;

import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.core.Is.is;

public class MavenVersionComparatorTest
{
    private MavenVersionComparator instance = new MavenVersionComparator();

    @Test
    public void testSegmentCounting()
        throws Exception
    {
        assertThat( 3, is( instance.getSegmentCount( new DefaultArtifactVersion( "5" ) ) ) );
        assertThat( 3, is( instance.getSegmentCount( new DefaultArtifactVersion( "5.0" ) ) ) );
        assertThat( 4, is( instance.getSegmentCount( new DefaultArtifactVersion( "5-0" ) ) ) );
        assertThat( 1, is( instance.getSegmentCount( new DefaultArtifactVersion( "5.3.a" ) ) ) );
        assertThat( 1, is( instance.getSegmentCount( new DefaultArtifactVersion( "5.0.a.1.4.5" ) ) ) );
        assertThat( 3, is( instance.getSegmentCount( new DefaultArtifactVersion( "" ) ) ) );
    }

    @Test
    public void testSegmentIncrementing() throws InvalidSegmentException
    {
        assertIncrement( "6-SNAPSHOT", "5", MAJOR );
        assertIncrement( "6.0-SNAPSHOT", "5.0", MAJOR );
        assertIncrement( "5.1-SNAPSHOT", "5.0", MINOR );
        assertIncrement( "5.1.0-SNAPSHOT", "5.0.1", MINOR );
        assertIncrement( "5.alpha.2-SNAPSHOT", "5.alpha.1", MAJOR );
        assertIncrement( "5.alpha-1.2-SNAPSHOT", "5.alpha-1.1", MAJOR );
        assertIncrement( "5.alpha-1.ba-SNAPSHOT", "5.alpha-1.az", MAJOR );
        assertIncrement( "5.alpha-wins.2-SNAPSHOT", "5.alpha-wins.1", MAJOR );
        assertIncrement( "1.0-alpha-3-SNAPSHOT", "1.0-alpha-2-SNAPSHOT", SUBINCREMENTAL );
        assertIncrement( "1.0-alpha-90-SNAPSHOT", "1.0-alpha-9-SNAPSHOT", SUBINCREMENTAL );
        assertIncrement( "1.0-za-SNAPSHOT", "1.0-z-SNAPSHOT", SUBINCREMENTAL );
        assertIncrement( "1.0-z90-SNAPSHOT", "1.0-z9-SNAPSHOT", SUBINCREMENTAL );
    }

    private void assertIncrement( String expected, String initial, Segment segment ) throws InvalidSegmentException
    {
        assertThat( instance.incrementSegment( new DefaultArtifactVersion( initial ), segment ).toString(),
                is( expected ) );
    }

    @Test
    public void testUpperBoundaryCustom()
    {
        assertThat( instance.compare( new BoundArtifactVersion( new DefaultArtifactVersion( "1.2.3" ),
                INCREMENTAL ), new DefaultArtifactVersion( "1.2.3-ANDRZEJ" ) ), greaterThan( 0 ) );
    }

    @Test
    public void testUpperBoundaryRelease()
    {
        assertThat( instance.compare( new BoundArtifactVersion( new DefaultArtifactVersion( "1.1.0" ),
                INCREMENTAL ), new DefaultArtifactVersion( "1.1.0" ) ), greaterThan( 0 ) );
    }

    @Test
    public void testUpperBoundarySnapshot()
    {
        assertThat( instance.compare( new BoundArtifactVersion( new DefaultArtifactVersion( "1.1.0" ),
                INCREMENTAL ), new DefaultArtifactVersion( "1.1.0-SNAPSHOT" ) ), greaterThan( 0 ) );
    }

    @Test
    public void testScopeLessThanNumSegmentsUpper()
    {
        ArtifactVersion artifactVersion = new BoundArtifactVersion( new DefaultArtifactVersion( "1.1" ),
                SUBINCREMENTAL );
        assertThat( artifactVersion.compareTo( new DefaultArtifactVersion( "1.0.1" ) ), greaterThan( 0 ) );
        assertThat( artifactVersion.compareTo( new DefaultArtifactVersion( "1.1-SNAPSHOT" ) ), greaterThan( 0 ) );
        assertThat( artifactVersion.compareTo( new DefaultArtifactVersion( "1.1" ) ), greaterThan( 0 ) );
        assertThat( artifactVersion.compareTo( new DefaultArtifactVersion( "1.1.0-2" ) ), greaterThan( 0 ) );
        assertThat( artifactVersion.compareTo( new DefaultArtifactVersion( "1.1.1-2" ) ), lessThan( 0 ) );
    }
}