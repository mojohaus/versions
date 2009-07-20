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

public class MavenVersionComparatorTest
    extends TestCase
{
    private MavenVersionComparator instance = new MavenVersionComparator();

    public void testSegmentCounting()
        throws Exception
    {
        assertEquals( 3, instance.getSegmentCount( new DefaultArtifactVersion( "5" ) ) );
        assertEquals( 3, instance.getSegmentCount( new DefaultArtifactVersion( "5.0" ) ) );
        assertEquals( 4, instance.getSegmentCount( new DefaultArtifactVersion( "5-0" ) ) );
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5.3.a" ) ) );
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5.0.a.1.4.5" ) ) );
        assertEquals( 3, instance.getSegmentCount( new DefaultArtifactVersion( "" ) ) );
    }

    public void testSegmentIncrementing()
        throws Exception
    {
        assertIncrement( "6", "5", 0 );
        assertIncrement( "6.0", "5.0", 0 );
        assertIncrement( "5.1", "5.0", 1 );
        assertIncrement( "5.1.0", "5.0.1", 1 );
        assertIncrement( "5.alpha.2", "5.alpha.1", 0 );
        assertIncrement( "5.alpha-1.2", "5.alpha-1.1", 0 );
        assertIncrement( "5.alpha-1.ba", "5.alpha-1.az", 0 );
        assertIncrement( "5.alpha-wins.2", "5.alpha-wins.1", 0 );
        assertIncrement( "1.0-alpha-3-SNAPSHOT", "1.0-alpha-2-SNAPSHOT", 3 );
        assertIncrement( "1.0-alpha-90-SNAPSHOT", "1.0-alpha-9-SNAPSHOT", 3 );
        assertIncrement( "1.0-za-SNAPSHOT", "1.0-z-SNAPSHOT", 3 );
        assertIncrement( "1.0-z90-SNAPSHOT", "1.0-z9-SNAPSHOT", 3 );
    }

    private void assertIncrement( String expected, String initial, int segment )
    {
        assertEquals( expected,
                      instance.incrementSegment( new DefaultArtifactVersion( initial ), segment ).toString() );
    }
}