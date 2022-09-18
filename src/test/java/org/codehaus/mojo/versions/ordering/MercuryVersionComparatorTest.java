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

import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.junit.Assert.assertEquals;

public class MercuryVersionComparatorTest
{
    private MercuryVersionComparator instance = new MercuryVersionComparator();

    @Test
    public void testSegmentCounting()
            throws Exception
    {
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5" ) ) );
        assertEquals( 2, instance.getSegmentCount( new DefaultArtifactVersion( "5.0" ) ) );
        assertEquals( 2, instance.getSegmentCount( new DefaultArtifactVersion( "5-0" ) ) );
        assertEquals( 3, instance.getSegmentCount( new DefaultArtifactVersion( "5.3.a" ) ) );
        assertEquals( 6, instance.getSegmentCount( new DefaultArtifactVersion( "5.0.a.1.4.5" ) ) );
        assertEquals( 0, instance.getSegmentCount( new DefaultArtifactVersion( "" ) ) );
    }

    @Test
    public void testSegmentIncrementing()
            throws Exception
    {
        assertEquals( new DefaultArtifactVersion( "6" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5" ), MAJOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "6.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.0" ), MAJOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.1" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.0" ), MINOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.1.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.0.1" ), MINOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.beta.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.alpha.1" ), MINOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.beta-0.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.1" ), MINOR ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-2.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.1" ), INCREMENTAL ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.beta-0.0" ).toString(),
                instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-wins.1" ), MINOR ).toString() );
    }
}