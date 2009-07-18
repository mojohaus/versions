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
        assertEquals( 4, instance.getSegmentCount( new DefaultArtifactVersion( "5" ) ) );
        assertEquals( 4, instance.getSegmentCount( new DefaultArtifactVersion( "5.0" ) ) );
        assertEquals( 4, instance.getSegmentCount( new DefaultArtifactVersion( "5-0" ) ) );
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5.3.a" ) ) );
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5.0.a.1.4.5" ) ) );
        assertEquals( 4, instance.getSegmentCount( new DefaultArtifactVersion( "" ) ) );
    }

    public void testSegmentIncrementing()
        throws Exception
    {
        assertEquals( new DefaultArtifactVersion( "6" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "6.0" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.0" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.1" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.0" ), 1 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.1.0" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.0.1" ), 1 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha.2" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha.1" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-1.2" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.1" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-1.ba" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.az" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-wins.2" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-wins.1" ), 0 ).toString() );
        assertEquals( new DefaultArtifactVersion( "1.0-alpha-3-SNAPSHOT" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "1.0-alpha-2-SNAPSHOT" ), 3 ).toString() );
        assertEquals( new DefaultArtifactVersion( "1.0-alpha-90-SNAPSHOT" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "1.0-alpha-9-SNAPSHOT" ), 3 ).toString() );
        assertEquals( new DefaultArtifactVersion( "1.0-za-SNAPSHOT" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "1.0-z-SNAPSHOT" ), 3 ).toString() );
        assertEquals( new DefaultArtifactVersion( "1.0-z90-SNAPSHOT" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "1.0-z9-SNAPSHOT" ), 3 ).toString() );
    }
}