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

public class NumericVersionComparatorTest
    extends TestCase
{
    private NumericVersionComparator instance = new NumericVersionComparator();

    public void testSmokes()
        throws Exception
    {
        assertTrue( instance.compare( "1.0.0.0.0", "1.0.0.0.1" ) < 0 );
        assertTrue( instance.compare( "1.0.0.0.0", "2.0.0.0.1" ) < 0 );
        assertTrue( instance.compare( "1.0.0.0.0", "1.0.0.0" ) < 0 );
        assertTrue( instance.compare( "1.0.0.0.0", "1.0.0.0.0" ) == 0 );
        assertTrue( instance.compare( "1.0.0.0", "1.0.0.0.0" ) > 0 );
    }

    public void testBigValues()
        throws Exception
    {
        assertTrue( instance.compare( "1.92.0", "1.100000000000000000000000.0" ) < 0 );
        assertTrue( instance.compare( "1.100000000000000000000000.0", "1.92.0" ) > 0 );
        assertTrue( instance.compare( "1.100000000000000000000000.0", "1.100000000000000000000000.0" ) == 0 );
    }

    public void testStringValues()
        throws Exception
    {
        assertTrue( instance.compare( "1.a20.0", "1.a3.0" ) < 0 );
        assertTrue( instance.compare( "1.a20.0", "1.b10.0" ) < 0 );
        assertTrue( instance.compare( "1.a.0.b.0", "1.a.0.b.1" ) < 0 );
        assertTrue( instance.compare( "1.162%a.0.b.0", "1.162%a.0.b.1" ) < 0 );
        assertTrue( instance.compare( "1.162%a.0.b.0", "1.2.0.b.1" ) < 0 );
        assertTrue( instance.compare( "1.0a.0.b.0", "1.162%.0.b.1" ) < 0 );
        assertTrue( instance.compare( "1.a.0.b.0", "2.a.0.b.1" ) < 0 );
        assertTrue( instance.compare( "1.a.0.b.0", "1.a.0.b" ) < 0 );
        assertTrue( instance.compare( "1.a.0.b.0", "1.a.0.b.0" ) == 0 );
        assertTrue( instance.compare( "1.a.0.b", "1.a.0.b.0" ) > 0 );
        assertTrue( instance.compare( "1.a.0.0", "1.a.0" ) < 0 );
        assertTrue( instance.compare( "1.a.0", "1.a.0.0" ) > 0 );
        assertTrue( instance.compare( "1.a.0.1", "1.a.0" ) > 0 );
        assertTrue( instance.compare( "1.a.0", "1.a.0.1" ) < 0 );
        assertTrue( instance.compare( "1.a.0.b", "1.a.0" ) > 0 );
        assertTrue( instance.compare( "1.a.0", "1.a.0.b" ) < 0 );
    }

    public void testQualifiers()
        throws Exception
    {
        assertTrue( instance.compare( "1.0-alpha.10", "1.0-alpha.20" ) < 0 );
        assertTrue( instance.compare( "1.0-alpha.10", "1.0-beta.1" ) < 0 );
        assertTrue( instance.compare( "1.0", "1.0-alpha.2" ) > 0 );
        assertTrue( instance.compare( "1.0-alpha.10", "1.0" ) < 0 );
        assertTrue( instance.compare( "1.0.10", "1.0-alpha.10" ) > 0 );
    }

    public void testSegmentCounting()
        throws Exception
    {
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5" ) ) );
        assertEquals( 2, instance.getSegmentCount( new DefaultArtifactVersion( "5.0" ) ) );
        assertEquals( 1, instance.getSegmentCount( new DefaultArtifactVersion( "5-0" ) ) );
        assertEquals( 3, instance.getSegmentCount( new DefaultArtifactVersion( "5.3.a" ) ) );
        assertEquals( 6, instance.getSegmentCount( new DefaultArtifactVersion( "5.0.a.1.4.5" ) ) );
        assertEquals( 0, instance.getSegmentCount( new DefaultArtifactVersion( "" ) ) );
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
        assertEquals( new DefaultArtifactVersion( "5.beta.0" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha.1" ), 1 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-2.0" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.1" ), 1 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.alpha-1.2" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-1.1" ), 2 ).toString() );
        assertEquals( new DefaultArtifactVersion( "5.beta.0" ).toString(),
                      instance.incrementSegment( new DefaultArtifactVersion( "5.alpha-wins.1" ), 1 ).toString() );
    }
}
