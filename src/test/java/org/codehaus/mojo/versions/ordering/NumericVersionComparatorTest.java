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

public class NumericVersionComparatorTest
    extends TestCase
{
    private NumericVersionComparator instance = new NumericVersionComparator();
    
    
    
    public void testSmokes() throws Exception {
        assertTrue(instance.compare( "1.0.0.0.0", "1.0.0.0.1" ) < 0);
        assertTrue(instance.compare( "1.0.0.0.0", "2.0.0.0.1" ) < 0);
        assertTrue(instance.compare( "1.0.0.0.0", "1.0.0.0" ) < 0);
        assertTrue(instance.compare( "1.0.0.0.0", "1.0.0.0.0" ) == 0);
        assertTrue(instance.compare( "1.0.0.0", "1.0.0.0.0" ) > 0);
    }
    
    public void testBigValues() throws Exception {
        assertTrue(instance.compare( "1.92.0", "1.100000000000000000000000.0" ) < 0);
    }

    public void testStringValues() throws Exception {
        assertTrue(instance.compare( "1.a20.0", "1.a3.0" ) < 0);
        assertTrue(instance.compare( "1.a20.0", "1.b10.0" ) < 0);
        assertTrue(instance.compare( "1.a.0.b.0", "1.a.0.b.1" ) < 0);
        assertTrue(instance.compare( "1.a.0.b.0", "2.a.0.b.1" ) < 0);
        assertTrue(instance.compare( "1.a.0.b.0", "1.a.0.b" ) < 0);
        assertTrue(instance.compare( "1.a.0.b.0", "1.a.0.b.0" ) == 0);
        assertTrue(instance.compare( "1.a.0.b", "1.a.0.b.0" ) > 0);
    }
}
