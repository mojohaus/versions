package org.codehaus.mojo.versions.api;

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
 *         Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/**
 * Indicates the segment along with its 0-based index
 *
 * @author Andrzej Jarmoniuk
 */
public enum Segment implements Comparable<Segment>
{
    MAJOR( 0 ), MINOR( 1 ), INCREMENTAL( 2 ), SUBINCREMENTAL( 3 );

    private final int index;

    Segment( int index )
    {
        this.index = index;
    }

    /**
     * Returns the 0-based sendex index
     *
     * @return 0-based sendex index
     */
    public int value()
    {
        return index;
    }

    public static Segment of( int index )
    {
        switch ( index )
        {
            case 0: return MAJOR;
            case 1: return MINOR;
            case 2: return INCREMENTAL;
            case 3: return SUBINCREMENTAL;
            default:
                throw new IllegalArgumentException( "Wrong segment index: " + index );
        }
    }

    @Override
    public String toString()
    {
        return name();
    }
}
