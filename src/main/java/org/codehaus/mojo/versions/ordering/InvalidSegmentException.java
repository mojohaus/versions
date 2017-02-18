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

/**
 * Represents an invalid segment being identified within a version.
 */
public class InvalidSegmentException
    extends RuntimeException
{
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new exception.
     *
     * @param segment the invalid segment index.
     * @param segmentCount the number of segments.
     * @param version the version string.
     */
    public InvalidSegmentException( int segment, int segmentCount, String version )
    {
        super( String.format( "Invalid segment, %d, for the %d segment version: '%s'", segment, segmentCount,
                              version ) );
    }
}
