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
import org.codehaus.mojo.versions.api.Segment;

/**
 * Represents an invalid segment being identified within a version.
 */
public class InvalidSegmentException extends Exception
{
    private final Segment segment;

    private final int segmentCount;

    private final ArtifactVersion version;

    /**
     * Constructs the exception object
     *
     * @param segment the invalid segment index.
     * @param segmentCount the number of segments.
     * @param version the version object.
     */
    public InvalidSegmentException( Segment segment, int segmentCount, ArtifactVersion version )
    {
        super( String.format( "Invalid segment %s for the %d segment version: '%s'", segment.toString(), segmentCount,
                version.toString() ) );
        this.segment = segment;
        this.segmentCount = segmentCount;
        this.version = version;
    }

    /**
     * @return segment
     */
    public Segment getSegment()
    {
        return segment;
    }

    /**
     * @return segment count
     */
    public int getSegmentCount()
    {
        return segmentCount;
    }

    /**
     * @return version object
     */
    public ArtifactVersion getVersion()
    {
        return version;
    }
}
