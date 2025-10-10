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
 * Exception thrown when an invalid {@link Segment} is referenced for a particular {@link ArtifactVersion}.
 *
 * <p>The constructor requires a non-{@code null} {@link Segment} and {@link ArtifactVersion} and a non-negative
 * segment count.</p>
 */
public class InvalidSegmentException extends Exception {
    /** The invalid segment identified. */
    private final Segment segment;

    /** The number of segments in the version under consideration. */
    private final int segmentCount;

    /** The ArtifactVersion being processed. */
    private final ArtifactVersion version;

    /**
     * Constructs a new {@code InvalidSegmentException}.
     *
     * @param segment the invalid {@link Segment}; must not be {@code null}
     * @param segmentCount the number of segments in the version; must be non-negative
     * @param version the {@link ArtifactVersion} instance; must not be {@code null}
     */
    public InvalidSegmentException(Segment segment, int segmentCount, ArtifactVersion version) {
        super(String.format(
                "Invalid segment %s for the %d segment version: '%s'",
                segment.toString(), segmentCount, version.toString()));
        this.segment = segment;
        this.segmentCount = segmentCount;
        this.version = version;
    }

    /**
     * Returns the invalid {@link Segment}.
     *
     * @return the invalid {@link Segment}; never {@code null} for a correctly constructed exception
     */
    public Segment getSegment() {
        return segment;
    }

    /**
     * Returns the number of segments in the version.
     *
     * @return the segment count (non-negative)
     */
    public int getSegmentCount() {
        return segmentCount;
    }

    /**
     * Returns the {@link ArtifactVersion} associated with the exception.
     *
     * @return the {@link ArtifactVersion}; never {@code null} for a correctly constructed exception
     */
    public ArtifactVersion getVersion() {
        return version;
    }
}
