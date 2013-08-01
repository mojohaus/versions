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

import java.util.Comparator;

/**
 * A rule for comparing and manipulating versions.
 */
public interface VersionComparator
    extends Comparator<ArtifactVersion>
{
    /**
     * Returns the number of segments specified or specifiable in the supplied artifact version.
     *
     * @param artifactVersion The artifact version to count the segments of.
     * @return The number of segments.
     * @since 1.0-beta-1
     */
    int getSegmentCount( ArtifactVersion artifactVersion );

    /**
     * Increment the specified segment of the supplied version.
     *
     * @param artifactVersion The artifact version to increment.
     * @param segment         The segment number to increment.
     * @return An artifact version with the specified segment incremented.
     * @since 1.0-beta-1
     */
    ArtifactVersion incrementSegment( ArtifactVersion artifactVersion, int segment );
}
