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

import java.util.Comparator;
import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.codehaus.mojo.versions.api.Segment;

import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

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
     * <p>Returns a {@linkplain Restriction} object for computing version <em>upgrades</em>
     * with the given segment allowing updates, with all more major segments locked in place.</p>
     * <p>The resulting restriction could be thought of as one
     * retaining the versions on positions up to the held position,
     * the position right after the position held in place will be incremented by one,
     * and on all positions which are more minor than that, the range would contain -&infin;
     * for the bottom bound and +&infin; for the above bound.</p>
     * <p>This will allow matching the required versions while not matching versions which are considered
     * inferior than the zeroth version, i.e. versions with a qualifier.</p>
     *
     * @param currentVersion The current version.
     * @param scope most major segment where updates are allowed Optional.empty() for no restriction
     * @return {@linkplain Restriction} object based on the arguments
     */
    default Restriction restrictionFor( ArtifactVersion currentVersion, Optional<Segment> scope )
            throws InvalidSegmentException
    {
        ArtifactVersion nextVersion = scope.filter( s -> s.isMajorTo( SUBINCREMENTAL ) )
                .map( s -> (ArtifactVersion)
                        new BoundArtifactVersion( currentVersion, Segment.of( s.value() + 1 ) ) )
                .orElse( currentVersion );
        return new Restriction( nextVersion, false, scope.filter( MAJOR::isMajorTo )
                .map( s -> (ArtifactVersion) new BoundArtifactVersion( currentVersion, s ) ).orElse( null ),
                false );
    }
}
