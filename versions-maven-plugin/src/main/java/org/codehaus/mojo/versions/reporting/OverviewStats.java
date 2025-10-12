package org.codehaus.mojo.versions.reporting;

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

import java.util.Collection;
import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.api.Segment;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Represents summary stats
 *
 * @author Andrzej Jarmoniuk
 */
public class OverviewStats {
    private int major;

    private int minor;

    private int incremental;

    private int any;

    private int upToDate;

    /**
     * Creates a new instance.
     */
    public OverviewStats() {}

    /**
     * Creates a {@linkplain OverviewStats} instance based on the collection of version updates in
     * the argument
     *
     * @param updates collection of all version updates, typically from
     * {@linkplain org.codehaus.mojo.versions.reporting.model.DependencyUpdatesModel#getAllUpdates()}
     * @param cache if not null, cache to retrieve the version information, initialised with
     * the {@link ArtifactVersions#getNewestUpdateWithinSegment(Optional, boolean)} update information
     * @param <T> subclass of {@linkplain OverviewStats}
     * @param <V> subclass of {@linkplain ArtifactVersions}
     * @param allowSnapshots whether snapshots should be included
     * @return instance of the {@linkplain OverviewStats}
     */
    public static <T extends OverviewStats, V extends AbstractVersionDetails> T fromUpdates(
            Collection<V> updates, ArtifactVersionsCache cache, boolean allowSnapshots) {
        OverviewStats stats = new OverviewStats();
        updates.forEach(details -> {
            if (getNewestUpdate(cache, details, of(SUBINCREMENTAL), allowSnapshots) != null) {
                stats.incrementAny();
            } else if (getNewestUpdate(cache, details, of(INCREMENTAL), allowSnapshots) != null) {
                stats.incrementIncremental();
            } else if (getNewestUpdate(cache, details, of(MINOR), allowSnapshots) != null) {
                stats.incrementMinor();
            } else if (getNewestUpdate(cache, details, of(MAJOR), allowSnapshots) != null) {
                stats.incrementMajor();
            } else {
                stats.incrementUpToDate();
            }
        });
        return (T) stats;
    }

    /**
     * Retrieves the newest update for the given details, segment and snapshot allowance from the cache.
     * If the cache is null, an assertion error is thrown.
     *
     * @param cache if not null, cache to retrieve the version information, initialised with
     * the {@link ArtifactVersions#getNewestUpdateWithinSegment(Optional, boolean)} update information
     * @param details details of the artifact to check
     * @param segment segment to check
     * @param allowSnapshots whether snapshots should be included
     * @param <V> subclass of {@linkplain AbstractVersionDetails}
     * @return the newest update or null if there is no update within the segment
     */
    protected static <V extends AbstractVersionDetails> ArtifactVersion getNewestUpdate(
            ArtifactVersionsCache cache, V details, Optional<Segment> segment, boolean allowSnapshots) {
        assert cache != null;
        return cache.get(details, segment, allowSnapshots);
    }

    /**
     * Gets the number of updates available within the major segment
     * @return number of updates available within the major segment
     */
    public int getMajor() {
        return major;
    }

    /**
     * Increments the number of updates available within the major segment
     */
    public void incrementMajor() {
        major++;
    }

    /**
     * Gets the number of updates available within the minor segment
     * @return number of updates available within the minor segment
     */
    public int getMinor() {
        return minor;
    }

    /**
     * Increments the number of updates available within the minor segment
     */
    public void incrementMinor() {
        minor++;
    }

    /**
     * Gets the number of updates available within the incremental segment
     * @return number of updates available within the incremental segment
     */
    public int getIncremental() {
        return incremental;
    }

    /**
     * Increments the number of updates available within the incremental segment
     */
    public void incrementIncremental() {
        incremental++;
    }

    /**
     * Gets the number of updates available within the sub-incremental segment
     * @return number of updates available within the sub-incremental segment
     */
    public int getAny() {
        return any;
    }

    /**
     * Increments the number of updates available within the sub-incremental segment
     */
    public void incrementAny() {
        any++;
    }

    /**
     * Gets the number of up-to-date artifacts
     * @return number of up-to-date artifacts
     */
    public int getUpToDate() {
        return upToDate;
    }

    /**
     * Increments the number of up-to-date artifacts
     */
    public void incrementUpToDate() {
        upToDate++;
    }
}
