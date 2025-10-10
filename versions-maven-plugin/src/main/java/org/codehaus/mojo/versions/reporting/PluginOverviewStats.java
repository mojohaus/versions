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

import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.ArtifactVersionsCache;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;

import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Extension of the {@linkplain OverviewStats} adding dependency stats
 */
public class PluginOverviewStats extends OverviewStats {
    /**
     * Extension of the {@linkplain OverviewStats} adding dependency stats
     */
    private int dependencies;

    /**
     * Creates a new instance.
     */
    public PluginOverviewStats() {}

    /**
     * Gets the number of plugins with dependency updates available
     * @return number of plugins with dependency updates available
     */
    public int getDependencies() {
        return dependencies;
    }

    /**
     * Increments the number of plugins with dependency updates available
     */
    public void incrementDependencies() {
        dependencies++;
    }

    /**
     * Creates a {@linkplain PluginOverviewStats} instance based on the collection of version updates in
     * the argument
     *
     * @param updates collection of all version updates, typically from {@linkplain PluginUpdatesModel#getAllUpdates()}
     * @param cache if not null, cache to retrieve the version information, initialised with
     * the {@link ArtifactVersions#getNewestUpdateWithinSegment(Optional, boolean)} update information
     * @param <T> always equal to {@linkplain PluginOverviewStats}
     * @param <V> always equal to {@linkplain PluginUpdatesDetails}
     * @param allowSnapshots whether snapshots should be included
     * @return instance of the {@linkplain PluginOverviewStats}, initialised with the update information
     */
    public static <T extends OverviewStats, V extends AbstractVersionDetails> T fromUpdates(
            Collection<V> updates, ArtifactVersionsCache cache, boolean allowSnapshots) {
        PluginOverviewStats stats = new PluginOverviewStats();
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
            if (((PluginUpdatesDetails) details).isDependencyUpdateAvailable()) {
                stats.incrementDependencies();
            }
        });
        return (T) stats;
    }
}
