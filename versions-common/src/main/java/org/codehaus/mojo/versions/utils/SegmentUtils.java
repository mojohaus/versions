package org.codehaus.mojo.versions.utils;

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

import java.util.Optional;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.Segment;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;

/**
 * Utility class for manipulating with {@link Segment} objects
 */
public class SegmentUtils {
    /**
     * <p>Based on the passed flags, determines which segment which is not to be changed.</p>
     *
     * <p>Also, logs the enriched values of the {@code allowMajorUpdates}, {@code allowMinorUpdates},
     * and {@code allowIncrementalUpdates} options so that {@code allowMinorUpdates} equal to {@code false}
     * implies that {@code allowMajorUpdates} is also {@code false}.</p>
     * <p>Also, {@code allowIncrementalUpdates} equal to {@code false}
     * implies that both {@code allowMajorUpdates} and {@code allowMinorUpdates} are also {@code false}.</p>
     *
     * <table>
     * <caption>Effective values for update options</caption>
     * <thead>
     * <tr><th>allowMajorUpdates</th><th>allowMinorUpdates</th><th>allowIncrementalUpdates</th></tr>
     * </thead>
     * <tbody>
     * <tr><td>true</td><td>true</td><td>true</td></tr>
     * <tr><td></td><td>true</td><td>true</td></tr>
     * <tr><td></td><td></td><td>true</td></tr>
     * <tr><td></td><td></td><td></td></tr>
     * </tbody>
     * </table>
     *
     * @param allowMajorUpdates whether all updates should be allowed
     * @param allowMinorUpdates if major updates are disallowed, minor, incremental updates should be allowed
     * @param allowIncrementalUpdates if major and minor updates are disallowed, incremental updates are allowed
     * @param log                     If not null, the {@linkplain Log} object to log the selected scope
     * @return Returns the segment (0-based) that is unchangeable. If any segment can change, returns -1.
     */
    public static Optional<Segment> determineUnchangedSegment(
            boolean allowMajorUpdates, boolean allowMinorUpdates, boolean allowIncrementalUpdates, Log log) {
        if (log != null && !allowIncrementalUpdates) {
            log.info("Assuming allowMinorUpdates false because allowIncrementalUpdates is false.");
        }

        if (log != null && !allowMinorUpdates) {
            log.info("Assuming allowMajorUpdates false because allowMinorUpdates is false.");
        }

        Optional<Segment> unchangedSegment = allowMajorUpdates && allowMinorUpdates && allowIncrementalUpdates
                ? empty()
                : allowMinorUpdates && allowIncrementalUpdates
                        ? of(MAJOR)
                        : allowIncrementalUpdates ? of(MINOR) : of(INCREMENTAL);
        if (log != null && log.isDebugEnabled()) {
            log.debug(unchangedSegment
                            .map(s -> Segment.of(s.value() + 1).toString())
                            .orElse("ALL") + " version changes allowed");
        }
        return unchangedSegment;
    }
}
