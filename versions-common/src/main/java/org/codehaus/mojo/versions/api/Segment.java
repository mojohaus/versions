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

import java.util.Optional;

/**
 * Indicates the segment along with its 0-based index
 *
 * @author Andrzej Jarmoniuk
 */
public enum Segment implements Comparable<Segment> {
    MAJOR,
    MINOR,
    INCREMENTAL,
    SUBINCREMENTAL;

    /**
     * Returns the 0-based sendex index
     *
     * @return 0-based sendex index
     */
    public int value() {
        return ordinal();
    }

    public static Segment of(int index) {
        if (index < 0 || index > 3) {
            throw new IllegalArgumentException("Wrong segment index: " + index);
        }
        return values()[index];
    }

    /**
     * Creates a segment that has a greater scope than the given segment or {@code null}
     * if the segment is already {@link #MAJOR}
     * @param other segment that the new segment is to be based on
     * @return that has a greater scope than the given segment or {@code null}
     * if the segment is already {@link #MAJOR}
     */
    public static Segment majorTo(Segment other) {
        return Optional.ofNullable(other).map(s -> of(s.value() - 1)).orElse(null);
    }

    /**
     * Creates a segment that has a lesser scope than the given segment
     * @param other segment that the new segment is to be based on
     * @return that has a lesser scope than the given segment
     */
    public static Segment minorTo(Segment other) {
        return Optional.ofNullable(other).map(s -> of(s.value() + 1)).orElse(MAJOR);
    }

    /**
     * Returns true if the given segment is more major than the other
     * @param other other segment to compare
     * @return true if the given segment is more major
     */
    public boolean isMajorTo(Segment other) {
        return value() < other.value();
    }

    @Override
    public String toString() {
        return name();
    }
}
