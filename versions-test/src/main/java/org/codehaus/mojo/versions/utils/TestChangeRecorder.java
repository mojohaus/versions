package org.codehaus.mojo.versions.utils;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.inject.Named;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.codehaus.mojo.versions.api.change.VersionChange;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.api.recording.PropertyChangeRecord;

/**
 * A simple {@link ChangeRecorder} implementation used in tests.
 * <p>
 * It records {@link VersionChange} instances reported via dependency or
 * property change records into an in-memory list so tests can assert on them.
 */
@Named("test")
public class TestChangeRecorder implements ChangeRecorder {
    /**
     * Create a new test change recorder.
     */
    public TestChangeRecorder() {
        // explicit constructor to provide Javadoc for the no-arg constructor
    }

    private final List<VersionChange> changes = new LinkedList<>();

    /**
     * Record a dependency version change.
     *
     * @param changeRecord the dependency change record containing the version change
     */
    @Override
    public void recordChange(DependencyChangeRecord changeRecord) {
        changes.add(changeRecord.getVersionChange());
    }

    /**
     * Record a property version change.
     *
     * @param changeRecord the property change record containing the version change
     */
    @Override
    public void recordChange(PropertyChangeRecord changeRecord) {
        changes.add(changeRecord.getVersionChange());
    }

    /**
     * Write a report of recorded changes to the given output path.
     * <p>
     * This test implementation does nothing.
     *
     * @param outputPath the path where a report would be written
     */
    @Override
    public void writeReport(Path outputPath) {}

    /**
     * Returns the list of recorded version changes in the order they were recorded.
     *
     * @return an unmodifiable view of the recorded version changes
     */
    public List<VersionChange> getChanges() {
        return changes;
    }

    /**
     * Return a simple map suitable for injection in tests. The map contains a single
     * entry with key {@code "none"} mapping to this recorder instance.
     *
     * @return a map with a single test change recorder entry
     */
    public Map<String, ChangeRecorder> asTestMap() {
        HashMap<String, ChangeRecorder> map = new HashMap<>();
        map.put("none", this);
        return map;
    }
}
