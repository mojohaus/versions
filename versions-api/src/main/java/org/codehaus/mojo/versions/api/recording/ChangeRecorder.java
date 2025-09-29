package org.codehaus.mojo.versions.api.recording;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import java.io.IOException;
import java.nio.file.Path;

/**
 * Interface for implement a recorder of version changes.
 *
 * @author Slawomir Jaranowski
 * @since 2.14.0
 * @deprecated use {@link VersionChangeRecorder} and {@code versions-model}
 */
@Deprecated
public interface ChangeRecorder {
    /**
     * Record that a dependency was updated.
     *
     * @param changeRecord a dependency record described change
     * @since 2.14.0
     */
    void recordChange(DependencyChangeRecord changeRecord);

    /**
     * Record that a property was updated.
     *
     * @param changeRecord a property record described change
     * @since 2.14.0
     */
    void recordChange(PropertyChangeRecord changeRecord);

    /**
     * Write the current set of changes to the given output path.
     * <p>
     * Implementation is responsible for creating all missing directories.
     * <p>
     * Output should not be created for empty record sets.
     *
     * @param outputPath The output path, can be null, provided by <code>changeRecorderOutputFile</code>
     *                   plugin parameters
     * @throws IOException On write and/or I/O errors
     * @since 2.14.0
     */
    void writeReport(Path outputPath) throws IOException;
}
