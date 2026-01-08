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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.maven.plugin.logging.SystemStreamLog;

/**
 * A test utility provided a closeable temporary file, implementing {@link AutoCloseable}, which
 * deletes the temporary file on {@link #close()}, typically upon completion of a {@code try-finally} block.
 */
public class CloseableTempFile implements AutoCloseable {
    private final Path path;

    /**
     * Returns the path of the temporary file.
     * @return path of the temporary file.
     */
    public Path getPath() {
        return path;
    }

    /**
     * Creates a new temporary file with the given prefix
     * @param prefix prefix of the temporary file
     * @throws IOException thrown if file creation fails
     */
    public CloseableTempFile(String prefix) throws IOException {
        path = Files.createTempFile(prefix, ".tmp");
    }

    @Override
    public void close() throws Exception {
        try {
            Files.deleteIfExists(path);
        } catch (IOException e) {
            new SystemStreamLog().error("Error deleting temporary directory " + path, e);
        }
    }
}
