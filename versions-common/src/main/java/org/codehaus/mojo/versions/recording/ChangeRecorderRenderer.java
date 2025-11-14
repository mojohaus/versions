package org.codehaus.mojo.versions.recording;

import java.io.IOException;
import java.nio.file.Path;

import org.codehaus.mojo.versions.model.ChangeRecorderLog;

/**
 * An optional API for use with the {@link GenericVersionChangeRecorder}
 *
 * @since 2.20.0
 */
public interface ChangeRecorderRenderer {
    /**
     * Reads the change log from the given path.
     *
     * @param path the path to read from
     * @return the read change log
     * @throws IOException if an I/O error occurs
     */
    ChangeRecorderLog read(Path path) throws IOException;

    /**
     * Writes the change log to the given path.
     *
     * @param path the path to write to
     * @param log the change log to write
     * @throws IOException if an I/O error occurs
     */
    void write(Path path, ChangeRecorderLog log) throws IOException;
}
