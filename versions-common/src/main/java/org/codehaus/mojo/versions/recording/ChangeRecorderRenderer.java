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
    ChangeRecorderLog read(Path path) throws IOException;

    void write(Path path, ChangeRecorderLog log) throws IOException;
}
