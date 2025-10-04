package org.codehaus.mojo.versions.api.recording;

import java.io.IOException;
import java.nio.file.Path;

import org.codehaus.mojo.versions.model.ChangeRecorderLog;

public interface ChangeRecorderRenderer {
    ChangeRecorderLog read(Path path) throws IOException;

    void write(Path path, ChangeRecorderLog log) throws IOException;
}
