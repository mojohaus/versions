package org.codehaus.mojo.versions.recording;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class VersionChangeFileHelper {
    public static void createFile(Path path) throws IOException {
        Path parent = path.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        if (Files.notExists(path)) {
            Files.createFile(path);
        }
    }
}
