package org.codehaus.mojo.versions.recording;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Helper class for file operations related to version change recording.
 *
 * @since 2.20.0
 */
public class VersionChangeFileHelper {
    private VersionChangeFileHelper() {
        // utility class
    }

    /**
     * Creates a file at the specified path, including any necessary but nonexistent parent directories.
     * If the file already exists, no action is taken.
     *
     * @param path the path of the file to create
     * @throws IOException if an I/O error occurs
     */
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
