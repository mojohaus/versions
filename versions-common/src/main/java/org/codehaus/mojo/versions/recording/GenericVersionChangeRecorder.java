package org.codehaus.mojo.versions.recording;

import java.io.IOException;
import java.nio.file.Path;
import java.time.ZonedDateTime;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.ChangeRecorderRenderer;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.model.VersionChange;
import org.codehaus.mojo.versions.model.VersionsExecution;

/**
 * Generic implementation of the {@link VersionChangeRecorder}, using a {@link ChangeRecorderRenderer}
 * to render {@link ChangeRecorderLog} objects.
 */
public class GenericVersionChangeRecorder implements VersionChangeRecorder {
    private final VersionsExecution versionsExecution;

    private final ObjectFactory objectFactory = new ObjectFactory();

    private final ChangeRecorderRenderer renderer;

    private final String defaultFileName;

    /**
     * Creates a new instance of the recorder
     *
     * @param defaultFileName default file name for the change recorder log, used if it's not provided by the user
     * @param objectFactory   instance of {@link ObjectFactory}, used by {@link ChangeRecorderRenderer} instances
     *                        during deserialization to create model objects
     * @param session {@link MavenSession} instance
     * @param mojoExecution {@link MojoExecution} instance
     * @param renderer {@link ChangeRecorderRenderer} instance used to delegate the actual rendering
     */
    public GenericVersionChangeRecorder(
            String defaultFileName,
            ObjectFactory objectFactory,
            MavenSession session,
            MojoExecution mojoExecution,
            ChangeRecorderRenderer renderer) {
        this.renderer = renderer;
        versionsExecution = objectFactory
                .createVersionsExecution()
                .withGoal(mojoExecution != null ? mojoExecution.getGoal() : null)
                .withDate(ZonedDateTime.now());
        this.defaultFileName = defaultFileName;
    }

    @Override
    public void recordChange(VersionChange versionChange) {
        versionsExecution.getVersionChanges().add(versionChange);
    }

    @Override
    public void writeReport(Path outputPath) throws IOException {
        if (outputPath == null) {
            throw new IllegalArgumentException("changeRecorderOutputFile not provided");
        }
        if (versionsExecution.getVersionChanges().isEmpty()) {
            // don't generate an empty file or add an entry with no changes
            return;
        }
        ChangeRecorderLog changeRecorderLog = renderer.read(outputPath);
        if (changeRecorderLog == null) {
            VersionChangeFileHelper.createFile(outputPath);
            changeRecorderLog = objectFactory.createChangeRecorderLog();
        }
        changeRecorderLog.getUpdates().add(versionsExecution);
        renderer.write(outputPath, changeRecorderLog);
    }

    @Override
    public String getDefaultFileName() {
        return defaultFileName;
    }
}
