package org.codehaus.mojo.versions.recording;

import javax.inject.Named;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.VersionChange;

/**
 * A {@link VersionChangeRecorderFactory} that creates a no-op {@link VersionChangeRecorder}.
 * This implementation does not record any changes or write any report.
 * It can be used when version change recording is not desired.
 */
@Named("none")
public class NoneVersionChangeRecorderFactory implements VersionChangeRecorderFactory {
    /**
     * Creates a new instance
     */
    public NoneVersionChangeRecorderFactory() {}

    @Override
    public VersionChangeRecorder create(
            MavenSession mavenSession,
            MojoExecution mojoExecution,
            Log log,
            Map<String, String> changeRendererOptions) {
        return new VersionChangeRecorder() {
            @Override
            public void recordChange(VersionChange versionChange) {}

            @Override
            public void writeReport(Path outputPath) throws IOException {}

            @Override
            public String getDefaultFileName() {
                return "";
            }
        };
    }
}
