package org.codehaus.mojo.versions.recording;

import javax.inject.Named;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.VersionChange;

@Named("none")
public class NoneVersionChangeRecorderFactory implements VersionChangeRecorderFactory {
    @Override
    public VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> changeRendererOptions) {
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
