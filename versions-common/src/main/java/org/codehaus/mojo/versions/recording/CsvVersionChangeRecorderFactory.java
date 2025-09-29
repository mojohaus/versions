package org.codehaus.mojo.versions.recording;

import javax.inject.Named;

import java.util.Map;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.recording.csv.CsvVersionChangeRenderer;

/**
 * A factory for rendering version changes into a CSV file
 */
@Named("csv")
public class CsvVersionChangeRecorderFactory implements VersionChangeRecorderFactory {
    @Override
    public synchronized VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> changeRendererOptions) {
        ObjectFactory objectFactory = new ObjectFactory();
        return new GenericVersionChangeRecorder(
                "versions-changes.csv",
                objectFactory,
                mavenSession,
                mojoExecution,
                new CsvVersionChangeRenderer(objectFactory));
    }
}
