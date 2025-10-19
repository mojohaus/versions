package org.codehaus.mojo.versions.recording;

import javax.inject.Named;

import java.util.Map;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.recording.json.JsonVersionChangeRenderer;

/**
 * A {@link VersionChangeRecorderFactory} for rendering version changes into a JSON file
 */
@Named("json")
public class JsonVersionChangeRecorderFactory implements VersionChangeRecorderFactory {
    /**
     * Create a new instance
     */
    public JsonVersionChangeRecorderFactory() {}

    @Override
    public synchronized VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> changeRendererOptions) {
        return new GenericVersionChangeRecorder(
                "versions-changes.json",
                new ObjectFactory(),
                mavenSession,
                mojoExecution,
                new JsonVersionChangeRenderer());
    }
}
