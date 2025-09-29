package org.codehaus.mojo.versions.api.recording;

import java.util.Map;
import java.util.Properties;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;

/**
 * A factory for version change recorders.
 */
public interface VersionChangeRecorderFactory {

    /**
     * Creates a new {@link VersionChangeRecorder} instance.
     *
     * @param mavenSession  {@link MavenSession} instance
     * @param mojoExecution {@link MojoExecution} instance
     * @return a new {@link VersionChangeRecorder} instance
     */
    default VersionChangeRecorder create(MavenSession mavenSession, MojoExecution mojoExecution) {
        return create(mavenSession, mojoExecution, null);
    }

    /**
     * Creates a new {@link VersionChangeRecorder} instance.
     *
     * @param mavenSession          {@link MavenSession} instance
     * @param mojoExecution         {@link MojoExecution} instance
     * @param changeRendererOptions {@link Properties} custom properties to pass to the renderer
     * @return a new {@link VersionChangeRecorder} instance
     */
    VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> changeRendererOptions);
}
