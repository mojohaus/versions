package org.codehaus.mojo.versions.recording;

import javax.inject.Named;

import java.util.Map;
import java.util.Optional;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.recording.xml.LegacyXmlVersionChangeRenderer;
import org.codehaus.mojo.versions.recording.xml.XmlVersionChangeRenderer;

/**
 * A {@link VersionChangeRecorderFactory} for rendering the updates into the format compatible
 * with the legacy {@code http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0} schema
 */
@Named("xml")
public class XmlVersionChangeRecorderFactory implements VersionChangeRecorderFactory {
    @Override
    public VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> options) {
        ObjectFactory objectFactory = new ObjectFactory();
        ChangeRecorderRenderer renderer;
        if (options == null
                || "true"
                        .equalsIgnoreCase(
                                Optional.ofNullable(options.get("legacy")).orElse("true"))) {
            renderer = new LegacyXmlVersionChangeRenderer(objectFactory);
        } else {
            renderer = new XmlVersionChangeRenderer(objectFactory);
        }
        return new GenericVersionChangeRecorder(
                "versions-changes.xml", objectFactory, mavenSession, mojoExecution, renderer);
    }
}
