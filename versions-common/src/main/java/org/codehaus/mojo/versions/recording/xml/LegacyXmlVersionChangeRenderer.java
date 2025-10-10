package org.codehaus.mojo.versions.recording.xml;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.model.VersionsExecution;
import org.codehaus.mojo.versions.recording.ChangeRecorderRenderer;

/**
 * {@link ChangeRecorderRenderer} which renders changes in the legacy
 * {@code http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0} format.
 */
public final class LegacyXmlVersionChangeRenderer implements ChangeRecorderRenderer {

    private final JAXBContext jaxbContext;

    private final ObjectFactory objectFactory;

    /**
     * Creates a new instance of the renderer
     * @param objectFactory {@link ObjectFactory} instance
     */
    public LegacyXmlVersionChangeRenderer(ObjectFactory objectFactory) {
        this.objectFactory = objectFactory;
        try {
            this.jaxbContext = JAXBContext.newInstance(VersionsExecution.class);
        } catch (JAXBException e) {
            throw new IllegalStateException("Failed to initialize JAXBContext for VersionsExecution", e);
        }
    }

    @Override
    public ChangeRecorderLog read(Path path) throws IOException {
        // Legacy format is not appendable
        return null;
    }

    @Override
    public void write(Path path, ChangeRecorderLog changeRecorderLog) throws IOException {
        VersionsExecution execution = changeRecorderLog.getUpdates().get(0);
        if (execution.getVersionChanges().isEmpty()) {
            // don't generate an empty file or add an entry with no changes
            return;
        }

        // marshall a clone without goal and date set
        VersionsExecution clone =
                objectFactory.createVersionsExecution().withVersionChanges(execution.getVersionChanges());

        try {
            // Marshall into a StringWriter to replace the namespace
            StringWriter sw = new StringWriter();
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.FALSE);
            marshaller.marshal(objectFactory.createUpdates(clone), sw);

            // Replace schema namespace from 3.0 to 2.0
            String xml = sw.toString()
                    .replace(
                            "http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0",
                            "http://www.mojohaus.org/versions-maven-plugin/schema/updates/2.0");

            Files.write(path, xml.getBytes(StandardCharsets.UTF_8));
        } catch (JAXBException e) {
            throw new IOException("Failed to write legacy XML ChangeRecorder", e);
        }
    }
}
