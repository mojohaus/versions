package org.codehaus.mojo.versions.recording.xml;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.recording.ChangeRecorderRenderer;

public final class XmlVersionChangeRenderer implements ChangeRecorderRenderer {

    private static final String SCHEMA_LOCATION =
            "http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0 updates-3.0.xsd";

    private final JAXBContext jaxbContext;

    private final ObjectFactory objectFactory;

    public XmlVersionChangeRenderer(ObjectFactory objectFactory) {
        this.objectFactory = objectFactory;
        try {
            this.jaxbContext = JAXBContext.newInstance(ChangeRecorderLog.class);
        } catch (JAXBException e) {
            throw new IllegalStateException("Failed to initialize JAXBContext for ChangeRecorderLog", e);
        }
    }

    @Override
    public ChangeRecorderLog read(Path path) throws IOException {
        if (path == null || !Files.exists(path) || Files.size(path) == 0) {
            return null;
        }
        try {
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            JAXBElement<ChangeRecorderLog> root =
                    unmarshaller.unmarshal(new StreamSource(path.toFile()), ChangeRecorderLog.class);
            return root.getValue();
        } catch (JAXBException e) {
            throw new IOException("Failed to read ChangeRecorderLog from " + path, e);
        }
    }

    @Override
    public void write(Path path, ChangeRecorderLog log) throws IOException {
        if (path == null) {
            throw new IllegalArgumentException("output path not provided");
        }
        if (log == null) {
            throw new IllegalArgumentException("ChangeRecorderLog is null");
        }
        try {
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
            marshaller.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, SCHEMA_LOCATION);
            marshaller.marshal(objectFactory.createChangeRecorderLog(log), path.toFile());
        } catch (JAXBException e) {
            throw new IOException("Failed to write ChangeRecorderLog to " + path, e);
        }
    }
}
