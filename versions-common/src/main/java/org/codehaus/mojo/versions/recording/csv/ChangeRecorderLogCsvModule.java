package org.codehaus.mojo.versions.recording.csv;

import com.fasterxml.jackson.databind.module.SimpleModule;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.ObjectFactory;

/**
 * A Jackson module for CSV serialization and deserialization of {@link ChangeRecorderLog}.
 * It registers the appropriate serializer and deserializer.
 *
 * @since 2.20.0
 */
public class ChangeRecorderLogCsvModule extends SimpleModule {
    /**
     * Creates a new module.
     *
     * @param objectFactory the object factory to use to create model instances during deserialization
     */
    public ChangeRecorderLogCsvModule(ObjectFactory objectFactory) {
        addSerializer(ChangeRecorderLog.class, new ChangeRecorderLogCsvSerializer());
        addDeserializer(ChangeRecorderLog.class, new ChangeRecorderLogCsvDeserializer(objectFactory));
    }
}
