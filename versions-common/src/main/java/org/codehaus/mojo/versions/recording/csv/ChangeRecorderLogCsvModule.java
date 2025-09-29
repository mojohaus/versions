package org.codehaus.mojo.versions.recording.csv;

import com.fasterxml.jackson.databind.module.SimpleModule;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.ObjectFactory;

public class ChangeRecorderLogCsvModule extends SimpleModule {
    public ChangeRecorderLogCsvModule(ObjectFactory objectFactory) {
        addSerializer(ChangeRecorderLog.class, new ChangeRecorderLogCsvSerializer());
        addDeserializer(ChangeRecorderLog.class, new ChangeRecorderLogCsvDeserializer(objectFactory));
    }
}
