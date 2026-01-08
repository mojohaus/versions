package org.codehaus.mojo.versions.recording.json;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleDeserializers;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.module.SimpleSerializers;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.VersionChange;

/**
 * A Jackson module for JSON deserialization of {@link VersionChange}.
 * It registers a deserializer that can handle the polymorphic nature of VersionChange.
 *
 * @since 2.20.0
 */
public class JsonVersionChangeModule extends SimpleModule {
    /**
     * Creates a new module.
     */
    public JsonVersionChangeModule() {}

    @Override
    public void setupModule(SetupContext context) {
        super.setupModule(context);

        SimpleSerializers serializers = new SimpleSerializers();
        serializers.addSerializer(DependencyChangeKind.class, new DependencyChangeKindSerializer());
        context.addSerializers(serializers);

        SimpleDeserializers deserializers = new SimpleDeserializers();
        deserializers.addDeserializer(DependencyChangeKind.class, new DependencyChangeKindDeserializer());
        context.addDeserializers(deserializers);
    }

    private static final class DependencyChangeKindSerializer extends JsonSerializer<DependencyChangeKind> {
        @Override
        public void serialize(DependencyChangeKind value, JsonGenerator gen, SerializerProvider serializers)
                throws IOException {
            if (value == null) {
                gen.writeNull();
            } else {
                gen.writeString(value.value());
            }
        }
    }

    private static final class DependencyChangeKindDeserializer extends JsonDeserializer<DependencyChangeKind> {
        @Override
        public DependencyChangeKind deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
            String text = p.getValueAsString();
            try {
                return DependencyChangeKind.fromValue(text);
            } catch (IllegalArgumentException e) {
                throw JsonMappingException.from(p, "Unknown DependencyChangeKind: " + text, e);
            }
        }
    }
}
