package org.codehaus.mojo.versions.recording.json;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
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
    public JsonVersionChangeModule() {
        addDeserializer(VersionChange.class, new JsonDeserializer<VersionChange>() {
            @Override
            public VersionChange deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
                ObjectCodec codec = p.getCodec();
                JsonNode node = codec.readTree(p);

                if (node.has("kind")) {
                    return codec.treeToValue(node, DependencyVersionChange.class);
                } else if (node.has("property")) {
                    return codec.treeToValue(node, PropertyVersionChange.class);
                } else {
                    throw new JsonMappingException(p, "Unknown VersionChange subtype: " + node.toString());
                }
            }
        });
    }
}
