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

public class JsonVersionChangeModule extends SimpleModule {
    {
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
