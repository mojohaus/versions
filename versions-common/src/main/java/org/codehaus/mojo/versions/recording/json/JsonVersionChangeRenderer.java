package org.codehaus.mojo.versions.recording.json;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.exc.StreamReadException;
import com.fasterxml.jackson.core.exc.StreamWriteException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DatabindException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.recording.ChangeRecorderRenderer;

import static com.fasterxml.jackson.databind.SerializationFeature.WRITE_DATES_AS_TIMESTAMPS;

/**
 * A JSON renderer for the {@link ChangeRecorderLog}
 */
public class JsonVersionChangeRenderer implements ChangeRecorderRenderer {
    private final ObjectMapper mapper;

    /**
     * Creates a new instance of JsonVersionChangeRenderer.
     */
    public JsonVersionChangeRenderer() {
        mapper = new ObjectMapper();
        // Do not serialize null-valued properties so attributes like oldVersion are omitted
        // from the JSON output when they are null. Register a mix-in annotated with
        // @JsonInclude(Include.NON_NULL) to avoid using deprecated or unavailable APIs.
        mapper.addMixIn(Object.class, JsonIncludeNonNullMixIn.class);
        // Register polymorphic mix-in for VersionChange so Jackson writes/reads
        // the "updateClass" discriminator automatically.
        mapper.addMixIn(
                org.codehaus.mojo.versions.model.VersionChange.class,
                org.codehaus.mojo.versions.recording.json.VersionChangeMixin.class);
        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(new JsonVersionChangeModule());
        mapper.disable(WRITE_DATES_AS_TIMESTAMPS);
    }

    // Local mix-in to exclude null-valued properties without touching generated models.
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private abstract static class JsonIncludeNonNullMixIn {}

    @Override
    public ChangeRecorderLog read(Path path) throws IOException {
        if (path == null || !Files.exists(path) || Files.size(path) == 0) {
            return null;
        }
        try {
            return mapper.readValue(path.toFile(), new TypeReference<ChangeRecorderLog>() {});
        } catch (DatabindException | StreamReadException e) {
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
            mapper.writerWithDefaultPrettyPrinter().writeValue(path.toFile(), log);
        } catch (DatabindException | StreamWriteException e) {
            throw new IOException("Failed to write ChangeRecorderLog to " + path, e);
        }
    }
}
