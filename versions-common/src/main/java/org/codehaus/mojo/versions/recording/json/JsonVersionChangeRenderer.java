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

import com.fasterxml.jackson.core.exc.StreamReadException;
import com.fasterxml.jackson.core.exc.StreamWriteException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DatabindException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.codehaus.mojo.versions.api.recording.ChangeRecorderRenderer;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;

/**
 * A JSON renderer for the {@link ChangeRecorderLog}
 */
public class JsonVersionChangeRenderer implements ChangeRecorderRenderer {
    private final ObjectMapper mapper;

    public JsonVersionChangeRenderer() {
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(new JsonVersionChangeModule());
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }

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
