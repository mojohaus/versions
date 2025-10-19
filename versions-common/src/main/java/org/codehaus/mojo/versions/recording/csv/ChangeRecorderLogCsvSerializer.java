package org.codehaus.mojo.versions.recording.csv;

import java.io.IOException;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.dataformat.csv.CsvGenerator;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.model.VersionChange;
import org.codehaus.mojo.versions.model.VersionsExecution;
import org.codehaus.mojo.versions.model.xjb.ZonedDateTimeXmlAdapter;

/**
 * A CSV serializer for {@link ChangeRecorderLog}.
 *
 * @since 2.20.0
 */
public class ChangeRecorderLogCsvSerializer extends JsonSerializer<ChangeRecorderLog> {

    /**
     * Creates a new serializer.
     */
    public ChangeRecorderLogCsvSerializer() {}

    @Override
    public void serialize(ChangeRecorderLog changeRecorderLog, JsonGenerator gen, SerializerProvider serializers)
            throws IOException {
        CsvGenerator csvGen = (CsvGenerator) gen;
        for (VersionsExecution execution : changeRecorderLog.getUpdates()) {
            String executionId = UUID.randomUUID().toString();
            for (VersionChange change : execution.getVersionChanges()) {
                // Start a new row
                csvGen.writeStartObject();

                // Common fields
                csvGen.writeStringField("execution", executionId);
                csvGen.writeStringField("goal", execution.getGoal());
                csvGen.writeStringField(
                        "date", execution.getDate() != null ? ZonedDateTimeXmlAdapter.print(execution.getDate()) : "");

                if (change instanceof DependencyVersionChange) {
                    DependencyVersionChange dep = (DependencyVersionChange) change;
                    csvGen.writeStringField("kind", dep.getKind().name());
                    csvGen.writeStringField("groupId", dep.getGroupId());
                    csvGen.writeStringField("artifactId", dep.getArtifactId());
                    csvGen.writeStringField("oldVersion", dep.getOldVersion());
                    csvGen.writeStringField("newVersion", dep.getNewVersion());
                    // leave property fields empty
                    csvGen.writeStringField("property", "");
                    csvGen.writeStringField("oldValue", "");
                    csvGen.writeStringField("newValue", "");
                } else if (change instanceof PropertyVersionChange) {
                    PropertyVersionChange prop = (PropertyVersionChange) change;
                    // leave dependency fields empty
                    csvGen.writeStringField("kind", "");
                    csvGen.writeStringField("groupId", "");
                    csvGen.writeStringField("artifactId", "");
                    csvGen.writeStringField("oldVersion", "");
                    csvGen.writeStringField("newVersion", "");
                    // property fields
                    csvGen.writeStringField("property", prop.getProperty());
                    csvGen.writeStringField("oldValue", prop.getOldValue());
                    csvGen.writeStringField("newValue", prop.getNewValue());
                }

                csvGen.writeEndObject();
            }
        }
    }
}
