package org.codehaus.mojo.versions.recording.csv;

import java.io.IOException;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.dataformat.csv.CsvGenerator;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.ExtensionVersionChange;
import org.codehaus.mojo.versions.model.PluginVersionChange;
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
                csvGen.writeStringField("execution", executionId);
                csvGen.writeStringField("goal", execution.getGoal());
                csvGen.writeStringField(
                        "date", execution.getDate() != null ? ZonedDateTimeXmlAdapter.print(execution.getDate()) : "");
                if (change instanceof DependencyVersionChange) {
                    writeDependency(csvGen, (DependencyVersionChange) change);
                } else if (change instanceof ExtensionVersionChange) {
                    writeExtension(csvGen, (ExtensionVersionChange) change);
                } else if (change instanceof PropertyVersionChange) {
                    writeProperty(csvGen, (PropertyVersionChange) change);
                } else if (change instanceof PluginVersionChange) {
                    writePlugin(csvGen, (PluginVersionChange) change);
                } else {
                    throw new IOException("Unknown VersionChange subtype: "
                            + change.getClass().getName());
                }
                csvGen.writeEndObject();
            }
        }
    }

    private static void writePlugin(CsvGenerator csvGen, PluginVersionChange plugin) throws IOException {
        csvGen.writeStringField("updateClass", "plugin");
        csvGen.writeOmittedField("kind");
        csvGen.writeStringField("groupId", plugin.getGroupId());
        csvGen.writeStringField("artifactId", plugin.getArtifactId());
        csvGen.writeOmittedField("property");
        if (plugin.getOldVersion() != null) {
            csvGen.writeStringField("oldValue", plugin.getOldVersion());
        } else {
            csvGen.writeOmittedField("oldValue");
        }
        csvGen.writeStringField("newValue", plugin.getNewVersion());
        csvGen.writeStringField("minMavenVersion", plugin.getMinimumMavenVersion());
    }

    private static void writeProperty(CsvGenerator csvGen, PropertyVersionChange prop) throws IOException {
        csvGen.writeStringField("updateClass", "property");
        csvGen.writeOmittedField("kind");
        csvGen.writeOmittedField("groupId");
        csvGen.writeOmittedField("artifactId");
        csvGen.writeStringField("property", prop.getProperty());
        csvGen.writeStringField("oldValue", prop.getOldValue());
        csvGen.writeStringField("newValue", prop.getNewValue());
        csvGen.writeOmittedField("minMavenVersion");
    }

    private static void writeDependency(CsvGenerator csvGen, DependencyVersionChange dep) throws IOException {
        csvGen.writeStringField("updateClass", "dependency");
        csvGen.writeStringField("kind", dep.getKind().value());
        csvGen.writeStringField("groupId", dep.getGroupId());
        csvGen.writeStringField("artifactId", dep.getArtifactId());
        csvGen.writeOmittedField("property");
        if (dep.getOldVersion() != null) {
            csvGen.writeStringField("oldValue", dep.getOldVersion());
        } else {
            csvGen.writeOmittedField("oldValue");
        }
        csvGen.writeStringField("newValue", dep.getNewVersion());
        csvGen.writeOmittedField("minMavenVersion");
    }

    private static void writeExtension(CsvGenerator csvGen, ExtensionVersionChange ext) throws IOException {
        csvGen.writeStringField("updateClass", "extension");
        csvGen.writeOmittedField("kind");
        csvGen.writeStringField("groupId", ext.getGroupId());
        csvGen.writeStringField("artifactId", ext.getArtifactId());
        csvGen.writeOmittedField("property");
        if (ext.getOldVersion() != null) {
            csvGen.writeStringField("oldValue", ext.getOldVersion());
        } else {
            csvGen.writeOmittedField("oldValue");
        }
        csvGen.writeStringField("newValue", ext.getNewVersion());
        csvGen.writeOmittedField("minMavenVersion");
    }
}
