package org.codehaus.mojo.versions.recording.csv;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.dataformat.csv.CsvParser;
import org.codehaus.mojo.versions.model.ChangeRecorderLog;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.ExtensionVersionChange;
import org.codehaus.mojo.versions.model.ObjectFactory;
import org.codehaus.mojo.versions.model.PluginVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.model.VersionsExecution;
import org.codehaus.mojo.versions.model.xjb.ZonedDateTimeXmlAdapter;

/**
 * A CSV deserializer for {@link ChangeRecorderLog}.
 *
 * @since 2.20.0
 */
public class ChangeRecorderLogCsvDeserializer extends StdDeserializer<ChangeRecorderLog> {
    /**
     * The object factory to use to create model instances.
     */
    private final ObjectFactory objectFactory;

    /**
     * Creates a new deserializer.
     *
     * @param objectFactory the object factory to use to create model instances
     */
    public ChangeRecorderLogCsvDeserializer(ObjectFactory objectFactory) {
        super(VersionsExecution.class);
        this.objectFactory = objectFactory;
    }

    @Override
    public ChangeRecorderLog deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        CsvParser csvParser = (CsvParser) p;
        ObjectCodec codec = p.getCodec();

        ChangeRecorderLog changeRecorderLog = objectFactory.createChangeRecorderLog();

        // We’ll read row by row
        String previousExecutionId = null;
        for (VersionsExecution exec = null; !csvParser.isClosed() && csvParser.nextToken() != null; ) {
            JsonNode node = codec.readTree(p);

            if (node.hasNonNull("execution")) {
                String executionId = node.get("execution").asText();
                if (!executionId.equals(previousExecutionId)) {
                    exec = objectFactory.createVersionsExecution();
                    changeRecorderLog.getUpdates().add(exec);
                    previousExecutionId = executionId;
                }
            }

            assert exec != null;
            if (exec.getGoal() == null && node.hasNonNull("goal")) {
                exec.setGoal(node.get("goal").asText());
            }
            if (exec.getDate() == null && node.hasNonNull("date")) {
                exec.setDate(ZonedDateTimeXmlAdapter.parse(node.get("date").asText()));
            }

            String updateClass = node.get("updateClass").asText();
            if ("dependency".equals(updateClass)) {
                DependencyVersionChange dep = objectFactory.createDependencyVersionChange();
                dep.setKind(DependencyChangeKind.fromValue(node.get("kind").asText()));
                dep.setGroupId(node.get("groupId").asText());
                dep.setArtifactId(node.get("artifactId").asText());
                if (node.hasNonNull("oldValue")) {
                    dep.setOldVersion(node.get("oldValue").asText());
                }
                dep.setNewVersion(node.get("newValue").asText());
                exec.getVersionChanges().add(dep);
            } else if ("extension".equals(updateClass)) {
                ExtensionVersionChange ext = objectFactory.createExtensionVersionChange();
                ext.setGroupId(node.get("groupId").asText());
                ext.setArtifactId(node.get("artifactId").asText());
                if (node.hasNonNull("oldValue")) {
                    ext.setOldVersion(node.get("oldValue").asText());
                }
                ext.setNewVersion(node.get("newValue").asText());
                exec.getVersionChanges().add(ext);
            } else if ("plugin".equals(updateClass)) {
                PluginVersionChange plugin = objectFactory.createPluginVersionChange();
                plugin.setGroupId(node.get("groupId").asText());
                plugin.setArtifactId(node.get("artifactId").asText());
                if (node.hasNonNull("oldValue")) {
                    plugin.setOldVersion(node.get("oldValue").asText());
                }
                plugin.setNewVersion(node.get("newValue").asText());
                if (node.hasNonNull("minMavenVersion")) {
                    plugin.setMinimumMavenVersion(node.get("minMavenVersion").asText());
                }
                exec.getVersionChanges().add(plugin);
            } else if ("property".equals(updateClass)) {
                PropertyVersionChange prop = objectFactory.createPropertyVersionChange();
                prop.setProperty(node.get("property").asText());
                prop.setOldValue(node.get("oldValue").asText());
                prop.setNewValue(node.get("newValue").asText());
                exec.getVersionChanges().add(prop);
            } else {
                throw new IOException("Unknown updateClass: " + updateClass);
            }
        }
        return changeRecorderLog;
    }
}
