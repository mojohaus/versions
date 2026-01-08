package org.codehaus.mojo.versions.recording.json;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.ExtensionVersionChange;
import org.codehaus.mojo.versions.model.PluginVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;

/**
 * Jackson mix-in to enable polymorphic (de)serialization of {@code VersionChange}
 * using an external discriminator property named "updateClass".
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "updateClass")
@JsonSubTypes({
    @JsonSubTypes.Type(value = DependencyVersionChange.class, name = "dependency"),
    @JsonSubTypes.Type(value = PropertyVersionChange.class, name = "property"),
    @JsonSubTypes.Type(value = PluginVersionChange.class, name = "plugin"),
    @JsonSubTypes.Type(value = ExtensionVersionChange.class, name = "extension")
})
public abstract class VersionChangeMixin {}
