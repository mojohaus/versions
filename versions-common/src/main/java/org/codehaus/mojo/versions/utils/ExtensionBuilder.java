package org.codehaus.mojo.versions.utils;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Extension;
import org.apache.maven.model.InputLocation;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;

/**
 * Builder class for {@linkplain Extension}
 */
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class ExtensionBuilder {
    private Optional<String> groupId = empty();
    private Optional<String> artifactId = empty();
    private Optional<String> version = empty();
    private Map<Object, InputLocation> locations = new HashMap<>();

    private ExtensionBuilder() {}

    /**
     * Passes groupId to the builder
     * @param groupId given groupId
     * @return builder instance
     */
    public ExtensionBuilder withGroupId(String groupId) {
        this.groupId = ofNullable(groupId);
        return this;
    }

    /**
     * Passes artifactId to the builder
     * @param artifactId given artifactId
     * @return builder instance
     */
    public ExtensionBuilder withArtifactId(String artifactId) {
        this.artifactId = ofNullable(artifactId);
        return this;
    }

    /**
     * Passes version to the builder
     * @param version given version
     * @return builder instance
     */
    public ExtensionBuilder withVersion(String version) {
        this.version = ofNullable(version);
        return this;
    }

    /**
     * Passes type to the builder
     * @param key location key
     * @param location input location
     * @return builder instance
     */
    public ExtensionBuilder withLocation(Object key, InputLocation location) {
        this.locations.put(key, location);
        return this;
    }

    /**
     * Creates a new instance of the builder
     * @return new instance of the builder
     */
    public static ExtensionBuilder newBuilder() {
        return new ExtensionBuilder();
    }

    /**
     * Builds the {@linkplain Dependency} instance
     * @return {@linkplain Dependency} instance
     */
    public Extension build() {
        Extension inst = new Extension();
        groupId.ifPresent(inst::setGroupId);
        artifactId.ifPresent(inst::setArtifactId);
        version.ifPresent(inst::setVersion);
        locations.forEach(inst::setLocation);
        return inst;
    }
}
