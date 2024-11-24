package org.codehaus.mojo.versions.utils;

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

import java.util.HashMap;
import java.util.Map;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.InputLocation;

/**
 * Builder class for {@linkplain Dependency}
 */
public class DependencyBuilder {
    public enum Location {
        GROUP_ID("groupId"),
        ARTIFACT_ID("artifactId"),
        VERSION("version");

        private final String stringValue;

        Location(String stringValue) {
            this.stringValue = stringValue;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    private static final Dependency TEMPLATE = new Dependency();
    private String groupId = TEMPLATE.getGroupId();
    private String artifactId = TEMPLATE.getArtifactId();
    private String version = TEMPLATE.getVersion();
    private String type = TEMPLATE.getType();
    private String classifier = TEMPLATE.getClassifier();
    private String scope = TEMPLATE.getScope();
    private String optional = TEMPLATE.getOptional();
    private final Map<String, InputLocation> inputLocationMap = new HashMap<>();

    private DependencyBuilder() {}

    /**
     * Passes groupId to the builder
     * @param groupId given groupId
     * @return builder instance
     */
    public DependencyBuilder withGroupId(String groupId) {
        this.groupId = groupId;
        return this;
    }

    /**
     * Passes artifactId to the builder
     * @param artifactId given artifactId
     * @return builder instance
     */
    public DependencyBuilder withArtifactId(String artifactId) {
        this.artifactId = artifactId;
        return this;
    }

    /**
     * Passes version to the builder
     * @param version given version
     * @return builder instance
     */
    public DependencyBuilder withVersion(String version) {
        this.version = version;
        return this;
    }

    /**
     * Passes type to the builder
     * @param type given type
     * @return builder instance
     */
    public DependencyBuilder withType(String type) {
        this.type = type;
        return this;
    }

    /**
     * Passes classifier to the builder
     * @param classifier given classifier
     * @return builder instance
     */
    public DependencyBuilder withClassifier(String classifier) {
        this.classifier = classifier;
        return this;
    }

    /**
     * Passes scope to the builder
     * @param scope given scope
     * @return builder instance
     */
    public DependencyBuilder withScope(String scope) {
        this.scope = scope;
        return this;
    }

    /**
     * Passes optional to the builder as String
     * @param optional given optional as String
     * @return builder instance
     */
    public DependencyBuilder withOptional(String optional) {
        this.optional = optional;
        return this;
    }

    /**
     * Passes optional to the builder as boolean
     * @param optional given optional as boolean
     * @return builder instance
     */
    public DependencyBuilder withOptional(boolean optional) {
        this.optional = String.valueOf(optional);
        return this;
    }

    public DependencyBuilder withLocation(String element, InputLocation location) {
        this.inputLocationMap.put(element, location);
        return this;
    }

    /**
     * Creates a new instance of the builder
     * @return new instance of the builder
     */
    public static DependencyBuilder newBuilder() {
        return new DependencyBuilder();
    }

    /**
     * Builds the {@linkplain Dependency} instance
     * @return {@linkplain Dependency} instance
     */
    public Dependency build() {
        Dependency inst = new Dependency();
        inst.setGroupId(groupId);
        inst.setArtifactId(artifactId);
        inst.setVersion(version);
        inst.setType(type);
        inst.setClassifier(classifier);
        inst.setScope(scope);
        inst.setOptional(optional);
        inputLocationMap.forEach(inst::setLocation);
        return inst;
    }
}
