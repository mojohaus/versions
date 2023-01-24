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

import java.util.Optional;

import org.apache.maven.model.Dependency;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;

/**
 * Builder class for {@linkplain Dependency}
 */
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class DependencyBuilder {
    private Optional<String> groupId = empty();
    private Optional<String> artifactId = empty();
    private Optional<String> version = empty();
    private Optional<String> type = empty();
    private Optional<String> classifier = empty();
    private Optional<String> scope = empty();
    private Optional<String> optional = empty();

    private DependencyBuilder() {}

    /**
     * Passes groupId to the builder
     * @param groupId given groupId
     * @return builder instance
     */
    public DependencyBuilder withGroupId(String groupId) {
        this.groupId = ofNullable(groupId);
        return this;
    }

    /**
     * Passes artifactId to the builder
     * @param artifactId given artifactId
     * @return builder instance
     */
    public DependencyBuilder withArtifactId(String artifactId) {
        this.artifactId = ofNullable(artifactId);
        return this;
    }

    /**
     * Passes version to the builder
     * @param version given version
     * @return builder instance
     */
    public DependencyBuilder withVersion(String version) {
        this.version = ofNullable(version);
        return this;
    }

    /**
     * Passes type to the builder
     * @param type given type
     * @return builder instance
     */
    public DependencyBuilder withType(String type) {
        this.type = ofNullable(type);
        return this;
    }

    /**
     * Passes classifier to the builder
     * @param classifier given classifier
     * @return builder instance
     */
    public DependencyBuilder withClassifier(String classifier) {
        this.classifier = ofNullable(classifier);
        return this;
    }

    /**
     * Passes scope to the builder
     * @param scope given scope
     * @return builder instance
     */
    public DependencyBuilder withScope(String scope) {
        this.scope = ofNullable(scope);
        return this;
    }

    /**
     * Passes optional to the builder as String
     * @param optional given optional as String
     * @return builder instance
     */
    public DependencyBuilder withOptional(String optional) {
        this.optional = ofNullable(optional);
        return this;
    }

    /**
     * Passes optional to the builder as boolean
     * @param optional given optional as boolean
     * @return builder instance
     */
    public DependencyBuilder withOptional(boolean optional) {
        this.optional = of(String.valueOf(optional));
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
     * Convenience builder method
     * @param groupId groupId of the dependency
     * @param artifactId artifactId of the dependency
     * @param version version of the dependency
     * @return new instance of {@linkplain Dependency}
     * @deprecated please use the {@link #newBuilder()} method instead
     */
    @Deprecated
    public static Dependency dependencyWith(String groupId, String artifactId, String version) {
        return newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version)
                .build();
    }

    /**
     * Convenience builder method
     * @param groupId groupId of the dependency
     * @param artifactId artifactId of the dependency
     * @param version version of the dependency
     * @param type type of the dependency
     * @param classifier classifier of the dependency
     * @param scope scope of the dependency
     * @return new instance of {@linkplain Dependency}
     * @deprecated please use the {@link #newBuilder()} method instead
     */
    @Deprecated
    public static Dependency dependencyWith(
            String groupId, String artifactId, String version, String type, String classifier, String scope) {
        return newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version)
                .withType(type)
                .withClassifier(classifier)
                .withScope(scope)
                .build();
    }

    /**
     * Builds the {@linkplain Dependency} instance
     * @return {@linkplain Dependency} instance
     */
    public Dependency build() {
        Dependency inst = new Dependency();
        groupId.ifPresent(inst::setGroupId);
        artifactId.ifPresent(inst::setArtifactId);
        version.ifPresent(inst::setVersion);
        type.ifPresent(inst::setType);
        classifier.ifPresent(inst::setClassifier);
        scope.ifPresent(inst::setScope);
        optional.ifPresent(inst::setOptional);

        return inst;
    }
}
