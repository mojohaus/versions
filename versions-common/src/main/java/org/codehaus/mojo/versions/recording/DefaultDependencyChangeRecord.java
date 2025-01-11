package org.codehaus.mojo.versions.recording;

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

import java.util.Objects;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.change.DependencyVersionChange;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;

public class DefaultDependencyChangeRecord implements DependencyChangeRecord {
    private final ChangeKind kind;
    private final DependencyVersionChange versionChange;

    private DefaultDependencyChangeRecord(ChangeKind kind, DependencyVersionChange versionChange) {
        this.kind = Objects.requireNonNull(kind, "kind must not be null");
        this.versionChange = Objects.requireNonNull(versionChange, "versionChange must not be null");
    }

    @Override
    public ChangeKind getKind() {
        return kind;
    }

    @Override
    public DependencyVersionChange getVersionChange() {
        return versionChange;
    }

    public static Builder builder() {
        return new Builder();
    }

    /**
     * A builder object for {@link DependencyChangeRecord} instances
     */
    public static class Builder {
        private ChangeKind kind;
        private String groupId;
        private String artifactId;
        private String oldVersion;
        private String newVersion;

        /**
         * Supplies the kind
         * @param kind requested kind
         * @return builder instance
         */
        public Builder withKind(ChangeKind kind) {
            this.kind = kind;
            return this;
        }

        /**
         * Supplies the groupId
         * @param groupId requested groupId
         * @return builder instance
         */
        public Builder withGroupId(String groupId) {
            this.groupId = groupId;
            return this;
        }

        /**
         * Supplies the artifactId
         * @param artifactId requested artifactId
         * @return builder instance
         */
        public Builder withArtifactId(String artifactId) {
            this.artifactId = artifactId;
            return this;
        }

        /**
         * Supplies the version from before the change
         * @param oldVersion version from before the change
         * @return builder instance
         */
        public Builder withOldVersion(String oldVersion) {
            this.oldVersion = oldVersion;
            return this;
        }

        /**
         * Supplies the version from after the change
         * @param newVersion version from after the change
         * @return builder instance
         */
        public Builder withNewVersion(String newVersion) {
            this.newVersion = newVersion;
            return this;
        }

        /**
         * Supplies the dependency
         * @param dependency requested dependency
         * @return builder instance
         */
        public Builder withDependency(Dependency dependency) {
            groupId = dependency.getGroupId();
            artifactId = dependency.getArtifactId();
            oldVersion = dependency.getVersion();
            return this;
        }

        /**
         * Supplies the artifact
         * @param artifact requested artifact
         * @return builder instance
         */
        public Builder withArtifact(Artifact artifact) {
            groupId = artifact.getGroupId();
            artifactId = artifact.getArtifactId();
            oldVersion = artifact.getVersion();
            return this;
        }

        /**
         * Builds the {@link DependencyChangeRecord} instance
         * @return {@link DependencyChangeRecord} instance
         */
        public DependencyChangeRecord build() {
            return new DefaultDependencyChangeRecord(
                    kind, new DefaultDependencyVersionChange(groupId, artifactId, oldVersion, newVersion));
        }
    }
}
