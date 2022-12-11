package org.codehaus.mojo.versions.recording;

import java.util.Objects;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.change.VersionChange;
import org.codehaus.mojo.versions.api.recording.ChangeRecord;
import org.codehaus.mojo.versions.change.DefaultVersionChange;

public class DefaultChangeRecord implements ChangeRecord {
    private final ChangeKind kind;
    private final VersionChange versionChange;

    private DefaultChangeRecord(ChangeKind kind, VersionChange versionChange) {
        this.kind = Objects.requireNonNull(kind, "kind must not be null");
        this.versionChange = Objects.requireNonNull(versionChange, "versionChange must not be null");
    }

    @Override
    public ChangeKind getKind() {
        return kind;
    }

    @Override
    public VersionChange getVersionChange() {
        return versionChange;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        private ChangeKind kind;
        private String groupId;
        private String artifactId;
        private String oldVersion;
        private String newVersion;

        public Builder withKind(ChangeKind kind) {
            this.kind = kind;
            return this;
        }

        public Builder withGroupId(String groupId) {
            this.groupId = groupId;
            return this;
        }

        public Builder withArtifactId(String artifactId) {
            this.artifactId = artifactId;
            return this;
        }

        public Builder withOldVersion(String oldVersion) {
            this.oldVersion = oldVersion;
            return this;
        }

        public Builder withNewVersion(String newVersion) {
            this.newVersion = newVersion;
            return this;
        }

        public Builder withDependency(Dependency dependency) {
            groupId = dependency.getGroupId();
            artifactId = dependency.getArtifactId();
            oldVersion = dependency.getVersion();
            return this;
        }

        public Builder withArtifact(Artifact artifact) {
            groupId = artifact.getGroupId();
            artifactId = artifact.getArtifactId();
            oldVersion = artifact.getVersion();
            return this;
        }

        public ChangeRecord build() {
            return new DefaultChangeRecord(kind, new DefaultVersionChange(groupId, artifactId, oldVersion, newVersion));
        }
    }
}
