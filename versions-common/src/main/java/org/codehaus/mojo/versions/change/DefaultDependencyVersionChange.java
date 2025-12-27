package org.codehaus.mojo.versions.change;

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

import org.codehaus.mojo.versions.api.change.DependencyVersionChange;

/**
 * Represents a change of an artifact's version.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 14:48:10
 *
 * @deprecated replaced by {@code versions-model}
 */
@Deprecated
public final class DefaultDependencyVersionChange implements DependencyVersionChange {
    private final String groupId;

    private final String artifactId;

    private final String oldVersion;

    private final String newVersion;

    /**
     * Creates a new instance, providing groupId, artifactId of the dependency as well as the old and new versions.
     * @param groupId groupId of the dependency
     * @param artifactId artifactId of the dependency
     * @param oldVersion old version
     * @param newVersion new version
     */
    public DefaultDependencyVersionChange(String groupId, String artifactId, String oldVersion, String newVersion) {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.oldVersion = oldVersion;
        this.newVersion = newVersion;
    }

    /**
     * Returns the groupId of the dependency
     * @return groupId of the dependency
     */
    public String getGroupId() {
        return groupId;
    }

    /**
     * Returns the artifact of the dependency
     * @return artifactId of the dependency
     */
    public String getArtifactId() {
        return artifactId;
    }

    /**
     * Returns the old version of the dependency
     * @return old version of the dependency
     */
    public String getOldVersion() {
        return oldVersion;
    }

    /**
     * Returns the new version of the dependency
     * @return new version of the dependency
     */
    public String getNewVersion() {
        return newVersion;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        DefaultDependencyVersionChange versionChange = (DefaultDependencyVersionChange) o;

        if (!Objects.equals(artifactId, versionChange.artifactId)) {
            return false;
        }
        if (!Objects.equals(groupId, versionChange.groupId)) {
            return false;
        }
        if (!Objects.equals(newVersion, versionChange.newVersion)) {
            return false;
        }
        return Objects.equals(oldVersion, versionChange.oldVersion);
    }

    @Override
    public int hashCode() {
        int result = groupId != null ? groupId.hashCode() : 0;
        result = 31 * result + (artifactId != null ? artifactId.hashCode() : 0);
        result = 31 * result + (oldVersion != null ? oldVersion.hashCode() : 0);
        result = 31 * result + (newVersion != null ? newVersion.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "DefaultDependencyVersionChange(" + groupId + ':' + artifactId + ":" + oldVersion + "-->" + newVersion
                + ')';
    }
}
