package org.codehaus.mojo.versions.model.util;

import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.model.VersionChange;

public enum VersionChangeSelector {
    DEPENDENCY,
    PROPERTY;

    public static <T extends VersionChange> VersionChangeSelector valueOf(T versionChange) {
        if (versionChange instanceof DependencyVersionChange) {
            return DEPENDENCY;
        }
        return PROPERTY;
    }

    @SuppressWarnings("unchecked")
    public <T extends VersionChange> T toVersionChange() {
        if (this == DEPENDENCY) return (T) new DependencyVersionChange();
        return (T) new PropertyVersionChange();
    }

    @Override
    public String toString() {
        return name().toLowerCase();
    }
}
