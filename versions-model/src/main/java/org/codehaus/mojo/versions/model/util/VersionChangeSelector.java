package org.codehaus.mojo.versions.model.util;

import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.model.VersionChange;

/**
 * Enum to select between different {@link VersionChange} types.
 */
public enum VersionChangeSelector {
    /**
     * Selects a {@link DependencyVersionChange}.
     */
    DEPENDENCY,
    /**
     * Selects a {@link PropertyVersionChange}.
     */
    PROPERTY;

    /**
     * Selects the appropriate enum constant for the given {@link VersionChange} instance.
     * @param versionChange the version change instance
     * @param <T> the type of the version change
     * @return the corresponding enum constant
     */
    public static <T extends VersionChange> VersionChangeSelector valueOf(T versionChange) {
        if (versionChange instanceof DependencyVersionChange) {
            return DEPENDENCY;
        }
        return PROPERTY;
    }

    /**
     * Creates a new {@link VersionChange} instance based on the enum constant.
     * @param <T> the type of the version change
     * @return the corresponding version change instance
     */
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
