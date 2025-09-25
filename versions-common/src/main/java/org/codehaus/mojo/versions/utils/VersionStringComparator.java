package org.codehaus.mojo.versions.utils;

import java.util.Comparator;

import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.InvalidVersionSpecificationException;
import org.eclipse.aether.version.Version;
import org.eclipse.aether.version.VersionScheme;

public enum VersionStringComparator implements Comparator<String> {
    STRICT(false),
    LENIENT(true);

    private final boolean lenient;

    VersionStringComparator(boolean lenient) {
        this.lenient = lenient;
    }

    private static final VersionScheme VERSION_SCHEME;

    private static final Version NULL_VERSION;

    static {
        VERSION_SCHEME = new GenericVersionScheme();
        try {
            NULL_VERSION = VERSION_SCHEME.parseVersion("0");
        } catch (InvalidVersionSpecificationException e) {
            throw new IllegalArgumentException(e);
        }
    }

    private Version extractVersion(String v) {
        try {
            return VERSION_SCHEME.parseVersion(v);
        } catch (InvalidVersionSpecificationException e) {
            if (lenient) {
                return NULL_VERSION;
            }
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public int compare(String s1, String s2) {
        return Comparator.nullsLast(Comparator.comparing(this::extractVersion)).compare(s1, s2);
    }
}
