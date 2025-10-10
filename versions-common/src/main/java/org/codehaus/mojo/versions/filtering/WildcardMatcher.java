package org.codehaus.mojo.versions.filtering;

import java.util.function.Predicate;

import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;

/**
 * A simple "matcher" object providing a utility whether the given string matches a pattern consisting of
 * strings containing the wildcard ({@code *}) character. Each occurrence of the wildcard can be expanded
 * to match any string.
 */
public class WildcardMatcher implements Predicate<String> {
    /**
     * The wildcard character.
     */
    public static final String WILDCARD = "*";

    private final String pattern;

    /**
     * Creates a new matcher for the given pattern. Each occurrence of the wildcard ({@code *}) can be expanded
     * to match any string.
     *
     * @param pattern pattern to be matched against.
     */
    protected WildcardMatcher(String pattern) {
        this.pattern = pattern;
    }

    /**
     * Creates a new matcher for the given pattern. Each occurrence of the wildcard ({@code *}) can be expanded
     * to match any string.
     *
     * @param pattern pattern to be matched against.
     * @return a new instance satisfying the provided pattern
     */
    public static WildcardMatcher parse(String pattern) {
        return new WildcardMatcher(pattern);
    }

    @Override
    public boolean test(String token) {
        if (token == null) {
            return WILDCARD.equals(pattern);
        }

        boolean matches;

        // support full wildcard and implied wildcard
        if (WILDCARD.equals(pattern) || pattern.isEmpty()) {
            matches = true;
        }
        // support contains wildcard
        else if (pattern.startsWith(WILDCARD) && pattern.endsWith(WILDCARD)) {
            String contains = pattern.substring(1, pattern.length() - 1);

            matches = token.contains(contains);
        }
        // support leading wildcard
        else if (pattern.startsWith(WILDCARD)) {
            matches = token.endsWith(pattern.substring(1));
        }
        // support trailing wildcard
        else if (pattern.endsWith(WILDCARD)) {
            String prefix = pattern.substring(0, pattern.length() - 1);

            matches = token.startsWith(prefix);
        }
        // support versions range
        else if (pattern.startsWith("[") || pattern.startsWith("(")) {
            matches = isVersionIncludedInRange(token, pattern);
        }
        // support exact match
        else {
            matches = token.equals(pattern);
        }

        return matches;
    }

    private boolean isVersionIncludedInRange(final String version, final String range) {
        try {
            return VersionRange.createFromVersionSpec(range)
                    .containsVersion(ArtifactVersionService.getArtifactVersion(version));
        } catch (InvalidVersionSpecificationException e) {
            return false;
        }
    }

    /**
     * Returns the pattern to be matched against
     * @return pattern
     */
    public String getPattern() {
        return pattern;
    }

    @Override
    public String toString() {
        return String.format("%s{%s}", getClass().getSimpleName(), pattern);
    }
}
