package org.codehaus.mojo.versions.utils;

import org.apache.maven.model.Dependency;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DependencyComparatorTest {

    private Dependency dep(String groupId, String artifactId, String version) {
        return DependencyBuilder.newBuilder()
                .withGroupId(groupId)
                .withArtifactId(artifactId)
                .withVersion(version)
                .build();
    }

    @Test
    void comparesByGroupIdThenArtifactIdThenVersion() {
        Dependency a1 = dep("a.group", "artifact", "1.0");
        Dependency b1 = dep("b.group", "artifact", "1.0");

        assertTrue(
                DependencyComparator.INSTANCE.compare(a1, b1) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(a1, b1));
        assertTrue(
                DependencyComparator.INSTANCE.compare(b1, a1) > 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(b1, a1));
    }

    @Test
    void comparesByArtifactIdWhenGroupIdEqual() {
        Dependency a1 = dep("g", "a", "1.0");
        Dependency b1 = dep("g", "b", "1.0");

        assertTrue(
                DependencyComparator.INSTANCE.compare(a1, b1) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(a1, b1));
    }

    @Test
    void comparesByVersionWhenGroupAndArtifactEqual() {
        Dependency v1 = dep("g", "a", "1.0");
        Dependency v2 = dep("g", "a", "2.0");

        assertTrue(
                DependencyComparator.INSTANCE.compare(v1, v2) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(v1, v2));
    }

    @Test
    void nullDependenciesAreHandled() {
        Dependency d = dep("g", "a", "1.0");
        assertTrue(
                DependencyComparator.INSTANCE.compare(null, d) > 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(null, d));
        assertTrue(
                DependencyComparator.INSTANCE.compare(d, null) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(d, null));
        assertEquals(0, DependencyComparator.INSTANCE.compare(null, null));
    }

    @Test
    void numericVersionsCompareSemanticallyNotLexicographically() {
        Dependency v2 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("2")
                .build();
        Dependency v10 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("10")
                .build();

        // Semantic: 2 < 10
        assertTrue(
                DependencyComparator.INSTANCE.compare(v2, v10) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(v2, v10));
        // Lexicographic would wrongly say "10" < "2"
    }

    @Test
    void patchVersionsCompareSemanticallyNotLexicographically() {
        Dependency v102 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0.2")
                .build();
        Dependency v1010 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0.10")
                .build();

        // Semantic: 1.0.2 < 1.0.10
        assertTrue(
                DependencyComparator.INSTANCE.compare(v102, v1010) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(v102, v1010));
        // Lexicographic would wrongly say "1.0.10" < "1.0.2"
    }

    @Test
    void prereleaseIsLessThanRelease() {
        Dependency alpha = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0.0-alpha")
                .build();
        Dependency release = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0.0")
                .build();

        // Semantic: alpha < release
        assertTrue(
                DependencyComparator.INSTANCE.compare(alpha, release) < 0,
                () -> "was " + DependencyComparator.INSTANCE.compare(alpha, release));
        // Lexicographic would put "1.0.0-alpha" after "1.0.0"
    }

    @Test
    void versionsWithDifferentPrecisionAreEquivalent() {
        Dependency v1 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1")
                .build();
        Dependency v10 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0")
                .build();
        Dependency v100 = DependencyBuilder.newBuilder()
                .withGroupId("g")
                .withArtifactId("a")
                .withVersion("1.0.0")
                .build();

        // All should compare as equal
        assertEquals(0, DependencyComparator.INSTANCE.compare(v1, v10));
        assertEquals(0, DependencyComparator.INSTANCE.compare(v10, v100));
        assertEquals(0, DependencyComparator.INSTANCE.compare(v1, v100));
    }
}
