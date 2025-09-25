package org.codehaus.mojo.versions.utils;

import org.apache.maven.model.Dependency;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;

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

        assertThat(DependencyComparator.INSTANCE.compare(a1, b1), lessThan(0));
        assertThat(DependencyComparator.INSTANCE.compare(b1, a1), greaterThan(0));
    }

    @Test
    void comparesByArtifactIdWhenGroupIdEqual() {
        Dependency a1 = dep("g", "a", "1.0");
        Dependency b1 = dep("g", "b", "1.0");

        assertThat(DependencyComparator.INSTANCE.compare(a1, b1), lessThan(0));
    }

    @Test
    void comparesByVersionWhenGroupAndArtifactEqual() {
        Dependency v1 = dep("g", "a", "1.0");
        Dependency v2 = dep("g", "a", "2.0");

        assertThat(DependencyComparator.INSTANCE.compare(v1, v2), lessThan(0));
    }

    @Test
    void nullDependenciesAreHandled() {
        Dependency d = dep("g", "a", "1.0");
        assertThat(DependencyComparator.INSTANCE.compare(null, d), greaterThan(0));
        assertThat(DependencyComparator.INSTANCE.compare(d, null), lessThan(0));
        assertThat(DependencyComparator.INSTANCE.compare(null, null), is(0));
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
        assertThat(DependencyComparator.INSTANCE.compare(v2, v10), lessThan(0));
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
        assertThat(DependencyComparator.INSTANCE.compare(v102, v1010), lessThan(0));
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
        assertThat(DependencyComparator.INSTANCE.compare(alpha, release), lessThan(0));
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
        assertThat(DependencyComparator.INSTANCE.compare(v1, v10), is(0));
        assertThat(DependencyComparator.INSTANCE.compare(v10, v100), is(0));
        assertThat(DependencyComparator.INSTANCE.compare(v1, v100), is(0));
    }
}
