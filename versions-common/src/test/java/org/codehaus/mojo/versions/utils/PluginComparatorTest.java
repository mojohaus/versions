package org.codehaus.mojo.versions.utils;

import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;

class PluginComparatorTest {

    private Plugin plugin(String groupId, String artifactId, String version) {
        Plugin p = new Plugin();
        p.setGroupId(groupId);
        p.setArtifactId(artifactId);
        p.setVersion(version);
        return p;
    }

    private ReportPlugin reportPlugin(String groupId, String artifactId, String version) {
        ReportPlugin rp = new ReportPlugin();
        rp.setGroupId(groupId);
        rp.setArtifactId(artifactId);
        rp.setVersion(version);
        return rp;
    }

    @Test
    void comparesByGroupIdThenArtifactIdThenVersion() {
        Plugin a = plugin("a.group", "artifact", "1.0");
        Plugin b = plugin("b.group", "artifact", "1.0");

        assertThat(PluginComparator.INSTANCE.compare(a, b), lessThan(0));
        assertThat(PluginComparator.INSTANCE.compare(b, a), greaterThan(0));
    }

    @Test
    void comparesByArtifactIdWhenGroupIdEqual() {
        Plugin a = plugin("g", "a", "1.0");
        Plugin b = plugin("g", "b", "1.0");

        assertThat(PluginComparator.INSTANCE.compare(a, b), lessThan(0));
    }

    @Test
    void comparesByVersionWhenGroupAndArtifactEqual() {
        Plugin v1 = plugin("g", "a", "1.0");
        Plugin v2 = plugin("g", "a", "2.0");

        assertThat(PluginComparator.INSTANCE.compare(v1, v2), lessThan(0));
    }

    @Test
    void worksWithReportPluginAsWell() {
        ReportPlugin rp1 = reportPlugin("g", "a", "1.0");
        ReportPlugin rp2 = reportPlugin("g", "a", "2.0");

        assertThat(PluginComparator.INSTANCE.compare(rp1, rp2), lessThan(0));
    }

    @Test
    void crossComparePluginAndReportPlugin() {
        Plugin p = plugin("g", "a", "1.0");
        ReportPlugin rp = reportPlugin("g", "a", "2.0");

        assertThat(PluginComparator.INSTANCE.compare(p, rp), lessThan(0));
        assertThat(PluginComparator.INSTANCE.compare(rp, p), greaterThan(0));
    }

    @Test
    void semanticComparisonDiffersFromLexicographicNumeric() {
        Plugin v2 = plugin("g", "a", "2");
        Plugin v10 = plugin("g", "a", "10");

        // Semantic: 2 < 10
        assertThat(PluginComparator.INSTANCE.compare(v2, v10), lessThan(0));
    }

    @Test
    void semanticComparisonDiffersFromLexicographicPatch() {
        Plugin v102 = plugin("g", "a", "1.0.2");
        Plugin v1010 = plugin("g", "a", "1.0.10");

        // Semantic: 1.0.2 < 1.0.10
        assertThat(PluginComparator.INSTANCE.compare(v102, v1010), lessThan(0));
    }

    @Test
    void versionsWithDifferentPrecisionAreEquivalent() {
        Plugin v1 = plugin("g", "a", "1");
        Plugin v10 = plugin("g", "a", "1.0");
        Plugin v100 = plugin("g", "a", "1.0.0");

        assertThat(PluginComparator.INSTANCE.compare(v1, v10), is(0));
        assertThat(PluginComparator.INSTANCE.compare(v10, v100), is(0));
        assertThat(PluginComparator.INSTANCE.compare(v1, v100), is(0));
    }

    @Test
    void prereleaseIsLessThanRelease() {
        Plugin alpha = plugin("g", "a", "1.0.0-alpha");
        Plugin release = plugin("g", "a", "1.0.0");

        assertThat(PluginComparator.INSTANCE.compare(alpha, release), lessThan(0));
    }

    @Test
    void throwsOnNonPluginObjects() {
        Object notAPlugin = new Object();
        Plugin p = plugin("g", "a", "1.0");

        org.junit.jupiter.api.Assertions.assertThrows(
                IllegalArgumentException.class, () -> PluginComparator.INSTANCE.compare(notAPlugin, p));
    }
}
