package org.codehaus.mojo.versions.utils;

import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

        assertTrue(PluginComparator.INSTANCE.compare(a, b) < 0, () -> "was " + PluginComparator.INSTANCE.compare(a, b));
        assertTrue(PluginComparator.INSTANCE.compare(b, a) > 0, () -> "was " + PluginComparator.INSTANCE.compare(b, a));
    }

    @Test
    void comparesByArtifactIdWhenGroupIdEqual() {
        Plugin a = plugin("g", "a", "1.0");
        Plugin b = plugin("g", "b", "1.0");

        assertTrue(PluginComparator.INSTANCE.compare(a, b) < 0, () -> "was " + PluginComparator.INSTANCE.compare(a, b));
    }

    @Test
    void comparesByVersionWhenGroupAndArtifactEqual() {
        Plugin v1 = plugin("g", "a", "1.0");
        Plugin v2 = plugin("g", "a", "2.0");

        assertTrue(
                PluginComparator.INSTANCE.compare(v1, v2) < 0,
                () -> "was " + PluginComparator.INSTANCE.compare(v1, v2));
    }

    @Test
    void worksWithReportPluginAsWell() {
        ReportPlugin rp1 = reportPlugin("g", "a", "1.0");
        ReportPlugin rp2 = reportPlugin("g", "a", "2.0");

        assertTrue(
                PluginComparator.INSTANCE.compare(rp1, rp2) < 0,
                () -> "was " + PluginComparator.INSTANCE.compare(rp1, rp2));
    }

    @Test
    void crossComparePluginAndReportPlugin() {
        Plugin p = plugin("g", "a", "1.0");
        ReportPlugin rp = reportPlugin("g", "a", "2.0");

        assertTrue(
                PluginComparator.INSTANCE.compare(p, rp) < 0, () -> "was " + PluginComparator.INSTANCE.compare(p, rp));
        assertTrue(
                PluginComparator.INSTANCE.compare(rp, p) > 0, () -> "was " + PluginComparator.INSTANCE.compare(rp, p));
    }

    @Test
    void semanticComparisonDiffersFromLexicographicNumeric() {
        Plugin v2 = plugin("g", "a", "2");
        Plugin v10 = plugin("g", "a", "10");

        // Semantic: 2 < 10
        assertTrue(
                PluginComparator.INSTANCE.compare(v2, v10) < 0,
                () -> "was " + PluginComparator.INSTANCE.compare(v2, v10));
    }

    @Test
    void semanticComparisonDiffersFromLexicographicPatch() {
        Plugin v102 = plugin("g", "a", "1.0.2");
        Plugin v1010 = plugin("g", "a", "1.0.10");

        // Semantic: 1.0.2 < 1.0.10
        assertTrue(
                PluginComparator.INSTANCE.compare(v102, v1010) < 0,
                () -> "was " + PluginComparator.INSTANCE.compare(v102, v1010));
    }

    @Test
    void versionsWithDifferentPrecisionAreEquivalent() {
        Plugin v1 = plugin("g", "a", "1");
        Plugin v10 = plugin("g", "a", "1.0");
        Plugin v100 = plugin("g", "a", "1.0.0");

        assertEquals(0, PluginComparator.INSTANCE.compare(v1, v10));
        assertEquals(0, PluginComparator.INSTANCE.compare(v10, v100));
        assertEquals(0, PluginComparator.INSTANCE.compare(v1, v100));
    }

    @Test
    void prereleaseIsLessThanRelease() {
        Plugin alpha = plugin("g", "a", "1.0.0-alpha");
        Plugin release = plugin("g", "a", "1.0.0");

        assertTrue(
                PluginComparator.INSTANCE.compare(alpha, release) < 0,
                () -> "was " + PluginComparator.INSTANCE.compare(alpha, release));
    }

    @Test
    void throwsOnNonPluginObjects() {
        Object notAPlugin = new Object();
        Plugin p = plugin("g", "a", "1.0");

        org.junit.jupiter.api.Assertions.assertThrows(
                IllegalArgumentException.class, () -> PluginComparator.INSTANCE.compare(notAPlugin, p));
    }
}
