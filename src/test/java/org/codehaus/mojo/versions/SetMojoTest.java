package org.codehaus.mojo.versions;

import static org.junit.Assert.assertEquals;

import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.junit.Test;

/**
 * Test cases covering SetMojo.
 */
public class SetMojoTest {
    private static final VersionComparator MAVEN = VersionComparators.getVersionComparator("maven");
    private static final VersionComparator NUMERIC = VersionComparators.getVersionComparator("numeric");

    @Test
    public void generateNextVersionMavenMajorMinorIncrement() {
        SetMojo set = new SetMojo();
        assertEquals("4.40.2", set.generateNextVersion(MAVEN, "4.40.1"));
    }

    @Test
    public void generateNextVersionMavenMajorMinorIncrementWithSnapshot() {
        SetMojo set = new SetMojo();
        assertEquals("4.40.2-SNAPSHOT", set.generateNextVersion(MAVEN, "4.40.1-SNAPSHOT"));
    }

    @Test
    public void generateNextVersionMavenSingleNumber() {
        SetMojo set = new SetMojo();
        assertEquals("42.0.1", set.generateNextVersion(MAVEN, "42"));
    }

    @Test
    public void generateNextVersionNumericSingleNumber() {
        SetMojo set = new SetMojo();
        assertEquals("43", set.generateNextVersion(NUMERIC, "42"));
    }

    @Test
    public void generateNextVersionMavenFourNumericParts() {
        SetMojo set = new SetMojo();
        assertEquals("1.0.0.1", set.generateNextVersion(MAVEN, "1.0.0.0"));
    }

    @Test
    public void generateNextVersionMavenMajorMinor() {
        SetMojo set = new SetMojo();
        assertEquals("1.0.1", set.generateNextVersion(MAVEN, "1.0"));
    }

    @Test
    public void generateNextVersionNumericMajorMinor() {
        SetMojo set = new SetMojo();
        assertEquals("1.1", set.generateNextVersion(NUMERIC, "1.0"));
    }
}
