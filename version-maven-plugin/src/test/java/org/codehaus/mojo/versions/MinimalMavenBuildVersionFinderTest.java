package org.codehaus.mojo.versions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.junit.Test;

/**
 * Unit tests for MinimalMavenBuildVersionFinder.
 */
public class MinimalMavenBuildVersionFinderTest {

    @Test
    public void testValidVersionRanges() {
        DefaultArtifactVersion expectedMinimumVersion = new DefaultArtifactVersion("1.0");
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("1.0"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("[1.0]"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("[1.0,)"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(1.0,)"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("[1.0,2.0]"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(1.0,2.0)"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(1.0,2.0]"));
        assertEquals(expectedMinimumVersion, MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("[1.0,2.0)"));
    }

    @Test
    public void testInvalidVersionRanges() {
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(,1.0]"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(,1.0)"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(,1.0],[1.2,)"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(,1.1),(1.1,)"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(,1.1),[1.1,)"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(1.0"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("1.0)"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("1.0()"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("()1.0"));
        assertNull(MinimalMavenBuildVersionFinder.getMinimumVersionFromRange("(1.0]"));
    }

}
