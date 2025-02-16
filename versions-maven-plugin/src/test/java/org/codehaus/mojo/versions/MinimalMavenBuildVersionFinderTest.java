package org.codehaus.mojo.versions;

import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Unit tests for MinimalMavenBuildVersionFinder.
 */
public class MinimalMavenBuildVersionFinderTest {

    @Test
    public void testValidVersionRanges() {
        ArtifactVersion expectedMinimumVersion = ArtifactVersionService.getArtifactVersion("1.0");
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

    @Test
    public void testGetGreatestVersion() {
        assertEquals(
                MinimalMavenBuildVersionFinder.getGreatestVersion(
                        new DefaultArtifactVersion("1"), new DefaultArtifactVersion("2")),
                Optional.of(new DefaultArtifactVersion("2")));
        assertEquals(
                MinimalMavenBuildVersionFinder.getGreatestVersion(
                        new DefaultArtifactVersion("1"),
                        new DefaultArtifactVersion("2"),
                        new DefaultArtifactVersion("3")),
                Optional.of(new DefaultArtifactVersion("3")));
        assertEquals(
                MinimalMavenBuildVersionFinder.getGreatestVersion(
                        new DefaultArtifactVersion("3"),
                        new DefaultArtifactVersion("2"),
                        new DefaultArtifactVersion("1")),
                Optional.of(new DefaultArtifactVersion("3")));
        assertEquals(
                MinimalMavenBuildVersionFinder.getGreatestVersion(
                        new DefaultArtifactVersion("1"), null, new DefaultArtifactVersion("3")),
                Optional.of(new DefaultArtifactVersion("3")));
        assertEquals(
                MinimalMavenBuildVersionFinder.getGreatestVersion(new DefaultArtifactVersion("1"), null),
                Optional.of(new DefaultArtifactVersion("1")));
    }
}
