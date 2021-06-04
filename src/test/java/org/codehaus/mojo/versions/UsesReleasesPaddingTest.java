package org.codehaus.mojo.versions;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.junit.Assert;
import org.junit.Test;

public class UsesReleasesPaddingTest {

    @Test
    public void testPadding() {
        UseReleasesMojo useReleasesMojo = new UseReleasesMojo();

        Assert.assertEquals("wrong padded version", useReleasesMojo.getPaddedArtifactVersion("1.0"), "1.0.0");
        Assert.assertEquals("wrong padded version", useReleasesMojo.getPaddedArtifactVersion("1.1"), "1.1.0");
        Assert.assertEquals("wrong padded version", useReleasesMojo.getPaddedArtifactVersion("1.0.1"), "1.0.1");

    }

}
