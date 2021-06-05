package org.codehaus.mojo.versions.ordering;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.junit.Test;

import static org.hamcrest.Matchers.arrayContaining;
import static org.junit.Assert.assertThat;

public class MajorMinorIncrementalFilterTest {

    private ArtifactVersion[] newerVersions = new ArtifactVersion[] { version("1.1.1-sp1"),
                                                                      version("1.1.1-1"),
                                                                      version("1.1.2"),
                                                                      version("1.1.3"),
                                                                      version("1.2.0"),
                                                                      version("2.0.0-SNAPSHOT") };

    @Test
    public void checkFilter() {
        ArtifactVersion selectedVersion = version("1.1.1");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(true, true, true);
        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion, newerVersions);
        assertThat(filteredVersions, arrayContaining(
            version("1.1.1-sp1"), version("1.1.1-1"),
            version("1.1.2"), version("1.1.3"),
            version("1.2.0"), version("2.0.0-SNAPSHOT")));
    }

    @Test
    public void checkFilterWithNoMajorUpdates() {
        ArtifactVersion selectedVersion = version("1.1.1");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(false, true, true);
        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion, newerVersions);
        assertThat(filteredVersions, arrayContaining(version("1.1.1-sp1"), version("1.1.1-1"),
                                                     version("1.1.2"), version("1.1.3"), version("1.2.0")));
    }

    @Test
    public void checkFilterWithNoMajorOrMinorUpdates() {
        ArtifactVersion selectedVersion = version("1.1.1");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(false, false, true);
        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion, newerVersions);
        assertThat(filteredVersions, arrayContaining(version("1.1.1-sp1"), version("1.1.1-1"),
                                                     version("1.1.2"), version("1.1.3")));
    }

    @Test
    public void checkFilterWithNoMajorOrMinorOrIncrementalUpdates() {
        ArtifactVersion selectedVersion = version("1.1.1");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(false, false, false);
        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion, newerVersions);
        assertThat(filteredVersions, arrayContaining(version("1.1.1-sp1"), version("1.1.1-1")));
    }

    @Test
    public void checkFilterWithSnapshotAtSameVersion() {
        ArtifactVersion selectedVersion = version("1.1.1-SNAPSHOT");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(false, false, false);
        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion,
                                                           new ArtifactVersion[] {version("1.1.1")});
        assertThat(filteredVersions, arrayContaining(version("1.1.1")));
    }


    @Test
    public void checkFilterWithNonStandardVersions() {
        ArtifactVersion selectedVersion = version("1.1.1.1");
        MajorMinorIncrementalFilter filter = new MajorMinorIncrementalFilter(true, true, true);

        ArtifactVersion[] newerVersions = new ArtifactVersion[] { version("1.1.1.1-sp1"),
                                                                  version("1.1.1.2"),
                                                                  version("1.1.2.21"),
                                                                  version("1.1.3.0"),
                                                                  version("1.2.0"),
                                                                  version("1.2.0.1"),
                                                                  version("2.0.0-SNAPSHOT") };


        ArtifactVersion[] filteredVersions = filter.filter(selectedVersion, newerVersions);
        assertThat(filteredVersions,
                   arrayContaining(version("1.1.1.1-sp1"),
                                   version("1.1.1.2"),
                                   version("1.1.2.21"),
                                   version("1.1.3.0"),
                                   version("1.2.0"),
                                   version("1.2.0.1"),
                                   version("2.0.0-SNAPSHOT")));
    }

    private ArtifactVersion version(String versionString) {
        return new DefaultArtifactVersion(versionString);
    }
}
