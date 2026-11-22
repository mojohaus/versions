package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.apache.maven.api.plugin.testing.Basedir;
import org.apache.maven.api.plugin.testing.InjectMojo;
import org.apache.maven.api.plugin.testing.MojoParameter;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotContains;

/**
 * Scenarios of the {@code use-latest-*}, {@code use-next-*} and {@code use-releases} goals that used to be the
 * {@code it-use-*} integration tests. Each test runs the POM of the former IT against the versions of its test
 * repository.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/use-goals")
class UseGoalsScenarioTest {

    private String run(AbstractVersionsUpdaterMojo mojo) throws Exception {
        mojo.repositorySystem = ItRepository.repositorySystem();
        mojo.execute();
        return new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "use-latest-releases-001/pom.xml")
    void useLatestReleasesUpdatesToTheLatestRelease(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>3.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "use-latest-releases-004/pom.xml")
    void useLatestReleasesWithoutMajorUpdates(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.3</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "use-latest-releases-005/pom.xml")
    void useLatestReleasesSkipsABetaOfTheNextMajor(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextNotContains(run(mojo), "<version>2.0.0-beta</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-001/pom.xml")
    void useLatestVersionsUpdatesToTheLatestVersion(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>3.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-004/pom.xml")
    void useLatestVersionsSkipsABetaOfTheNextMajor(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextNotContains(run(mojo), "<version>2.0.0-beta</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-005/pom.xml")
    void useLatestVersionsWithoutAnyUpdate(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>2.0.8</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-006/pom.xml")
    void useLatestVersionsWithIncrementalUpdatesOnly(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>2.0.11</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-007/pom.xml")
    void useLatestVersionsWithMinorUpdates(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>2.2.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-008/pom.xml")
    void useLatestVersionsLeavesAManagedPropertyAlone(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api.version>2.0.8</api.version>");
        assertTextContains(pom, "<version>${api.version}</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "use-next-releases-001/pom.xml")
    void useNextReleasesUpdatesToTheNextRelease(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "use-next-releases-002/pom.xml")
    void useNextReleasesIgnoresSnapshotsEvenWhenAllowed(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "use-next-releases-004/pom.xml")
    void useNextReleasesRespectsExcludes(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-versions", pom = "use-next-versions-001/pom.xml")
    void useNextVersionsUpdatesToTheNextVersion(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-versions", pom = "use-next-versions-002/pom.xml")
    void useNextVersionsWithoutSnapshots(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-releases", pom = "use-releases-001/pom.xml")
    void useReleasesKeepsAReleaseVersion(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.1.1-2</version>");
    }

    @Test
    @InjectMojo(goal = "use-dep-version", pom = "use-dep-version-001/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost")
    @MojoParameter(name = "depVersion", value = "1.0.1")
    void useDepVersionSetsTheDependencyVersion(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.0.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-dep-version", pom = "use-dep-version-003/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost")
    @MojoParameter(name = "depVersion", value = "1.0.1")
    void useDepVersionSetsTheVersionOfAProfileDependency(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.0.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-dep-version", pom = "use-dep-version-004/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost")
    @MojoParameter(name = "depVersion", value = "1.0.1")
    @MojoParameter(name = "processProperties", value = "true")
    void useDepVersionUpdatesTheLinkedProperty(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<revision>1.0.1</revision>");
        assertTextContains(pom, "<version>${revision}</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "use-latest-releases-002/pom.xml")
    void useLatestReleasesLeavesASnapshotAlone(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-snapshots", pom = "use-latest-snapshots-001/pom.xml")
    @MojoParameter(name = "allowMinorUpdates", value = "true")
    void useLatestSnapshotsWithMinorUpdates(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.9.1-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-snapshots", pom = "use-latest-snapshots-002/pom.xml")
    void useLatestSnapshotsDoesNotDowngradeARelease(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-snapshots", pom = "use-latest-snapshots-005/pom.xml")
    void useLatestSnapshotsUpdatesToTheLatestSnapshot(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>3.1.5-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-002/pom.xml")
    void useLatestVersionsUpdatesADependency(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>3.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-versions", pom = "use-latest-versions-003/pom.xml")
    void useLatestVersionsUpdatesAnotherDependency(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>3.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "use-next-releases-003/pom.xml")
    void useNextReleasesLeavesASnapshotAlone(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-snapshots", pom = "use-next-snapshots-001/pom.xml")
    void useNextSnapshotsUpdatesToTheNextSnapshot(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-snapshots", pom = "use-next-snapshots-002/pom.xml")
    void useNextSnapshotsKeepsASnapshotWhenThereIsNoNextOne(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-versions", pom = "use-next-versions-002-snapshot/pom.xml")
    void useNextVersionsMovesToTheNextSnapshot(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2-SNAPSHOT</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-versions", pom = "use-next-versions-003/pom.xml")
    void useNextVersionsMovesToTheNextRelease(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-releases", pom = "use-releases-002/pom.xml")
    void useReleasesReplacesASnapshotByItsRelease(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.2</version>");
    }

    @Test
    @InjectMojo(goal = "use-releases", pom = "use-releases-003/pom.xml")
    @MojoParameter(name = "allowRangeMatching", value = "true")
    @MojoParameter(name = "failIfNotReplaced", value = "true")
    void useReleasesMatchesTheRange(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<version>1.1.3</version>");
    }
}
