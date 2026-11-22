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

/**
 * Scenarios of {@link UpdatePropertiesMojo} that used to be the {@code it-update-properties-NNN} integration tests.
 * Each test runs the POM of the former IT against the versions of its test repository.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/update-properties")
class UpdatePropertiesMojoScenarioTest {

    private String run(UpdatePropertiesMojo mojo) throws Exception {
        mojo.repositorySystem = ItRepository.repositorySystem();
        mojo.execute();
        return new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-001/pom.xml")
    void updatesPropertyConfiguredForADependency(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>3.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-003/pom.xml")
    void updatesPropertyUsedInARangeAndKeepsTheRange(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<version>[${api}]</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-006/pom.xml")
    void updatesTwoConfiguredProperties(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<impl>2.2</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-007/pom.xml")
    void updatesLinkedPropertyAndKeepsTheReference(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<version>${api}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-008/pom.xml")
    void updatesPropertiesSharedBetweenDependencies(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<impl>2.2</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-010/pom.xml")
    void updatesPropertyToTheConfiguredRange(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>3.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-015/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    void staysWithinTheMajorVersion(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.1</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-016/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    void staysWithinTheMajorVersionForConfiguredAndLinkedProperties(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>2.1</api>");
        assertTextContains(pom, "<impl>1.4</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-019/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void keepsVersionWithoutIncrementalUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-021/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void appliesIncrementalUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.0.1</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-022/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    @MojoParameter(name = "allowIncrementalUpdates", value = "false")
    void keepsVersionWithoutAnyUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-023/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    @MojoParameter(name = "allowIncrementalUpdates", value = "false")
    void appliesBuildNumberUpdateOnly(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.1.0-2</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-002/pom.xml")
    void updatesPropertyWithinTheConfiguredRange(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.3</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-004/pom.xml")
    @MojoParameter(name = "autoLinkItems", value = "true")
    void updatesAutoLinkedPropertyWithinTheRangeOfItsDependency(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-009/pom.xml")
    @MojoParameter(name = "autoLinkItems", value = "true")
    void ignoresAutoLinkedPropertyOfAnInactiveProfile(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.1</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-017/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowSnapshots", value = "true")
    void updatesToASnapshotWithinTheMajorVersion(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.1.1-SNAPSHOT</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-018/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowSnapshots", value = "true")
    void updatesSnapshotAndLinkedPropertyWithinTheMajorVersion(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>2.1.1-SNAPSHOT</api>");
        assertTextContains(pom, "<impl>1.4</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-020/pom.xml")
    @MojoParameter(name = "allowSnapshots", value = "true")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void updatesToAnIncrementalSnapshot(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.1.1-SNAPSHOT</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-279-1/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-api")
    @MojoParameter(name = "allowSnapshots", value = "true")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowDowngrade", value = "true")
    @MojoParameter(name = "allowIncrementalUpdates", value = "true")
    @MojoParameter(name = "allowMinorUpdates", value = "true")
    void keepsASnapshotThatIsTheLatestAllowedVersion(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>1.9.1-SNAPSHOT</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-279-2/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-api")
    @MojoParameter(name = "allowSnapshots", value = "false")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowDowngrade", value = "true")
    @MojoParameter(name = "allowIncrementalUpdates", value = "true")
    @MojoParameter(name = "allowMinorUpdates", value = "true")
    void downgradesASnapshotToTheLatestReleaseWithinTheMajorVersion(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>1.3</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-279-3/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-api")
    @MojoParameter(name = "allowSnapshots", value = "false")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowDowngrade", value = "true")
    @MojoParameter(name = "allowIncrementalUpdates", value = "true")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void keepsASnapshotWhenOnlyIncrementalUpdatesAreAllowedAndNoneFits(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>1.9.1-SNAPSHOT</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-279-4/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-api")
    @MojoParameter(name = "allowSnapshots", value = "false")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowDowngrade", value = "true")
    @MojoParameter(name = "allowIncrementalUpdates", value = "true")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void downgradesASnapshotToTheLatestIncrementalRelease(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>1.2.2</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-279-5/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-api")
    @MojoParameter(name = "allowSnapshots", value = "false")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowDowngrade", value = "true")
    @MojoParameter(name = "allowIncrementalUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void downgradesASnapshotToTheLatestBuildNumberRelease(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>1.1.0-2</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-98-1/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-parent2")
    @MojoParameter(name = "allowSnapshots", value = "true")
    void keepsASnapshotOfAParentWhenSnapshotsAreAllowed(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>3.2-SNAPSHOT</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "issue-98-2/pom.xml")
    @MojoParameter(name = "includesList", value = "localhost:dummy-parent2")
    @MojoParameter(name = "allowSnapshots", value = "false")
    void keepsASnapshotOfAParentWhenNothingElseFits(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<versionProp>3.2-SNAPSHOT</versionProp>");
        assertTextContains(pom, "<version>${versionProp}</version>");
    }
}
