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

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.codehaus.mojo.versions.utils.TestVersionChangeRecorder;
import org.junit.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;

/**
 * Unit tests for {@link UpdatePropertiesMojo}
 */
public class UpdatePropertiesMojoTest extends UpdatePropertiesMojoTestBase {
    @Test
    public void testAllowMajorUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        setUpMojo("update-properties").execute();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DependencyVersionChange()
                        .withKind(DependencyChangeKind.PROPERTY_UPDATE)
                        .withGroupId("default-group")
                        .withArtifactId("default-artifact")
                        .withOldVersion("1.0.0")
                        .withNewVersion("2.0.0-M1")));
    }

    @Test
    public void testAllowMinorUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        UpdatePropertiesMojo mojo = setUpMojo("update-properties");
        mojo.allowMajorUpdates = false;
        mojo.execute();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DependencyVersionChange()
                        .withKind(DependencyChangeKind.PROPERTY_UPDATE)
                        .withGroupId("default-group")
                        .withArtifactId("default-artifact")
                        .withOldVersion("1.0.0")
                        .withNewVersion("1.1.0-alpha")));
    }

    @Test
    public void testAllowIncrementalUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        UpdatePropertiesMojo mojo = setUpMojo("update-properties");
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.execute();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DependencyVersionChange()
                        .withKind(DependencyChangeKind.PROPERTY_UPDATE)
                        .withGroupId("default-group")
                        .withArtifactId("default-artifact")
                        .withOldVersion("1.0.0")
                        .withNewVersion("1.0.1-rc1")));
    }

    @Test
    public void testChangesNotRegisteredIfNoUpdatesInPom() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-837"), pomDir);
        UpdatePropertiesMojo mojo = setUpMojo("update-properties");
        TestVersionChangeRecorder changeRecorder = new TestVersionChangeRecorder();
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        setVariableValueToObject(mojo, "changeRecorderFormat", "none");
        //            pomHelperClass.when( () -> PomHelper.setPropertyVersion( any(), anyString(), anyString(),
        // anyString() ) )
        //                            .thenReturn( false );
        mojo.execute();
        assertThat(changeRecorder.getChanges(), is(empty()));
    }

    @Test
    public void testIssue929() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-929"), pomDir);
        UpdatePropertiesMojo mojo = setUpMojo("update-properties");
        mojo.allowMajorUpdates = false;
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"6.2.5.Final", "8.0.0.Final"});
            }
        });

        mojo.execute();
        assertThat(changeRecorder.getChanges(), is(empty()));
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        testProblemCausingArtifact("update-properties");
    }
}
