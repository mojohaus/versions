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

import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.emptyList;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for {@link UpdatePropertiesMojo}
 */
public class UpdatePropertyMojoTest extends UpdatePropertiesMojoTestBase {
    @Test
    public void testAllowMajorUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        UpdatePropertyMojo mojo = setUpMojo("update-property");
        mojo.property = "artifact-version";
        mojo.execute();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "default-artifact", "1.0.0", "2.0.0-M1")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testAllowMinorUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        UpdatePropertyMojo mojo = setUpMojo("update-property");
        mojo.property = "artifact-version";
        mojo.allowMajorUpdates = false;
        mojo.execute();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "default-artifact", "1.0.0", "1.1.0-alpha")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testAllowIncrementalUpdates() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml"),
                Paths.get(pomDir.toString(), "pom.xml"),
                REPLACE_EXISTING);
        UpdatePropertyMojo mojo = setUpMojo("update-property");
        mojo.property = "artifact-version";
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.execute();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "default-artifact", "1.0.0", "1.0.1-rc1")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * The property is defined in the reactor's parent POM and only used in a module, which is where the goal runs.
     */
    @Test
    public void testIssue997PropertyInReactorParent() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-997"), pomDir);
        String module = new String(Files.readAllBytes(pomDir.resolve("module1/pom.xml")));
        UpdatePropertyMojo mojo = setUpReactorMojo("update-property", "module1");
        mojo.property = "artifact-version";
        mojo.execute();

        assertTextContains(
                new String(Files.readAllBytes(pomDir.resolve("pom.xml"))),
                "<artifact-version>2.0.0-M1</artifact-version>");
        assertEquals(module, new String(Files.readAllBytes(pomDir.resolve("module1/pom.xml"))));
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "default-artifact", "1.0.0", "2.0.0-M1")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * A parent POM outside the reactor is not written, even when its file is reachable by {@code relativePath}.
     */
    @Test
    public void testPropertyInParentOutsideReactor() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-properties/issue-837"), pomDir);
        String parent = new String(Files.readAllBytes(pomDir.resolve("parent-pom.xml")));
        UpdatePropertyMojo mojo = setUpMojo("update-property");
        mojo.property = "artifact-version";
        mojo.execute();

        assertEquals(parent, new String(Files.readAllBytes(pomDir.resolve("parent-pom.xml"))));
        assertEquals(emptyList(), changeRecorder.getChanges());
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        testProblemCausingArtifact("update-property", m -> ((UpdatePropertyMojo) m).property = "artifact-version");
    }
}
