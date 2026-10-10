package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.TestLog;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.TextAssertions.assertTextNotContains;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

/**
 * Unit tests for the versions.skip parameter across different mojos.
 *
 * @author Claude Code
 */
@ProjectMojoTest
public class SkipParameterTest {
    protected ProjectMojos mojos;

    private Path tempDir;
    private ArtifactFactory artifactFactory;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        tempDir = TestUtils.createTempDir("skip");
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
    }

    @AfterEach
    public void tearDown() throws Exception {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testSetMojoSkip() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/remove-snapshot/pom.xml"),
                Paths.get(tempDir.toString(), "pom.xml"),
                REPLACE_EXISTING);

        TestLog testLog = new TestLog();
        SetMojo mojo = mojos.lookup("set", tempDir);
        setVariableValueToObject(mojo, "skip", true);
        setVariableValueToObject(mojo, "log", testLog);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));

        // Verify the file was not modified (version should still be 1.0-SNAPSHOT, not 1.0)
        String pomContent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(pomContent, "<version>1.0-SNAPSHOT</version>");
        assertTextNotContains(pomContent, "<version>1.0</version>");
    }

    @Test
    public void testDisplayDependencyUpdatesMojoSkip() throws Exception {
        TestLog testLog = new TestLog();
        DisplayDependencyUpdatesMojo mojo =
                new DisplayDependencyUpdatesMojo(artifactFactory, mockAetherRepositorySystem(), null, null);
        mojo.skip = true;
        setVariableValueToObject(mojo, "log", testLog);

        MavenProject project = new MavenProject();
        project.setOriginalModel(new Model());
        MavenSession session = mock(MavenSession.class);
        doReturn(Collections.singletonList(project)).when(session).getProjects();
        setVariableValueToObject(mojo, "project", project);
        setVariableValueToObject(mojo, "session", session);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));
    }

    @Test
    public void testCommitMojoSkip() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/remove-snapshot/pom.xml"),
                Paths.get(tempDir.toString(), "pom.xml"),
                REPLACE_EXISTING);

        // Create a backup file to ensure it's not deleted when skip=true
        Path backupFile = tempDir.resolve("pom.xml.versionsBackup");
        Files.write(backupFile, "backup content".getBytes());

        TestLog testLog = new TestLog();
        CommitMojo mojo = new CommitMojo();
        setVariableValueToObject(mojo, "skip", true);
        setVariableValueToObject(mojo, "log", testLog);

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());
        setVariableValueToObject(mojo, "project", project);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));

        // Verify the backup file was not deleted
        assertEquals(true, Files.exists(backupFile));
    }

    @Test
    public void testDisplayParentUpdatesMojoSkip() throws Exception {
        TestLog testLog = new TestLog();
        DisplayParentUpdatesMojo mojo =
                new DisplayParentUpdatesMojo(artifactFactory, mockAetherRepositorySystem(), null, null);
        mojo.skip = true;
        setVariableValueToObject(mojo, "log", testLog);

        MavenProject project = new MavenProject();
        project.setOriginalModel(new Model());
        MavenSession session = mock(MavenSession.class);
        doReturn(Collections.singletonList(project)).when(session).getProjects();
        setVariableValueToObject(mojo, "project", project);
        setVariableValueToObject(mojo, "session", session);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));
    }

    @Test
    public void testUseDepVersionMojoSkip() throws Exception {
        TestLog testLog = new TestLog();
        UseDepVersionMojo mojo = new UseDepVersionMojo(artifactFactory, mockAetherRepositorySystem(), null, null);
        mojo.skip = true;
        setVariableValueToObject(mojo, "log", testLog);

        MavenProject project = new MavenProject();
        project.setOriginalModel(new Model());
        MavenSession session = mock(MavenSession.class);
        doReturn(Collections.singletonList(project)).when(session).getProjects();
        setVariableValueToObject(mojo, "project", project);
        setVariableValueToObject(mojo, "session", session);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));
    }

    @Test
    public void testAbstractVersionsUpdaterMojoSkip() throws Exception {
        // Test that the base class skip parameter works for mojos that don't override execute()
        TestLog testLog = new TestLog();

        // Use ForceReleasesMojo as it extends AbstractVersionsUpdaterMojo without overriding execute()
        ForceReleasesMojo mojo = new ForceReleasesMojo(artifactFactory, mockAetherRepositorySystem(), null, null);
        mojo.skip = true;
        setVariableValueToObject(mojo, "log", testLog);

        MavenProject project = new MavenProject();
        project.setFile(tempDir.resolve("pom.xml").toFile());
        project.setOriginalModel(new Model());
        setVariableValueToObject(mojo, "project", project);

        mojo.execute();

        // Verify that "Skipping execution" was logged
        assertEquals(
                true,
                testLog.getLoggedMessages().stream()
                        .anyMatch(triple -> triple.getMiddle() != null
                                && triple.getMiddle().toString().contains("Skipping execution")));
    }
}
