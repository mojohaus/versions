package org.codehaus.mojo.versions.recording.json;

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
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.recording.JsonVersionChangeRecorderFactory;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

class JsonVersionChangeRecorderTest {

    private VersionChangeRecorder recorder;

    @BeforeEach
    public void setup() {
        MojoExecution mojoExecution = mock(MojoExecution.class);
        doReturn("test").when(mojoExecution).getGoal();
        recorder = new JsonVersionChangeRecorderFactory().create(null, mojoExecution);
    }

    @Test
    void testCreateFileAndSubdirectories() throws Exception {
        Path tempDir = createTempDir("testCreateFileAndSubdirectories");
        try {
            createChanges();
            Path reportPath = tempDir.resolve("dir1").resolve("dir2").resolve("report.csv");
            recorder.writeReport(reportPath);

            String output = String.join("", Files.readAllLines(reportPath)).replaceAll("\\s+", "");
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example0\",\"oldVersion\":\"0.0.1\",\"newVersion\":\"0.0.2\"}"));
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_MANAGEMENT_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example1\",\"oldVersion\":\"1.0.0\",\"newVersion\":\"2.0.0\"}"));
            assertThat(output, containsString("\"date\":"));
        } finally {
            tearDownTempDir(tempDir);
        }
    }

    @Test
    void testEmptyFile() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("ChangeRecorderTest")) {
            createChanges();
            recorder.writeReport(tempFile.getPath());

            String output =
                    String.join("", Files.readAllLines(tempFile.getPath())).replaceAll("\\s+", "");
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example0\",\"oldVersion\":\"0.0.1\",\"newVersion\":\"0.0.2\"}"));
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_MANAGEMENT_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example1\",\"oldVersion\":\"1.0.0\",\"newVersion\":\"2.0.0\"}"));
        }
    }

    private void createChanges() {
        recorder.recordChange(new DependencyVersionChange()
                .withKind(DependencyChangeKind.DEPENDENCY_UPDATE)
                .withGroupId("org.codehaus")
                .withArtifactId("example0")
                .withOldVersion("0.0.1")
                .withNewVersion("0.0.2"));

        recorder.recordChange(new DependencyVersionChange()
                .withKind(DependencyChangeKind.DEPENDENCY_MANAGEMENT_UPDATE)
                .withGroupId("org.codehaus")
                .withArtifactId("example1")
                .withOldVersion("1.0.0")
                .withNewVersion("2.0.0"));
    }

    @Test
    void testAppend() throws Exception {
        Path tempDir = createTempDir("testAppend");
        try {
            Path testFile = tempDir.resolve("versions-changes.json");
            Files.copy(
                    Paths.get("src/test/resources/org/codehaus/mojo/versions/recording/versions-changes.json"),
                    testFile);
            createChanges();
            recorder.writeReport(testFile);

            String output = String.join("", Files.readAllLines(testFile)).replaceAll("\\s+", "");
            assertThat(
                    output,
                    containsString(
                            "{\"versionChanges\":[{\"kind\":\"DEPENDENCY_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example0\",\"oldVersion\":\"0.0.1-SNAPSHOT\",\"newVersion\":\"0.0.1\"}],\"goal\":\"test\",\"date\":\"2025-10-03T05:05:38.437013556Z\"}"));
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example0\",\"oldVersion\":\"0.0.1-SNAPSHOT\",\"newVersion\":\"0.0.1\"}"));
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example0\",\"oldVersion\":\"0.0.1\",\"newVersion\":\"0.0.2\"}"));
            assertThat(
                    output,
                    containsString(
                            "{\"kind\":\"DEPENDENCY_MANAGEMENT_UPDATE\",\"groupId\":\"org.codehaus\",\"artifactId\":\"example1\",\"oldVersion\":\"1.0.0\",\"newVersion\":\"2.0.0\"}"));
            assertThat(output, containsString("\"date\":"));
        } finally {
            tearDownTempDir(tempDir);
        }
    }

    @Test
    void emptyResultShouldNotGenerateReports() throws Exception {
        Path path = Files.createTempDirectory("ChangeRecorderTest").resolve("ChangeRecorderTest.xml");
        recorder.writeReport(path);
        assertFalse(Files.isRegularFile(path), "File should not be created");
    }
}
