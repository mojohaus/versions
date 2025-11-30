package org.codehaus.mojo.versions.recording.xml;

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
import java.util.Collections;

import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.recording.XmlVersionChangeRecorderFactory;
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

class XmlVersionChangeRecorderTest {

    private VersionChangeRecorder recorder;

    @BeforeEach
    public void setup() {
        MojoExecution mojoExecution = mock(MojoExecution.class);
        doReturn("test").when(mojoExecution).getGoal();
        recorder = new XmlVersionChangeRecorderFactory()
                .create(null, mojoExecution, new SystemStreamLog(), Collections.singletonMap("legacy", "false"));
    }

    @Test
    void testCreateFileAndSubdirectories() throws Exception {
        Path tempDir = createTempDir("testCreateFileAndSubdirectories");
        try {
            createChanges();
            Path reportPath = tempDir.resolve("dir1").resolve("dir2").resolve("report.xml");
            recorder.writeReport(reportPath);

            String output = String.join("", Files.readAllLines(reportPath));
            assertThat(
                    output,
                    containsString(
                            "changeRecorderLog xmlns=\"http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0\""));
            assertThat(output, containsString("<updates goal=\"test\" date="));
            assertThat(
                    output,
                    containsString(
                            "<dependencyUpdate kind=\"dependency-update\" groupId=\"org.codehaus\" artifactId=\"example0\" oldVersion=\"0.0.1\" newVersion=\"0.0.2\"/>"));
            assertThat(
                    output,
                    containsString(
                            "<dependencyUpdate kind=\"dependency-management-update\" groupId=\"org.codehaus\" artifactId=\"example1\" newVersion=\"2.0.0\"/>"));
        } finally {
            tearDownTempDir(tempDir);
        }
    }

    @Test
    void testEmptyFile() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("ChangeRecorderTest")) {
            createChanges();
            recorder.writeReport(tempFile.getPath());

            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(
                    output,
                    containsString(
                            "changeRecorderLog xmlns=\"http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0\""));
            assertThat(output, containsString("<updates goal=\"test\" date="));
            assertThat(
                    output,
                    containsString(
                            "<dependencyUpdate kind=\"dependency-update\" groupId=\"org.codehaus\" artifactId=\"example0\" oldVersion=\"0.0.1\" newVersion=\"0.0.2\"/>"));
            assertThat(
                    output,
                    containsString(
                            "<dependencyUpdate kind=\"dependency-management-update\" groupId=\"org.codehaus\" artifactId=\"example1\" newVersion=\"2.0.0\"/>"));
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
                .withNewVersion("2.0.0"));
    }

    @Test
    void testAppend() throws Exception {
        Path testFile = Paths.get("target/test-classes/org/codehaus/mojo/versions/recording/versions-changes.xml");
        createChanges();
        recorder.writeReport(testFile);

        String output = String.join("", Files.readAllLines(testFile));
        assertThat(
                output,
                containsString(
                        "changeRecorderLog xmlns=\"http://www.mojohaus.org/versions-maven-plugin/schema/updates/3.0\""));
        assertThat(output, containsString("<updates goal=\"previous\" date="));
        assertThat(
                output,
                containsString(
                        "<dependencyUpdate kind=\"dependency-update\" groupId=\"org.codehaus\" artifactId=\"example0\" oldVersion=\"0.0.1-SNAPSHOT\" newVersion=\"0.0.1\"/>"));

        assertThat(output, containsString("<updates goal=\"test\" date="));
        assertThat(
                output,
                containsString(
                        "<dependencyUpdate kind=\"dependency-update\" groupId=\"org.codehaus\" artifactId=\"example0\" oldVersion=\"0.0.1\" newVersion=\"0.0.2\"/>"));
        assertThat(
                output,
                containsString(
                        "<dependencyUpdate kind=\"dependency-management-update\" groupId=\"org.codehaus\" artifactId=\"example1\" newVersion=\"2.0.0\"/>"));
    }

    @Test
    void emptyResultShouldNotGenerateReports() throws Exception {
        Path path = Files.createTempDirectory("ChangeRecorderTest").resolve("ChangeRecorderTest.xml");
        recorder.writeReport(path);
        assertFalse(Files.isRegularFile(path), "File should not be created");
    }
}
