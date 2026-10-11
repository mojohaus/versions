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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.commons.codec.CharEncoding.UTF_8;
import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertAnyLineContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextMatches;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotMatches;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.verify;

/**
 * Unit tests for {@link DisplayPropertyUpdatesMojo}
 */
@ProjectMojoTest
public class DisplayPropertyUpdatesMojoTest {
    protected ProjectMojos mojos;

    private Path tempDir;

    private Path tempFile;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        tempDir = TestUtils.createTempDir("display-property-updates");
        tempFile = Files.createTempFile(tempDir, "output", "");
    }

    @AfterEach
    public void tearDown() throws Exception {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testPropertiesFromParent() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo =
                (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", tempDir.resolve("child"));
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = true;
        mojo.execute();

        assertTextMatches(String.join("", Files.readAllLines(tempFile)), ".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*");
    }

    @Test
    public void testDisablePropertiesFromParent() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo =
                (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", tempDir.resolve("child"));
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = false;
        mojo.execute();

        assertTextNotMatches(
                String.join("", Files.readAllLines(tempFile)), ".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*");
    }

    private void testAllowUpdatesFromLesserSegments(String availableVersion) throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-960"), tempDir);
        DisplayPropertyUpdatesMojo mojo =
                (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", tempDir);
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {availableVersion, "2"});
            }
        });
        mojo.includeParent = false;
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        mojo.execute();

        assertTextContains(String.join("", Files.readAllLines(tempFile)), availableVersion);
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsMinor() throws Exception {
        testAllowUpdatesFromLesserSegments("1.1");
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsIncremental() throws Exception {
        testAllowUpdatesFromLesserSegments("1.0.1");
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsSubIncremental() throws Exception {
        testAllowUpdatesFromLesserSegments("1.0.0-1");
    }

    @Test
    public void testClassifierAndTypeAreInterpolated() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-1409"), tempDir);
        DisplayPropertyUpdatesMojo mojo =
                (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", tempDir);
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "2.0.0"});
            }
        });
        mojo.includeParent = false;
        mojo.execute();

        // Resolver 2 rejects coordinates that still contain ${...}
        verify(mojo.repositorySystem)
                .resolveVersionRange(
                        any(),
                        argThat((VersionRangeRequest request) ->
                                "linux-x86_64".equals(request.getArtifact().getClassifier())
                                        && "jar".equals(request.getArtifact().getExtension())));
        assertTextMatches(String.join("", Files.readAllLines(tempFile)), ".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*");
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        try {
            File projectDir = new File("src/test/resources/org/codehaus/mojo/update-properties/problem-causing");
            DisplayPropertyUpdatesMojo mojo =
                    (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", projectDir);
            mojo.outputEncoding = UTF_8;
            mojo.outputFile = tempFile.toFile();
            mojo.setPluginContext(new HashMap<>());
            mojo.repositorySystem = mockAetherRepositorySystem();
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertInstanceOf(VersionRetrievalException.class, e.getCause());
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertEquals(
                    "problem-causing-artifact",
                    vre.getArtifact().map(Artifact::getArtifactId).orElse(""));
        }
    }

    @Test
    public void testLatestVersion() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/updates-only"), tempDir);
        DisplayPropertyUpdatesMojo mojo =
                (DisplayPropertyUpdatesMojo) mojos.lookup("display-property-updates", tempDir);
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem();
        mojo.execute();

        List<String> output = Files.readAllLines(tempFile);
        assertAnyLineContains(output, "All version properties are referencing the newest version available.");
    }
}
