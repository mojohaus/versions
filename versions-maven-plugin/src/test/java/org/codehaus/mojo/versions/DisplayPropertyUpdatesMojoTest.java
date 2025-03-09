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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.apache.commons.codec.CharEncoding.UTF_8;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.not;

/**
 * Unit tests for {@link DisplayPropertyUpdatesMojo}
 */
public class DisplayPropertyUpdatesMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    private Path tempFile;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("display-property-updates");
        tempFile = Files.createTempFile(tempDir, "output", "");
    }

    @After
    public void tearDown() throws Exception {
        try {
            TestUtils.tearDownTempDir(tempDir);
        } finally {
            super.tearDown();
        }
    }

    @Test
    public void testPropertiesFromParent() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo = (DisplayPropertyUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "display-property-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = true;
        mojo.execute();

        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                matchesPattern(".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*"));
    }

    @Test
    public void testDisablePropertiesFromParent() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo = (DisplayPropertyUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "display-property-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = false;
        mojo.execute();

        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                not(matchesPattern(".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*")));
    }

    private void testAllowUpdatesFromLesserSegments(String availableVersion) throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-960"), tempDir);
        DisplayPropertyUpdatesMojo mojo = (DisplayPropertyUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.toFile(), "display-property-updates");
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

        assertThat(String.join("", Files.readAllLines(tempFile)), containsString(availableVersion));
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
    public void testProblemCausingArtifact() throws Exception {
        try {
            File projectDir = new File("src/test/resources/org/codehaus/mojo/update-properties/problem-causing");
            DisplayPropertyUpdatesMojo mojo =
                    (DisplayPropertyUpdatesMojo) mojoRule.lookupConfiguredMojo(projectDir, "display-property-updates");
            mojo.outputEncoding = UTF_8;
            mojo.outputFile = tempFile.toFile();
            mojo.setPluginContext(new HashMap<>());
            mojo.repositorySystem = mockAetherRepositorySystem();
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertThat(e.getCause(), instanceOf(VersionRetrievalException.class));
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertThat(vre.getArtifact().map(Artifact::getArtifactId).orElse(""), equalTo("problem-causing-artifact"));
        }
    }
}
