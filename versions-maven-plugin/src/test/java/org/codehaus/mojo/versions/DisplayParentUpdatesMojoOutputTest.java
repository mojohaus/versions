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
import java.util.HashMap;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.apache.commons.codec.CharEncoding.UTF_8;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.matchesPattern;

/**
 * Unit tests for {@link DisplayPropertyUpdatesMojo}
 */
public class DisplayParentUpdatesMojoOutputTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    private Path tempFile;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("display-parent-updates");
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
    public void testPluginLoads() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/display-parent-updates"), tempDir);
        DisplayParentUpdatesMojo mojo = (DisplayParentUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("no-parent").toFile(), "display-parent-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.aetherRepositorySystem = mockAetherRepositorySystem();
        // mojo.includeParent = true;
        mojo.execute();

        assertThat(String.join("", Files.readAllLines(tempFile)), matchesPattern("Project does not have a parent\\."));
    }

    @Test
    public void testDisplayParentUpdate() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/display-parent-updates"), tempDir);
        DisplayParentUpdatesMojo mojo = (DisplayParentUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("parent").toFile(), "display-parent-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.aetherRepositorySystem = mockAetherRepositorySystem();
        // mojo.includeParent = true;
        mojo.execute();

        // assertThat(String.join("", Files.readAllLines(tempFile)), matchesPattern("Project does not have a
        // parent\\."));
    }
}