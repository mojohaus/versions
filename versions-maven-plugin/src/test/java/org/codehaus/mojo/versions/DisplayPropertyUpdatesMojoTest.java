package org.codehaus.mojo.versions;
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
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
import static org.hamcrest.Matchers.not;

/**
 * Unit tests for {@link DisplayPropertyUpdatesMojo}
 */
public class DisplayPropertyUpdatesMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("display-property-updates");
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
        Path tempFile = Files.createTempFile(tempDir, "output", "");

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo = (DisplayPropertyUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "display-property-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.aetherRepositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = true;
        mojo.execute();

        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                matchesPattern(".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*"));
    }

    @Test
    public void testDisablePropertiesFromParent() throws Exception {
        Path tempFile = Files.createTempFile(tempDir, "output", "");

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367"), tempDir);
        DisplayPropertyUpdatesMojo mojo = (DisplayPropertyUpdatesMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "display-property-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.aetherRepositorySystem = mockAetherRepositorySystem();
        mojo.includeParent = false;
        mojo.execute();

        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                not(matchesPattern(".*\\$\\{ver} \\.* 1\\.0\\.0 -> 2\\.0\\.0.*")));
    }
}
