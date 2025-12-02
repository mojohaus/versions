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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.matchesPattern;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
public class SetScmTagMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Mock
    protected Log log;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
    }

    @After
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testNewScmValues() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-scm-tag/new-scm-values"), tempDir);
        SetScmTagMojo mojo = mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set-scm-tag");
        TestUtils.fixAllProjects(mojo);
        mojo.execute();
        String output =
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))).replaceAll("\\s*", "");
        assertThat(
                output,
                allOf(
                        matchesPattern(".*<scm>.*<tag>\\s*newTag\\s*</tag>.*</scm>.*"),
                        matchesPattern(".*<scm>.*<url>\\s*url\\s*</url>.*</scm>.*"),
                        matchesPattern(".*<scm>.*<connection>\\s*connection\\s*</connection>.*</scm>.*"),
                        matchesPattern(".*<scm>.*<developerConnection>\\s*"
                                + "developerConnection\\s*</developerConnection>.*</scm>.*")));
    }

    /**
     * Tests against a case where {@link SetScmTagMojo} is executed on a module project list
     * (that is, if a -pl parameter is used providing a list of modules to process).
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testModuleList() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-scm-tag/module-list"), tempDir);

        MavenSession session = TestUtils.createMavenSession(
                mojoRule.getContainer(), mojoRule::readMavenProject, tempDir, "mod1", "mod2");
        SetScmTagMojo mojo = mojoRule.lookupConfiguredMojo(session, newMojoExecution("set-scm-tag"));
        setVariableValueToObject(mojo, "newTag", "newTag");
        setVariableValueToObject(mojo, "connection", "connection");
        setVariableValueToObject(mojo, "developerConnection", "developerConnection");
        setVariableValueToObject(mojo, "url", "url");
        mojo.execute();
        for (String project : Arrays.asList("mod1", "mod2")) {
            Path pomFile = tempDir.resolve(project).resolve("pom.xml");
            String output = String.join("", Files.readAllLines(pomFile)).replaceAll("\\s*", "");
            assertThat(
                    output,
                    allOf(
                            matchesPattern(".*<scm>.*<tag>\\s*newTag\\s*</tag>.*</scm>.*"),
                            matchesPattern(".*<scm>.*<url>\\s*url\\s*</url>.*</scm>.*"),
                            matchesPattern(".*<scm>.*<connection>\\s*connection\\s*</connection>.*</scm>.*"),
                            matchesPattern(".*<scm>.*<developerConnection>\\s*"
                                    + "developerConnection\\s*</developerConnection>.*</scm>.*")));
        }
    }
}
