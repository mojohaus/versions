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

import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextMatches;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
@ProjectMojoTest
public class SetScmTagMojoTest {
    protected ProjectMojos mojos;

    private Path tempDir;

    @Mock
    protected Log log;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
    }

    @AfterEach
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testNewScmValues() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-scm-tag/new-scm-values"), tempDir);
        SetScmTagMojo mojo = mojos.lookup("set-scm-tag", tempDir);
        mojo.execute();
        String output =
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))).replaceAll("\\s*", "");
        assertTextMatches(output, ".*<scm>.*<tag>\\s*newTag\\s*</tag>.*</scm>.*");
        assertTextMatches(output, ".*<scm>.*<url>\\s*url\\s*</url>.*</scm>.*");
        assertTextMatches(output, ".*<scm>.*<connection>\\s*connection\\s*</connection>.*</scm>.*");
        assertTextMatches(
                output,
                ".*<scm>.*<developerConnection>\\s*" + "developerConnection\\s*</developerConnection>.*</scm>.*");
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

        SetScmTagMojo mojo = mojos.lookupInReactor("set-scm-tag", tempDir, "mod1", "mod2");
        setVariableValueToObject(mojo, "newTag", "newTag");
        setVariableValueToObject(mojo, "connection", "connection");
        setVariableValueToObject(mojo, "developerConnection", "developerConnection");
        setVariableValueToObject(mojo, "url", "url");
        mojo.execute();
        for (String project : Arrays.asList("mod1", "mod2")) {
            Path pomFile = tempDir.resolve(project).resolve("pom.xml");
            String output = String.join("", Files.readAllLines(pomFile)).replaceAll("\\s*", "");
            assertTextMatches(output, ".*<scm>.*<tag>\\s*newTag\\s*</tag>.*</scm>.*");
            assertTextMatches(output, ".*<scm>.*<url>\\s*url\\s*</url>.*</scm>.*");
            assertTextMatches(output, ".*<scm>.*<connection>\\s*connection\\s*</connection>.*</scm>.*");
            assertTextMatches(
                    output,
                    ".*<scm>.*<developerConnection>\\s*" + "developerConnection\\s*</developerConnection>.*</scm>.*");
        }
    }
}
