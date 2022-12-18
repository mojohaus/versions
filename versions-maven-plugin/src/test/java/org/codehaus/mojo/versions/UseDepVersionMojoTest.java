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
import java.util.Collections;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.change.DefaultVersionChange;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;

/**
 * Basic tests for {@linkplain UseDepVersionMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class UseDepVersionMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("use-dep-version");
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
    public void testIssue673() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-637"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "serverId", "serverId");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));

        mojo.execute();
    }

    @Test
    public void testParameters() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-474"), tempDir);
        TestChangeRecorder changeRecorder = new TestChangeRecorder();
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockRepositorySystem());
        setVariableValueToObject(mojo, "aetherRepositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultVersionChange("default-group", "artifactA", "${revision}", "2.0.0")));

        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve(Paths.get("pom.xml")))),
                containsString("<version>2.0.0</version>"));
    }
}
