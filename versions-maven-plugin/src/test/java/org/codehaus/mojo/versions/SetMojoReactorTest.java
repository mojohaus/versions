package org.codehaus.mojo.versions;

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

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPathFactory;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests of {@link SetMojo} on a tree of POMs: which POMs it rewrites, by what each of them is selected.
 */
@ProjectMojoTest
class SetMojoReactorTest {
    private static final Path RESOURCES = Paths.get("src/test/resources/org/codehaus/mojo/set");

    private ProjectMojos mojos;

    private Path tempDir;

    @BeforeEach
    void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        tempDir = TestUtils.createTempDir("set-reactor");
    }

    @AfterEach
    void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    private String xpath(String pom, String expression) throws Exception {
        try (InputStream in = Files.newInputStream(tempDir.resolve(pom))) {
            return XPathFactory.newInstance()
                    .newXPath()
                    .evaluate(
                            expression,
                            DocumentBuilderFactory.newInstance()
                                    .newDocumentBuilder()
                                    .parse(in)
                                    .getDocumentElement());
        }
    }

    private void assertVersion(String expected, String pom, String expression) throws Exception {
        assertEquals(expected, xpath(pom, expression), pom + " " + expression);
    }

    /** Wildcards in groupId, artifactId and oldVersion select the modules by artifactId, with their children. */
    @Test
    void testWildcardsSelectModulesByArtifactId() throws Exception {
        TestUtils.copyDir(RESOURCES.resolve("wildcards"), tempDir);
        SetMojo mojo = mojos.lookup("set", tempDir);
        setVariableValueToObject(mojo, "groupId", "*");
        setVariableValueToObject(mojo, "artifactId", "module-a*");
        setVariableValueToObject(mojo, "oldVersion", "*");
        setVariableValueToObject(mojo, "newVersion", "2.0.9-SNAPSHOT");
        setVariableValueToObject(mojo, "updateMatchingVersions", false);
        mojo.execute();

        assertVersion("1.2.0-SNAPSHOT", "pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a1/pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a1/module-b1/pom.xml", "/project/parent/version");
        assertVersion("", "module-a1/module-b1/pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a1/module-b2/pom.xml", "/project/parent/version");
        assertVersion("2.0.7-SNAPSHOT", "module-a1/module-b2/pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a1/module-b3/pom.xml", "/project/parent/version");
        assertVersion("2.0.8-SNAPSHOT", "module-a1/module-b3/pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a2/pom.xml", "/project/version");
        assertVersion("2.0.9-SNAPSHOT", "module-a2/pom.xml", "/project/dependencies/dependency/version");
    }

    /** With processAllModules, the module with a parent and a managed dependency on the project version follows. */
    @Test
    void testProcessAllModulesWithBomChild() throws Exception {
        TestUtils.copyDir(RESOURCES.resolve("issue-198"), tempDir);
        SetMojo mojo = mojos.lookup("set", tempDir);
        setVariableValueToObject(mojo, "oldVersion", "1.0.0-SNAPSHOT");
        setVariableValueToObject(mojo, "newVersion", "1.0.0");
        setVariableValueToObject(mojo, "generateBackupPoms", false);
        setVariableValueToObject(mojo, "processAllModules", true);
        mojo.execute();

        assertVersion("1.0.0", "pom.xml", "/project/version");
        assertVersion("1.0.0", "fake-api/pom.xml", "/project/parent/version");
        assertVersion("1.0.0", "fake-api/pom.xml", "/project/version");
        assertVersion("1.0.0", "the-bom/pom.xml", "/project/version");
    }

    /** Run in a child module with processFromLocalAggregationRoot=false, only that module's tree changes. */
    @Test
    void testChildModuleWithoutLocalAggregationRoot() throws Exception {
        TestUtils.copyDir(RESOURCES.resolve("child-aggregation-root"), tempDir);
        SetMojo mojo = mojos.lookupInReactor("set", tempDir, "module-a1");
        setVariableValueToObject(mojo, "newVersion", "2.5.0-SNAPSHOT");
        setVariableValueToObject(mojo, "processFromLocalAggregationRoot", false);
        mojo.execute();

        assertChildModuleTreeChanged();
        assertVersion("1.0.3-SNAPSHOT", "module-a2/pom.xml", "/project/version");
        assertVersion("2.0.7-SNAPSHOT", "module-a2/pom.xml", "/project/dependencies/dependency/version");
    }

    /** Run in a child module from the local aggregation root, dependencies in the other modules follow too. */
    @Test
    void testChildModuleFromLocalAggregationRoot() throws Exception {
        TestUtils.copyDir(RESOURCES.resolve("child-aggregation-root"), tempDir);
        SetMojo mojo = mojos.lookupInReactor("set", tempDir, "module-a1");
        setVariableValueToObject(mojo, "newVersion", "2.5.0-SNAPSHOT");
        mojo.execute();

        assertChildModuleTreeChanged();
        assertVersion("1.0.3-SNAPSHOT", "module-a2/pom.xml", "/project/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a2/pom.xml", "/project/dependencies/dependency/version");
    }

    private void assertChildModuleTreeChanged() throws Exception {
        assertVersion("1.2.0-SNAPSHOT", "pom.xml", "/project/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/pom.xml", "/project/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/module-b1/pom.xml", "/project/parent/version");
        assertVersion("", "module-a1/module-b1/pom.xml", "/project/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/module-b2/pom.xml", "/project/parent/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/module-b2/pom.xml", "/project/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/module-b3/pom.xml", "/project/parent/version");
        assertVersion("2.5.0-SNAPSHOT", "module-a1/module-b3/pom.xml", "/project/version");
    }
}
