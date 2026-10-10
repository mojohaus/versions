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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.change.DefaultPropertyVersionChange;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Basic tests for {@linkplain UseDepVersionMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
@ProjectMojoTest
public class UseDepVersionMojoTest {
    protected ProjectMojos mojos;

    private Path tempDir;

    private TestChangeRecorder changeRecorder = new TestChangeRecorder();

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        tempDir = TestUtils.createTempDir("use-dep-version");
    }

    @AfterEach
    public void tearDown() throws Exception {
        TestUtils.tearDownTempDir(tempDir);
        changeRecorder.getChanges().clear();
    }

    @Test
    public void testIssue673() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-637"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "serverId", "serverId");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));

        mojo.execute();
    }

    /**
     * Tests a simple case with a single property: the property value needs to be changed.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesSimple() throws Exception {
        Log logger = new SystemStreamLog() {
            @Override
            public boolean isDebugEnabled() {
                return true;
            }
        };
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/simple"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);

        mojo.execute();

        String pom = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(pom, "<version>${revision}</version>");
        assertTextContains(pom, "<revision>2.0.0</revision>");
    }

    /**
     * The same as {@link #testPropertiesSimple()}, but with profiles.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesSimpleProfiles() throws Exception {
        Log logger = new SystemStreamLog() {
            @Override
            public boolean isDebugEnabled() {
                return true;
            }
        };
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/simple-profiles"), tempDir);
        TestChangeRecorder changeRecorder = new TestChangeRecorder();
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String pom = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(pom, "<version>${revision}</version>");
        assertTextContains(pom, "<revision>2.0.0</revision>");
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultPropertyVersionChange("revision", "1.0.0-SNAPSHOT", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * Tests a case with a single property used for more than one dependency, of which only one is to be changed:
     * the property value must remain unchanged, and a warning must be logged.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesConflict() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder warnLog = new StringBuilder();
        doAnswer(i -> warnLog.append(i.getArgument(0).toString())).when(logger).warn(anyString());
        TestChangeRecorder changeRecorder = new TestChangeRecorder();

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/conflict"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        assertEquals(emptyList(), changeRecorder.getChanges());
        assertTextContains(
                warnLog.toString(), "Cannot update property ${revision}: controls more than one dependency: artifactB");
        assertEquals(emptyList(), changeRecorder.getChanges());
    }

    /**
     * Tests a case with a single property used for more than one dependency, of which only one is to be changed:
     * however, the other dependency (not to be changed) uses the redefined value of the property.
     * In this case, the change should take place in the child, but not in the parent.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesConflictRedefinition() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/conflict-redefinition"),
                tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(child, "<version>${revision}</version>");
        assertTextContains(parent, "<version>${revision}</version>");
        assertTextContains(child, "<revision>2.0.0</revision>");
        assertTextContains(parent, "<revision>1.0.0-SNAPSHOT</revision>");
        assertTrue(
                changeRecorder.getChanges().contains(new DefaultPropertyVersionChange("revision", "1.0.1", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * Tests a case with a single property used for more than one dependency, of which only one is to be changed:
     * the dependency to be changed is in the parent, and both the child and the parent redefine the same property.
     * Because the property is redefined at the child level, the child is immune to property changes, hence
     * the substitution must take place.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesConflictCancellation() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/conflict-cancellation"),
                tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(child, "<version>${revision}</version>");
        assertTextContains(parent, "<version>${revision}</version>");
        assertTextContains(parent, "<revision>2.0.0</revision>");
        assertTextContains(child, "<revision>1.0.1</revision>");
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultPropertyVersionChange("revision", "1.0.0-SNAPSHOT", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * The same as {@link #testPropertiesConflictCancellation()}, but working on profiles.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesConflictCancellationProfiles() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/"
                        + "conflict-cancellation-profiles"),
                tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(child, "<version>${revision}</version>");
        assertTextContains(parent, "<version>${revision}</version>");
        assertTextContains(parent, "<revision>2.0.0</revision>");
        assertTextContains(child, "<revision>1.0.1</revision>");
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultPropertyVersionChange("revision", "1.0.0-SNAPSHOT", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * Tests a case with a single property defined in the parent, and used in the child: the property value in
     * the parent needs to be updated.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesChildParent() throws Exception {
        Log logger = new SystemStreamLog() {
            @Override
            public boolean isDebugEnabled() {
                return true;
            }
        };
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/child-parent"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(child, "<version>${revision}</version>");
        assertTextContains(parent, "<revision>2.0.0</revision>");
        assertTrue(
                changeRecorder.getChanges().contains(new DefaultPropertyVersionChange("revision", "1.0.0", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * Tests a case with a single property defined in the parent and then redefined in the child: the property
     * must be redefined in the child and remain the same in the parent.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertiesChildParentRedefinition() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/child-parent-redefinition"),
                tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(child, "<version>${revision}</version>");
        assertTextContains(parent, "<revision>1.0.0-SNAPSHOT</revision>");
        assertTextContains(child, "<revision>2.0.0</revision>");
        assertTrue(
                changeRecorder.getChanges().contains(new DefaultPropertyVersionChange("revision", "1.0.1", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    /**
     * Tests a case with a single property defined in the parent: a warning must be logged and no files must
     * be changed.
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testPropertyFromParent() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder log = new StringBuilder();
        doAnswer(i -> log.append("[WARN] ").append(i.getArgument(0).toString()))
                .when(logger)
                .warn(anyString());
        TestChangeRecorder changeRecorder = new TestChangeRecorder();

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/child-parent"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir.resolve("child"));
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        assertEquals(emptyList(), changeRecorder.getChanges());
        assertTextContains(log.toString(), "[WARN] Not updating property ${revision}: defined in parent");
    }

    @Test
    public void testVersionlessDependency() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder log = new StringBuilder();
        doAnswer(i -> log.append("[WARN] ").append(i.getArgument(0).toString()))
                .when(logger)
                .warn(anyString());
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());
        setVariableValueToObject(mojo, "log", logger);
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "processDependencyManagement", false);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertEquals(emptyList(), changeRecorder.getChanges());
        assertTextContains(
                log.toString(),
                "[WARN] Not updating default-group:artifactA in dependencies: version defined in dependencyManagement");
    }

    @Test
    public void testDependencyManagemenent() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", false);
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange("default-group", "artifactA", "1.0.0", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testVersionDefinedInDependencyManagemenent() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder log = new StringBuilder();
        doAnswer(i -> log.append("[WARN] ").append(i.getArgument(0).toString()))
                .when(logger)
                .warn(anyString());
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", tempDir);
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());
        setVariableValueToObject(mojo, "log", logger);
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertTextContains(
                log.toString(),
                "[WARN] Not updating default-group:artifactA in dependencies: version defined in dependencyManagement");
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange("default-group", "artifactA", "1.0.0", "2.0.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        File projectDir = new File("src/test/resources/org/codehaus/mojo/use-dep-version/problem-causing");
        UseDepVersionMojo mojo = mojos.lookup("use-dep-version", projectDir.toPath());
        try {
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoFailureException e) {
            assertInstanceOf(VersionRetrievalException.class, e.getCause());
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertEquals(
                    "problem-causing-artifact",
                    vre.getArtifact().map(Artifact::getArtifactId).orElse(""));
        }
    }

    /**
     * Tests a regression since 2.15.0 when the mojo is only executed on the project on the module list
     * (that is, if a -pl parameter is used providing a list of modules to process).
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testModuleList() throws Exception {
        Log logger = new SystemStreamLog() {
            @Override
            public boolean isDebugEnabled() {
                return true;
            }
        };
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/module-list"), tempDir);

        UseDepVersionMojo mojo = mojos.lookupInReactor("use-dep-version", tempDir, "mod1", "mod2");
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        mojo.execute();

        String mod1 = String.join("", Files.readAllLines(tempDir.resolve("mod1/pom.xml")));
        String mod2 = String.join("", Files.readAllLines(tempDir.resolve("mod2/pom.xml")));
        assertTextContains(mod1, "<version>2.0.0</version>");
        assertTextContains(mod2, "<version>2.0.0</version>");
        assertEquals(2, changeRecorder.getChanges().size(), () -> String.valueOf(changeRecorder.getChanges()));
    }
}
