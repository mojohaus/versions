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
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.model.PropertyVersionChange;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.codehaus.mojo.versions.utils.TestVersionChangeRecorder;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Basic tests for {@linkplain UseDepVersionMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class UseDepVersionMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    private TestVersionChangeRecorder changeRecorder = new TestVersionChangeRecorder();

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("use-dep-version");
    }

    @After
    public void tearDown() throws Exception {
        try {
            TestUtils.tearDownTempDir(tempDir);
            changeRecorder.getChanges().clear();
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);

        mojo.execute();

        String pom = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(pom, containsString("<version>${revision}</version>"));
        assertThat(pom, containsString("<revision>2.0.0</revision>"));
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
        TestVersionChangeRecorder changeRecorder = new TestVersionChangeRecorder();
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String pom = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(pom, containsString("<version>${revision}</version>"));
        assertThat(pom, containsString("<revision>2.0.0</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.0-SNAPSHOT")
                        .withNewValue("2.0.0")));
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
        TestVersionChangeRecorder changeRecorder = new TestVersionChangeRecorder();

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/conflict"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        setVariableValueToObject(mojo, "log", logger);

        mojo.execute();

        assertThat(changeRecorder.getChanges(), empty());
        assertThat(
                warnLog.toString(),
                containsString("Cannot update property ${revision}: controls more than one dependency: artifactB"));
        assertThat(changeRecorder.getChanges(), empty());
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(child, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<version>${revision}</version>"));
        assertThat(child, containsString("<revision>2.0.0</revision>"));
        assertThat(parent, containsString("<revision>1.0.0-SNAPSHOT</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.1")
                        .withNewValue("2.0.0")));
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(child, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<revision>2.0.0</revision>"));
        assertThat(child, containsString("<revision>1.0.1</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.0-SNAPSHOT")
                        .withNewValue("2.0.0")));
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(child, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<revision>2.0.0</revision>"));
        assertThat(child, containsString("<revision>1.0.1</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.0-SNAPSHOT")
                        .withNewValue("2.0.0")));
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", logger);
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(child, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<revision>2.0.0</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.0")
                        .withNewValue("2.0.0")));
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
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);

        mojo.execute();

        String child = String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml")));
        String parent = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(child, containsString("<version>${revision}</version>"));
        assertThat(parent, containsString("<revision>1.0.0-SNAPSHOT</revision>"));
        assertThat(child, containsString("<revision>2.0.0</revision>"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new PropertyVersionChange()
                        .withProperty("revision")
                        .withOldValue("1.0.1")
                        .withNewValue("2.0.0")));
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
        TestVersionChangeRecorder changeRecorder = new TestVersionChangeRecorder();

        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/properties/child-parent"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo)
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        setVariableValueToObject(mojo, "log", logger);

        mojo.execute();

        assertThat(changeRecorder.getChanges(), empty());
        assertThat(log.toString(), containsString("[WARN] Not updating property ${revision}: defined in parent"));
    }

    @Test
    public void testVersionlessDependency() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder log = new StringBuilder();
        doAnswer(i -> log.append("[WARN] ").append(i.getArgument(0).toString()))
                .when(logger)
                .warn(anyString());
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        setVariableValueToObject(mojo, "log", logger);
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "processDependencyManagement", false);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertThat(changeRecorder.getChanges(), empty());
        assertThat(
                log.toString(),
                containsString(
                        "[WARN] Not updating default-group:artifactA in dependencies: version defined in dependencyManagement"));
    }

    @Test
    public void testDependencyManagemenent() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", false);
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DependencyVersionChange()
                        .withKind(DependencyChangeKind.DEPENDENCY_MANAGEMENT_UPDATE)
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withOldVersion("1.0.0")
                        .withNewVersion("2.0.0")));
    }

    @Test
    public void testVersionDefinedInDependencyManagemenent() throws Exception {
        Log logger = mock(Log.class);
        StringBuilder log = new StringBuilder();
        doAnswer(i -> log.append("[WARN] ").append(i.getArgument(0).toString()))
                .when(logger)
                .warn(anyString());
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-dep-version/issue-925"), tempDir);
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "use-dep-version");
        setVariableValueToObject(mojo, "reactorProjects", Collections.singletonList(mojo.getProject()));
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "changeRecorder", changeRecorder);
        setVariableValueToObject(mojo, "log", logger);
        mojo.depVersion = "2.0.0";
        mojo.forceVersion = true;
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:artifactA"});

        mojo.execute();

        assertThat(
                log.toString(),
                containsString(
                        "[WARN] Not updating default-group:artifactA in dependencies: version defined in dependencyManagement"));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DependencyVersionChange()
                        .withKind(DependencyChangeKind.DEPENDENCY_MANAGEMENT_UPDATE)
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withOldVersion("1.0.0")
                        .withNewVersion("2.0.0")));
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        File projectDir = new File("src/test/resources/org/codehaus/mojo/use-dep-version/problem-causing");
        UseDepVersionMojo mojo = (UseDepVersionMojo) mojoRule.lookupConfiguredMojo(projectDir, "use-dep-version");
        try {
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoFailureException e) {
            assertThat(e.getCause(), instanceOf(VersionRetrievalException.class));
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertThat(vre.getArtifact().map(Artifact::getArtifactId).orElse(""), equalTo("problem-causing-artifact"));
        }
    }
}
