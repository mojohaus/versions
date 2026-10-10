package org.codehaus.mojo.versions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Triple;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.TestLog;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.TextAssertions.assertTextMatches;
import static org.codehaus.mojo.versions.TextAssertions.assertTextNotContains;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.openMocks;

@ProjectMojoTest
public class SetMojoTest {
    protected ProjectMojos mojos;

    private Path tempDir;

    @Mock
    protected Log log;

    protected PomHelper pomHelper;

    protected ArtifactFactory artifactFactory;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
    }

    @AfterEach
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testGetIncrementedVersion() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                assertEquals("2-SNAPSHOT", getIncrementedVersion("1", null));
                assertEquals("1.1-SNAPSHOT", getIncrementedVersion("1.0", null));
                assertEquals("1.0.1-SNAPSHOT", getIncrementedVersion("1.0.0", null));
                assertEquals("1.0.0.1-SNAPSHOT", getIncrementedVersion("1.0.0.0", null));
                assertEquals("1.0.1-SNAPSHOT", getIncrementedVersion("1.0.0-SNAPSHOT", null));
                assertEquals("2.0.0-SNAPSHOT", getIncrementedVersion("1.0.0-SNAPSHOT", 1));
                assertEquals("1.1.0-SNAPSHOT", getIncrementedVersion("1.0.0-SNAPSHOT", 2));
                assertEquals("1.0.1-SNAPSHOT", getIncrementedVersion("1.0.0-SNAPSHOT", 3));
            }
        };
    }

    @Test
    public void testNextSnapshotIndexLowerBound() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                try {
                    getIncrementedVersion("1.0.0", 0);
                    fail();
                } catch (MojoExecutionException e) {
                    assertTextContains(e.getMessage(), "nextSnapshotIndexToIncrement cannot be less than 1");
                }
            }
        };
    }

    @Test
    public void testNextSnapshotIndexUpperBound() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                try {
                    getIncrementedVersion("1.0.0", 4);
                    fail();
                } catch (MojoExecutionException e) {
                    assertTextContains(
                            e.getMessage(),
                            "nextSnapshotIndexToIncrement cannot be greater than the last version index");
                }
            }
        };
    }

    @Test
    public void testNextSnapshotIndexWithoutNextSnapshot() throws MojoFailureException {
        try {
            new SetMojo(artifactFactory, null, null, null, null, null) {
                {
                    project = new MavenProject();
                    project.setParent(new MavenProject());
                    project.setOriginalModel(new Model());
                    project.getOriginalModel().setVersion("1.2.3-SNAPSHOT");
                    session = mock(MavenSession.class);
                    doReturn(Collections.singletonList(project)).when(session).getProjects();
                    nextSnapshotIndexToIncrement = 4;
                }
            }.execute();
        } catch (MojoExecutionException e) {
            assertTextContains(e.getMessage(), "nextSnapshotIndexToIncrement is not valid when nextSnapshot is false");
        }
    }

    @Test
    public void testVersionlessDependency() throws Exception {
        SetMojo myMojo = mojos.lookup("set", Paths.get("target/test-classes/org/codehaus/mojo/set/versionless-01"));
        myMojo.execute();
    }

    @Test
    public void testRemoveSnapshotIdempotency() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/remove-snapshot/pom.xml"),
                Paths.get(tempDir.toString(), "pom.xml"),
                REPLACE_EXISTING);

        SetMojo firstRun = mojos.lookup("set", tempDir);
        firstRun.execute();
        assertTextContains(String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))), "<version>1.0</version>");

        // no exception should be thrown, the file should stay with version "1.0"
        SetMojo secondRun = mojos.lookup("set", tempDir);
        secondRun.session.getSettings().setInteractiveMode(false);
        secondRun.execute();
        assertTextContains(String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))), "<version>1.0</version>");
    }

    @Test
    public void testSetOldVersionMismatch() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-794"), tempDir);
        SetMojo mojo = mojos.lookup("set", tempDir);
        setVariableValueToObject(mojo, "oldVersion", "foo");
        setVariableValueToObject(mojo, "newVersion", "bar");
        mojo.execute();
        assertTextNotContains(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))), "<version>bar</version>");
    }

    @Test
    public void testSetOldVersionMismatchProcessAllModules() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-794"), tempDir);
        SetMojo mojo = mojos.lookup("set", tempDir);
        setVariableValueToObject(mojo, "oldVersion", "foo");
        setVariableValueToObject(mojo, "newVersion", "bar");
        setVariableValueToObject(mojo, "processAllModules", true);
        mojo.execute();
        // with processAllModules, the module *should* be selected for update
        // regardless of the version
        assertTextContains(String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))), "<version>bar</version>");
    }

    private void testSetParameterValue(String filename, Consumer<SetMojo>... initializers) throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/issue-855/").resolve(filename),
                tempDir.resolve("pom.xml"));
        SetMojo mojo = mojos.lookup("set", tempDir);
        Stream.of(initializers).forEachOrdered(i -> i.accept(mojo));
        mojo.execute();
        String output = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertTextContains(output, "<version>testing</version>");
    }

    @Test
    public void testSetParameterValueSimple() throws Exception {
        testSetParameterValue("pom-simple.xml");
    }

    @Test
    public void testSetParameterValueBuildNumber() throws Exception {
        testSetParameterValue("pom-build-number.xml");
    }

    @Test
    public void testSetParameterValueUndefined() throws Exception {
        testSetParameterValue("pom-undefined.xml");
    }

    @Test
    public void testSetParameterValueMultipleProps() throws Exception {
        testSetParameterValue("pom-multiple-props.xml");
    }

    @Test
    public void testSetParameterValuePartial() throws Exception {
        testSetParameterValue("pom-revision.xml");
    }

    @Test
    public void testParentWithProperty() throws Exception {
        TestUtils.copyDir(
                Paths.get("src/test/resources/org/codehaus/mojo/set/issue-855/parent-with-property"), tempDir);
        SetMojo mojo = (SetMojo) mojos.lookup("set", tempDir);
        mojo.execute();
        assertTextContains(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))), "<version>testing</version>");
        assertTextContains(
                String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml"))), "<version>testing</version>");
    }

    @Test
    public void testIssue1042() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-1042"), tempDir);
        SetMojo mojo = mojos.lookup("set", tempDir);
        mojo.execute();
        assertTextMatches(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                ".*\\Q<artifactId>child-reactor</artifactId>\\E\\s*" + "\\Q<version>1.0</version>\\E.*");
        assertTextMatches(
                String.join("", Files.readAllLines(tempDir.resolve("child-webapp/pom.xml"))),
                ".*\\Q<artifactId>child-webapp</artifactId>\\E\\s*" + "\\Q<version>1.0</version>\\E.*");
    }

    @Test
    public void testIssue1137() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-1137"), tempDir);
        TestLog testLog = new TestLog();
        SetMojo mojo = mojos.lookup("set", tempDir.resolve("child"));
        setVariableValueToObject(mojo, "newVersion", "1.1");
        setVariableValueToObject(mojo, "log", testLog);
        mojo.execute();
        assertTextNotContains(
                testLog.getLoggedMessages().stream().map(Triple::getMiddle).collect(Collectors.joining("\n")),
                "Processing change of null:child");
    }

    @Test
    public void testNextSnapshotIndexToIncrement() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                nextSnapshot = true;
                assertEquals("2.0.0-SNAPSHOT", getIncrementedVersion("1.1.1-SNAPSHOT", 1));
                assertEquals("1.2.0-SNAPSHOT", getIncrementedVersion("1.1.1-SNAPSHOT", 2));
                assertEquals("1.1.2-SNAPSHOT", getIncrementedVersion("1.1.1-SNAPSHOT", 3));
            }
        };
    }

    /**
     * Tests against a case where {@link SetMojo} is executed on a module project list
     * (that is, if a -pl parameter is used providing a list of modules to process).
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testModuleList() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/module-list"), tempDir);

        SetMojo mojo = mojos.lookupInReactor("set", tempDir, "mod1", "mod2");
        setVariableValueToObject(mojo, "newVersion", "2.0.0");
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", log);

        mojo.execute();

        String mod1 = String.join("", Files.readAllLines(tempDir.resolve("mod1/pom.xml")));
        String mod2 = String.join("", Files.readAllLines(tempDir.resolve("mod2/pom.xml")));
        assertTextContains(mod1, "<version>2.0.0</version>");
        assertTextContains(mod2, "<version>2.0.0</version>");
    }
}
