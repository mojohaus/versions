package org.codehaus.mojo.versions;

import java.io.File;
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
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.TestLog;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.TestUtils.fixAllProjects;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.matchesRegex;
import static org.hamcrest.Matchers.not;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.openMocks;

public class SetMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Mock
    protected Log log;

    protected PomHelper pomHelper;

    protected ArtifactFactory artifactFactory;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
    }

    @After
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testGetIncrementedVersion() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                assertThat(getIncrementedVersion("1", null), is("2-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0", null), is("1.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0", null), is("1.0.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0.0", null), is("1.0.0.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", null), is("1.0.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 1), is("2.0.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 2), is("1.1.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 3), is("1.0.1-SNAPSHOT"));
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
                    assertThat(e.getMessage(), containsString("nextSnapshotIndexToIncrement cannot be less than 1"));
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
                    assertThat(
                            e.getMessage(),
                            containsString(
                                    "nextSnapshotIndexToIncrement cannot be greater than the last version index"));
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
            assertThat(
                    e.getMessage(),
                    containsString("nextSnapshotIndexToIncrement is not valid when nextSnapshot is false"));
        }
    }

    @Test
    public void testVersionlessDependency() throws Exception {
        SetMojo myMojo = fixAllProjects(mojoRule.lookupConfiguredMojo(
                new File("target/test-classes/org/codehaus/mojo/set/versionless-01"), "set"));
        myMojo.execute();
    }

    @Test
    public void testRemoveSnapshotIdempotency() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/remove-snapshot/pom.xml"),
                Paths.get(tempDir.toString(), "pom.xml"),
                REPLACE_EXISTING);

        SetMojo firstRun = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        firstRun.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>1.0</version>"));

        // no exception should be thrown, the file should stay with version "1.0"
        SetMojo secondRun = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        secondRun.session.getSettings().setInteractiveMode(false);
        secondRun.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>1.0</version>"));
    }

    @Test
    public void testSetOldVersionMismatch() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-794"), tempDir);
        SetMojo mojo = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        setVariableValueToObject(mojo, "oldVersion", "foo");
        setVariableValueToObject(mojo, "newVersion", "bar");
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                not(containsString("<version>bar</version>")));
    }

    @Test
    public void testSetOldVersionMismatchProcessAllModules() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-794"), tempDir);
        SetMojo mojo = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        setVariableValueToObject(mojo, "oldVersion", "foo");
        setVariableValueToObject(mojo, "newVersion", "bar");
        setVariableValueToObject(mojo, "processAllModules", true);
        mojo.execute();
        // with processAllModules, the module *should* be selected for update
        // regardless of the version
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>bar</version>"));
    }

    private void testSetParameterValue(String filename, Consumer<SetMojo>... initializers) throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/issue-855/").resolve(filename),
                tempDir.resolve("pom.xml"));
        SetMojo mojo = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        fixAllProjects(mojo);
        Stream.of(initializers).forEachOrdered(i -> i.accept(mojo));
        mojo.execute();
        String output = String.join("", Files.readAllLines(tempDir.resolve("pom.xml")));
        assertThat(output, containsString("<version>testing</version>"));
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
        SetMojo mojo = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
        fixAllProjects(mojo);
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>testing</version>"));
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("child/pom.xml"))),
                containsString("<version>testing</version>"));
    }

    @Test
    public void testIssue1042() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-1042"), tempDir);
        SetMojo mojo = fixAllProjects(mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set"));
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                matchesPattern(
                        ".*\\Q<artifactId>child-reactor</artifactId>\\E\\s*" + "\\Q<version>1.0</version>\\E.*"));
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("child-webapp/pom.xml"))),
                matchesRegex(".*\\Q<artifactId>child-webapp</artifactId>\\E\\s*" + "\\Q<version>1.0</version>\\E.*"));
    }

    @Test
    public void testIssue1137() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-1137"), tempDir);
        TestLog testLog = new TestLog();
        SetMojo mojo = fixAllProjects(
                mojoRule.lookupConfiguredMojo(tempDir.resolve("child").toFile(), "set"));
        setVariableValueToObject(mojo, "newVersion", "1.1");
        setVariableValueToObject(mojo, "log", testLog);
        mojo.execute();
        assertThat(
                testLog.getLoggedMessages().stream().map(Triple::getMiddle).collect(Collectors.joining("\n")),
                not(containsString("Processing change of null:child")));
    }

    @Test
    public void testNextSnapshotIndexToIncrement() throws MojoExecutionException {
        new SetMojo(artifactFactory, null, null, null, null, null) {
            {
                nextSnapshot = true;
                assertThat(getIncrementedVersion("1.1.1-SNAPSHOT", 1), is("2.0.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.1.1-SNAPSHOT", 2), is("1.2.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.1.1-SNAPSHOT", 3), is("1.1.2-SNAPSHOT"));
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

        MavenSession session = TestUtils.createMavenSession(
                mojoRule.getContainer(), mojoRule::readMavenProject, tempDir, "mod1", "mod2");
        SetMojo mojo = mojoRule.lookupConfiguredMojo(session, newMojoExecution("set"));
        setVariableValueToObject(mojo, "newVersion", "2.0.0");
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem());
        setVariableValueToObject(mojo, "log", log);

        mojo.execute();

        String mod1 = String.join("", Files.readAllLines(tempDir.resolve("mod1/pom.xml")));
        String mod2 = String.join("", Files.readAllLines(tempDir.resolve("mod2/pom.xml")));
        assertThat(mod1, containsString("<version>2.0.0</version>"));
        assertThat(mod2, containsString("<version>2.0.0</version>"));
    }
}
