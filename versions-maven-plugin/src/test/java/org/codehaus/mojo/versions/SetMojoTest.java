package org.codehaus.mojo.versions;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.matchesRegex;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

public class SetMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("set");
    }

    @After
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    @Test
    public void testGetIncrementedVersion() throws MojoExecutionException {
        new SetMojo(null, null, null, null, null, null) {
            {
                assertThat(getIncrementedVersion("1.0.0", null), is("1.0.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", null), is("1.0.1-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 1), is("2.0.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 2), is("1.1.0-SNAPSHOT"));
                assertThat(getIncrementedVersion("1.0.0-SNAPSHOT", 3), is("1.0.1-SNAPSHOT"));
            }
        };
    }

    @Test
    public void testNextSnapshotIndexLowerBound() {
        new SetMojo(null, null, null, null, null, null) {
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
    public void testNextSnapshotIndexUpperBound() {
        new SetMojo(null, null, null, null, null, null) {
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
            new SetMojo(null, null, null, null, null, null) {
                {
                    project = new MavenProject();
                    project.setParent(new MavenProject());
                    project.setOriginalModel(new Model());
                    project.getOriginalModel().setVersion("1.2.3-SNAPSHOT");

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
        SetMojo myMojo = (SetMojo) mojoRule.lookupConfiguredMojo(
                new File("target/test-classes/org/codehaus/mojo/set/versionless-01"), "set");
        myMojo.execute();
    }

    @Test
    public void testRemoveSnapshotIdempotency() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/set/remove-snapshot/pom.xml"),
                Paths.get(tempDir.toString(), "pom.xml"),
                REPLACE_EXISTING);

        SetMojo firstRun = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
        firstRun.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>1.0</version>"));

        // no exception should be thrown, the file should stay with version "1.0"
        SetMojo secondRun = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
        secondRun.session.getSettings().setInteractiveMode(false);
        secondRun.execute();
        assertThat(
                String.join("", Files.readAllLines(tempDir.resolve("pom.xml"))),
                containsString("<version>1.0</version>"));
    }

    @Test
    public void testSetOldVersionMismatch() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set/issue-794"), tempDir);
        SetMojo mojo = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
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
        SetMojo mojo = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
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
        SetMojo mojo = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
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
        SetMojo mojo = (SetMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "set");
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
    public void testComputeChildren() {
        Map<Pair<String, String>, Set<Model>> children = SetMojo.computeChildren(new HashMap<File, Model>() {
            {
                put(new File("parent"), new Model() {
                    {
                        setArtifactId("parent");
                        setParent(new Parent() {
                            {
                                setGroupId("default");
                                setArtifactId("superParent");
                            }
                        });
                    }
                });
                put(new File("child2"), new Model() {
                    {
                        setArtifactId("child2");
                        setParent(new Parent() {
                            {
                                setGroupId("default");
                                setArtifactId("superParent");
                            }
                        });
                    }
                });
                put(new File("superParent"), new Model() {
                    {
                        setArtifactId("superParent");
                    }
                });
                put(new File("child"), new Model() {
                    {
                        setArtifactId("child");
                        setParent(new Parent() {
                            {
                                setGroupId("default");
                                setArtifactId("parent");
                            }
                        });
                    }
                });
            }
        });
        assertThat(children.get(new ImmutablePair<>("default", "superParent")), hasSize(2));
        assertThat(children.get(new ImmutablePair<>("default", "parent")), hasSize(1));
        assertThat(children.get(new ImmutablePair<>("default", "child")), is(nullValue()));
    }
}
