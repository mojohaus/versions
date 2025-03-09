package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.InputLocation;
import org.apache.maven.model.InputSource;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.model.TestIgnoreVersions;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.TYPE_REGEX;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.matches;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Basic tests for {@linkplain DisplayDependencyUpdatesMojo}.
 */
public class DisplayDependencyUpdatesMojoTest extends AbstractMojoTestCase {
    @Rule
    public final MojoRule mojoRule = new MojoRule(this);

    @Mock
    private Log log;

    private ArtifactFactory artifactFactory;

    @Mock
    private ExpressionEvaluator expressionEvaluator;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        openMocks(this);
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
    }

    @Test
    public void testValidateGAVListSuccessful() throws MojoExecutionException {
        DisplayDependencyUpdatesMojo.validateGAVList(
                Arrays.asList("group", "group:artifact", "group:artifact:version"), 3, "");
    }

    @Test
    public void testValidateGAVListFailed() {
        try {
            DisplayDependencyUpdatesMojo.validateGAVList(
                    Arrays.asList("group:artifact:version", "group:artifact:version:type"), 3, "");
            fail("Method should have thrown a MojoExecutionException");
        } catch (MojoExecutionException ignored) {
        }
    }

    @Test
    public void testRuleSetPresentAndWorking() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/ruleset"),
                    "display-dependency-updates");
            assertThat(mojo.ruleSet, notNullValue());
            assertThat(mojo.ruleSet.getIgnoreVersions(), notNullValue());
            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            assertThat(
                    mojo.ruleSet.getIgnoreVersions(), hasItem(matches(new TestIgnoreVersions().withVersion("1.0.1"))));
            assertThat(
                    mojo.ruleSet.getIgnoreVersions(),
                    containsInAnyOrder(
                            matches(new TestIgnoreVersions().withVersion("1.0.1")),
                            matches(new TestIgnoreVersions()
                                    .withType(TYPE_REGEX)
                                    .withVersion(".+-SNAPSHOT")),
                            matches(new TestIgnoreVersions()
                                    .withType(TYPE_REGEX)
                                    .withVersion(".+-M\\d+"))));

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("1.1.0-M1"))));
        }
    }

    private MavenProject createProject() {
        return new MavenProject(new Model() {
            {
                setGroupId("default-group");
                setArtifactId("default-artifact");
                setVersion("1.0.0-SNAPSHOT");

                InputLocation fakeLocation = new InputLocation(-1, -1, new InputSource());
                setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("default-dependency")
                        .withVersion("1.0.0")
                        .withLocation(DependencyBuilder.Location.ARTIFACT_ID.toString(), fakeLocation)
                        .withLocation(DependencyBuilder.Location.VERSION.toString(), fakeLocation)
                        .build()));
            }
        }) {
            {
                setOriginalModel(getModel());
            }
        };
    }

    @Test
    public void testVersionsWithQualifiersNotConsideredAsMinorUpdates() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put(
                                    "default-dependency",
                                    new String[] {"1.0.0", "1.1.0", "2.0.0-SNAPSHOT", "2.0.0-beta", "2.0.0-rc1"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowMajorUpdates = false;
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    allowSnapshots = true;
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            assertThat(
                    String.join("", Files.readAllLines(tempFile.getPath())),
                    not(anyOf(
                            containsString("2.0.0-SNAPSHOT"),
                            containsString("2.0.0-beta"),
                            containsString("2.0.0-rc1"))));
        }
    }

    @Test
    public void testAllowMajorUpdatesFalse() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowMajorUpdates = false;
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString("1.1.0"));
            assertThat(output, not(containsString("2.0.0")));
        }
    }

    @Test
    public void testAllowMinorUpdatesFalse() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.0.1", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowMinorUpdates = false;
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString("1.0.1"));
            assertThat(output, not(containsString("1.1.0")));
            assertThat(output, not(containsString("2.0.0")));
        }
    }

    @Test
    public void testAllowIncrementalUpdatesFalse() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowIncrementalUpdates = false;
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString("1.0.0-1"));
            assertThat(output, not(containsString("1.0.1")));
            assertThat(output, not(containsString("1.1.0")));
            assertThat(output, not(containsString("2.0.0")));
        }
    }

    @Test
    public void testVersionsWithQualifiersNotConsideredAsIncrementalUpdates() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put(
                                    "default-dependency",
                                    new String[] {"1.0.0", "1.1.0", "1.9.0-SNAPSHOT", "1.9.0-beta", "1.9.0-rc1"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowMinorUpdates = false;
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    allowSnapshots = true;
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            assertThat(
                    String.join("", Files.readAllLines(tempFile.getPath())),
                    not(anyOf(
                            containsString("1.9.0-SNAPSHOT"),
                            containsString("1.9.0-beta"),
                            containsString("1.9.0-rc1"))));
        }
    }

    @Test
    public void testDetermineUpdatedSegment() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/ruleset"),
                    "display-dependency-updates");

            assertThat(mojo.ruleSet, notNullValue());
            assertThat(mojo.ruleSet.getIgnoreVersions(), notNullValue());
            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            assertThat(
                    mojo.ruleSet.getIgnoreVersions(), hasItem(matches(new TestIgnoreVersions().withVersion("1.0.1"))));
            assertThat(
                    mojo.ruleSet.getIgnoreVersions(),
                    containsInAnyOrder(
                            matches(new TestIgnoreVersions().withVersion("1.0.1")),
                            matches(new TestIgnoreVersions()
                                    .withType(TYPE_REGEX)
                                    .withVersion(".+-SNAPSHOT")),
                            matches(new TestIgnoreVersions()
                                    .withType(TYPE_REGEX)
                                    .withVersion(".+-M\\d+"))));

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("1.1.0-M1"))));
        }
    }

    @Test
    public void testVersionInterpolation() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/version-interpolation"),
                    "display-dependency-updates");

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"2.0.1"});
                }
            });
            mojo.processDependencyManagementTransitive = false;
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("mycomponent.version"))));
        }
    }

    @Test
    public void testAllowSnapshots() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"2.0.0-SNAPSHOT"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    allowSnapshots = true;
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString("2.0.0-SNAPSHOT"));
        }
    }

    private void testAllowUpdatesFromLesserSegments(String availableVersion) throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {availableVersion});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    processDependencies = true;
                    processDependencyManagement = false;
                    dependencyIncludes = singletonList(WildcardMatcher.WILDCARD);
                    dependencyExcludes = emptyList();
                    allowMajorUpdates = false;
                    outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                    mojoExecution = mock(MojoExecution.class);
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString(availableVersion));
        }
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsMinor() throws Exception {
        testAllowUpdatesFromLesserSegments("1.1.0");
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsIncremental() throws Exception {
        testAllowUpdatesFromLesserSegments("1.0.1");
    }

    @Test
    public void testAllowUpdatesFromLesserSegmentsSubIncremental() throws Exception {
        testAllowUpdatesFromLesserSegments("1.0.0-1");
    }

    /*
     * A dependency version is managed by a parent from outside the reactor; with showVersionsless false the output
     * should not be generated at all.
     */
    @Test
    public void testShowVersionlessFalse() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "version-from-parent/child"),
                    "display-dependency-updates");
            mojo.setPluginContext(new HashMap<>());
            mojo.showVersionless = false;
            mojo.processDependencyManagement = false;
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.outputEncoding = Charset.defaultCharset().toString();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "2.0.0"});
                }
            });
            mojo.execute();
            assertThat(Files.exists(tempFile.getPath()), is(false));
        }
    }

    /*
     * A dependency version is managed by a parent from outside the reactor; with showVersionsless true
     * and verbose, the output should contain the location of the versionless dependency.
     */
    @Test
    public void testShowVersionlessVerbose() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "version-from-parent/child"),
                    "display-dependency-updates");
            mojo.setPluginContext(new HashMap<>());
            mojo.showVersionless = true;
            mojo.verbose = true;
            mojo.processDependencyManagement = false;
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.outputEncoding = Charset.defaultCharset().toString();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "2.0.0"});
                }
            });
            mojo.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(
                    output, containsString("1.0.0 (managed by default-group:parent-artifact:1.0.0-SNAPSHOT) -> 2.0.0"));
        }
    }

    /*
     * A dependency version is managed by a parent from outside the reactor; with showVersionsless false the output
     * should be generated and should contain the transitive dependency
     */
    @Test
    public void testShowVersionlessFalseDependencyManagementTransitive() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "dependency-management-from-parent/child"),
                    "display-dependency-updates");
            mojo.setPluginContext(new HashMap<>());
            mojo.showVersionless = false;
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.outputEncoding = Charset.defaultCharset().toString();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "2.0.0"});
                }
            });
            mojo.execute();
            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(output, containsString("1.0.0 -> 2.0.0"));
        }
    }

    /*
     * dependencyManagement in active profiles should be processed with processDependencyManagementTransitive false
     */
    @Test
    public void testProcessDependencyManagementTransitiveWithProfiles() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/" + "profiles"),
                    "display-dependency-updates");
            mojo.setPluginContext(new HashMap<>());
            mojo.processDependencyManagementTransitive = false;
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.outputEncoding = Charset.defaultCharset().toString();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "2.0.0"});
                }
            });
            mojo.execute();
            assertThat(Files.exists(tempFile.getPath()), is(true));
            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(output, containsString("1.0.0 -> 2.0.0"));
        }
    }
}
