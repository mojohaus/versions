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
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.InputLocation;
import org.apache.maven.model.InputSource;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.TextAssertions.assertNoLineContains;
import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.TextAssertions.assertTextNotContains;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Basic tests for {@linkplain DisplayDependencyUpdatesMojo}.
 */
@ProjectMojoTest
public class DisplayDependencyUpdatesMojoTest {
    protected ProjectMojos mojos;

    @Mock
    private Log log;

    private ArtifactFactory artifactFactory;

    @Mock
    private ExpressionEvaluator expressionEvaluator;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
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

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    Paths.get("target/test-classes/org/codehaus/mojo/display-dependency-updates/ruleset"));
            assertNotNull(mojo.ruleSet);
            assertNotNull(mojo.ruleSet.getIgnoreVersions());
            assertEquals(
                    3, mojo.ruleSet.getIgnoreVersions().size(), () -> String.valueOf(mojo.ruleSet.getIgnoreVersions()));
            assertEquals(
                    Arrays.asList("exact 1.0.1", "regex .+-M\\d+", "regex .+-SNAPSHOT"),
                    mojo.ruleSet.getIgnoreVersions().stream()
                            .map(v -> v.getType() + " " + v.getVersion())
                            .sorted()
                            .collect(Collectors.toList()));

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertEquals(
                    3, mojo.ruleSet.getIgnoreVersions().size(), () -> String.valueOf(mojo.ruleSet.getIgnoreVersions()));
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertNoLineContains(output, "1.1.0-M1");
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

            assertTextNotContains(
                    String.join("", Files.readAllLines(tempFile.getPath())),
                    "2.0.0-SNAPSHOT",
                    "2.0.0-beta",
                    "2.0.0-rc1");
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

            assertTextContains(output, "1.1.0");
            assertTextNotContains(output, "2.0.0");
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

            assertTextContains(output, "1.0.1");
            assertTextNotContains(output, "1.1.0");
            assertTextNotContains(output, "2.0.0");
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

            assertTextContains(output, "1.0.0-1");
            assertTextNotContains(output, "1.0.1");
            assertTextNotContains(output, "1.1.0");
            assertTextNotContains(output, "2.0.0");
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

            assertTextNotContains(
                    String.join("", Files.readAllLines(tempFile.getPath())),
                    "1.9.0-SNAPSHOT",
                    "1.9.0-beta",
                    "1.9.0-rc1");
        }
    }

    @Test
    public void testDetermineUpdatedSegment() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    Paths.get("target/test-classes/org/codehaus/mojo/display-dependency-updates/ruleset"));

            assertNotNull(mojo.ruleSet);
            assertNotNull(mojo.ruleSet.getIgnoreVersions());
            assertEquals(
                    3, mojo.ruleSet.getIgnoreVersions().size(), () -> String.valueOf(mojo.ruleSet.getIgnoreVersions()));
            assertEquals(
                    Arrays.asList("exact 1.0.1", "regex .+-M\\d+", "regex .+-SNAPSHOT"),
                    mojo.ruleSet.getIgnoreVersions().stream()
                            .map(v -> v.getType() + " " + v.getVersion())
                            .sorted()
                            .collect(Collectors.toList()));

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertEquals(
                    3, mojo.ruleSet.getIgnoreVersions().size(), () -> String.valueOf(mojo.ruleSet.getIgnoreVersions()));
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertNoLineContains(output, "1.1.0-M1");
        }
    }

    @Test
    public void testVersionInterpolation() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    Paths.get(
                            "target/test-classes/org/codehaus/mojo/display-dependency-updates/version-interpolation"));

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
            assertNoLineContains(output, "mycomponent.version");
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

            assertTextContains(output, "2.0.0-SNAPSHOT");
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

            assertTextContains(output, availableVersion);
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
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "version-from-parent/child"));
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
            assertEquals(0, Files.readAllBytes(tempFile.getPath()).length);
        }
    }

    /*
     * A dependency version is managed by a parent from outside the reactor; with showVersionsless true
     * and verbose, the output should contain the location of the versionless dependency.
     */
    @Test
    public void testShowVersionlessVerbose() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "version-from-parent/child"));
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
            assertTextContains(output, "1.0.0 (managed by default-group:parent-artifact:1.0.0-SNAPSHOT) -> 2.0.0");
        }
    }

    /*
     * A dependency version is managed by a parent from outside the reactor; with showVersionsless false the output
     * should be generated and should contain the transitive dependency
     */
    @Test
    public void testShowVersionlessFalseDependencyManagementTransitive() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/"
                            + "dependency-management-from-parent/child"));
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
            assertTextContains(output, "1.0.0 -> 2.0.0");
        }
    }

    /*
     * dependencyManagement in active profiles should be processed with processDependencyManagementTransitive false
     */
    @Test
    public void testProcessDependencyManagementTransitiveWithProfiles() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    Paths.get("target/test-classes/org/codehaus/mojo/display-dependency-updates/" + "profiles"));
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
            assertEquals(true, Files.exists(tempFile.getPath()));
            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertTextContains(output, "1.0.0 -> 2.0.0");
        }
    }

    /*
     * error while attempting a retrieval of a problematic artifact should be reported citing the artifact
     * causing the problem
     */
    @Test
    public void testProblemCausingArtifact() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojos.lookup(
                    "display-dependency-updates",
                    Paths.get("src/test/resources/org/codehaus/mojo/display-dependency-updates/problem-causing"));
            mojo.setPluginContext(new HashMap<>());
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.outputEncoding = Charset.defaultCharset().toString();
            mojo.repositorySystem = mockAetherRepositorySystem();
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertInstanceOf(VersionRetrievalException.class, e.getCause());
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertEquals(
                    "problem-causing-artifact",
                    vre.getArtifact().map(Artifact::getArtifactId).orElse(""));
        }
    }

    @Test
    public void testOverwriteOutput() throws Exception {
        testOutputFileOverwrite(true);
    }

    @Test
    public void testOverwriteOutputFalse() throws Exception {
        testOutputFileOverwrite(false);
    }

    private void testOutputFileOverwrite(boolean overwrite) throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            for (int i = 0; i < 2; ++i) {
                new DisplayDependencyUpdatesMojo(
                        artifactFactory,
                        mockAetherRepositorySystem(new HashMap<String, String[]>() {
                            {
                                put("default-dependency", new String[] {"1.0.0", "2.0.0"});
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
                        outputFile = tempFile.getPath().toFile();
                        overwriteOutput = overwrite;
                        setPluginContext(new HashMap<>());

                        session = mockMavenSession();
                        mojoExecution = mock(MojoExecution.class);
                    }
                }.execute();
            }

            List<String> output = Files.readAllLines(tempFile.getPath());
            assertEquals(
                    overwrite ? 1L : 2L,
                    output.stream()
                            .filter(line -> line.contains("1.0.0 -> 2.0.0"))
                            .count(),
                    output + " should contain 2 strings matching 1.0.0 -> 2.0.0");
        }
    }

    private static Dependency createManagedDependency(
            Dependency prototype, String version, String scope, String classifier) {
        return DependencyBuilder.newBuilder()
                .withGroupId(prototype.getGroupId())
                .withArtifactId(prototype.getArtifactId())
                .withVersion(version)
                .withScope(scope)
                .withClassifier(classifier)
                .build();
    }

    @Test
    public void testDependenciesMatchTestScope() {
        Dependency dependency = DependencyBuilder.newBuilder()
                .withGroupId("default-group")
                .withArtifactId("default-artifact")
                .withVersion("1.0.0")
                .withScope("compile")
                .withClassifier("sources")
                .build();

        assertTrue(DisplayDependencyUpdatesMojo.dependenciesMatch(
                dependency, createManagedDependency(dependency, null, null, null)));
        assertTrue(DisplayDependencyUpdatesMojo.dependenciesMatch(
                dependency, createManagedDependency(dependency, "1.0.0", null, null)));
        assertTrue(DisplayDependencyUpdatesMojo.dependenciesMatch(
                dependency, createManagedDependency(dependency, null, "compile", null)));
        assertTrue(DisplayDependencyUpdatesMojo.dependenciesMatch(
                dependency, createManagedDependency(dependency, null, null, "sources")));
        assertTrue(DisplayDependencyUpdatesMojo.dependenciesMatch(
                dependency, createManagedDependency(dependency, null, null, "sources")));
    }

    @Test
    public void testShouldOnlyShowUpdates() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    artifactFactory,
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"0.0.1", "0.0.2", "1.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    allowMajorUpdates = true;
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

            assertTextNotContains(String.join("", Files.readAllLines(tempFile.getPath())), "1.0.0");
        }
    }

    /*
     * Regression guard: in case a dependency artifact has no "immediate" version defined, which can happen if the version
     * is defined as a restriction, ArtifactVersions::filter would still call the IgnoreVersionHelper on the current
     * version, which is null, causing a NullPointerException.
     */
    @Test
    public void testIgnoredVersionsWithNoCurrentVersion() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {

            DisplayDependencyUpdatesMojo mojo = mojos.lookup(
                    "display-dependency-updates",
                    Paths.get("target/test-classes/org/codehaus/mojo/display-dependency-updates/ranges"));
            mojo.ignoredVersions = Collections.singleton("Alpha");
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.setPluginContext(new HashMap<>());
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0"});
                }
            });

            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertNoLineContains(output, "1.0.0");
        }
    }
}
