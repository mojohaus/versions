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

import java.io.File;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.model.TestIgnoreVersions;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.TYPE_REGEX;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.matches;
import static org.codehaus.mojo.versions.utils.MockUtils.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Basic tests for {@linkplain DisplayDependencyUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayDependencyUpdatesMojoTest extends AbstractMojoTestCase {
    @Rule
    public final MojoRule mojoRule = new MojoRule(this);

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
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
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

                setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("default-dependency")
                        .withVersion("1.0.0")
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
                    mockRepositorySystem(),
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
                    setVariableValueToObject(this, "allowAnyUpdates", false);
                    setVariableValueToObject(this, "allowMajorUpdates", false);
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.allowSnapshots = true;
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
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
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "allowAnyUpdates", false);
                    setVariableValueToObject(this, "allowMajorUpdates", false);
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
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
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.0.1", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "allowAnyUpdates", false);
                    setVariableValueToObject(this, "allowMinorUpdates", false);
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
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
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.0.0-1", "1.0.1", "1.1.0", "2.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "allowAnyUpdates", false);
                    setVariableValueToObject(this, "allowIncrementalUpdates", false);
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
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
                    mockRepositorySystem(),
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
                    setVariableValueToObject(this, "allowAnyUpdates", false);
                    setVariableValueToObject(this, "allowMinorUpdates", false);
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.allowSnapshots = true;
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
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
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
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
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"2.0.1"});
                }
            });
            setVariableValueToObject(mojo, "processDependencyManagementTransitive", false);
            mojo.execute();
            List<String> output = Files.readAllLines(tempFile.getPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("mycomponent.version"))));
        }
    }

    @Test
    public void testAllowSnapshots() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"1.0.0", "1.0.1-SNAPSHOT"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.allowSnapshots = true;
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));

            assertThat(output, containsString("1.0.1-SNAPSHOT"));
        }
    }

    /**
     * With {@code allowMajorUpdates}, {@code allowMinorUpdates}, or {@code allowIncrementalUpdates} all equal
     * {@code true}, {@code allowAnyUpdates} should be respected.
     */
    @Test
    public void testAllowAnyUpdatesTrue() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"2.0.0", "1.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.outputFile = tempFile.getPath().toFile();
                    setPluginContext(new HashMap<>());
                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(output, containsString("2.0.0"));
        }
    }

    /**
     * Setting {@code allowMajorUpdates}, {@code allowMinorUpdates}, or {@code allowIncrementalUpdates} to {@code false}
     * should also set {@code allowAnyUpdates} to {@code false}
     */
    @Test
    public void testAllowAnyUpdatesFalseIfOtherAreSet() throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            new DisplayDependencyUpdatesMojo(
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(new HashMap<String, String[]>() {
                        {
                            put("default-dependency", new String[] {"2.0.0", "1.0.0"});
                        }
                    }),
                    null,
                    null) {
                {
                    setProject(createProject());
                    setVariableValueToObject(this, "processDependencies", true);
                    setVariableValueToObject(this, "dependencyIncludes", singletonList(WildcardMatcher.WILDCARD));
                    setVariableValueToObject(this, "dependencyExcludes", emptyList());
                    this.outputFile = tempFile.getPath().toFile();
                    setVariableValueToObject(this, "allowMajorUpdates", false);
                    setPluginContext(new HashMap<>());
                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempFile.getPath()));
            assertThat(output, not(containsString("2.0.0")));
        }
    }
}
