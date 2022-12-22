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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.codehaus.mojo.versions.model.TestIgnoreVersions;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.TYPE_REGEX;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.matches;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

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
        File outputFile = null;
        try {
            outputFile = File.createTempFile("display-dependency-updates", "");
            assert outputFile.exists();

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
            mojo.outputFile = outputFile;
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            mojo.execute();
            List<String> output = Files.readAllLines(outputFile.toPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("1.1.0-M1"))));
        } finally {
            assert outputFile == null || !outputFile.exists() || outputFile.delete();
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
    public void testVersionsWithQualifiersNotConsideredAsMinorUpdates()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            final File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            assertThat(
                    String.join("", Files.readAllLines(tempPath)),
                    not(anyOf(
                            containsString("2.0.0-SNAPSHOT"),
                            containsString("2.0.0-beta"),
                            containsString("2.0.0-rc1"))));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    @Test
    public void testAllowMajorUpdatesFalse()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            final File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempPath));

            assertThat(output, containsString("1.1.0"));
            assertThat(output, not(containsString("2.0.0")));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    @Test
    public void testAllowMinorUpdatesFalse()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            final File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempPath));

            assertThat(output, containsString("1.0.1"));
            assertThat(output, not(containsString("1.1.0")));
            assertThat(output, not(containsString("2.0.0")));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    @Test
    public void testAllowIncrementalUpdatesFalse()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            final File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempPath));

            assertThat(output, containsString("1.0.0-1"));
            assertThat(output, not(containsString("1.0.1")));
            assertThat(output, not(containsString("1.1.0")));
            assertThat(output, not(containsString("2.0.0")));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    @Test
    public void testVersionsWithQualifiersNotConsideredAsIncrementalUpdates()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            final File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            assertThat(
                    String.join("", Files.readAllLines(tempPath)),
                    not(anyOf(
                            containsString("1.9.0-SNAPSHOT"),
                            containsString("1.9.0-beta"),
                            containsString("1.9.0-rc1"))));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }

    @Test
    public void testDetermineUpdatedSegment() throws Exception {
        File outputFile = null;
        try {
            outputFile = File.createTempFile("display-dependency-updates", "");
            assert outputFile.exists();

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
            mojo.outputFile = outputFile;
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT"});
                }
            });

            assertThat(mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize(3));
            mojo.execute();
            List<String> output = Files.readAllLines(outputFile.toPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("1.1.0-M1"))));
        } finally {
            assert outputFile == null || !outputFile.exists() || outputFile.delete();
        }
    }

    @Test
    public void testVersionInterpolation() throws Exception {
        File outputFile = null;
        try {
            outputFile = File.createTempFile("display-dependency-updates", "");
            assert outputFile.exists();

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/version-interpolation"),
                    "display-dependency-updates");

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = outputFile;
            mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", new String[] {"2.0.1"});
                }
            });
            setVariableValueToObject(mojo, "processDependencyManagementTransitive", false);
            mojo.execute();
            List<String> output = Files.readAllLines(outputFile.toPath(), UTF_8);
            assertThat(output, not(hasItem(containsString("mycomponent.version"))));
        } finally {
            assert outputFile == null || !outputFile.exists() || outputFile.delete();
        }
    }

    @Test
    public void testAllowSnapshots()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        Path tempPath = null;
        try {
            tempPath = Files.createTempFile("display-dependency-updates", "");
            File tempFile = tempPath.toFile();
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
                    this.outputFile = tempFile;
                    setPluginContext(new HashMap<>());

                    session = mockMavenSession();
                }
            }.execute();

            String output = String.join("", Files.readAllLines(tempPath));

            assertThat(output, containsString("1.0.1-SNAPSHOT"));
        } finally {
            if (tempPath != null && Files.exists(tempPath)) {
                Files.delete(tempPath);
            }
        }
    }
}
