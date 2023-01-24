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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.reporting.ReportRendererFactoryImpl;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.codehaus.plexus.i18n.I18N;
import org.hamcrest.Matchers;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockI18N;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.matchesPattern;

/**
 * Basic tests for {@linkplain DependencyUpdatesReportMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DependencyUpdatesReportMojoTest {
    private static class TestDependencyUpdatesReportMojo extends DependencyUpdatesReportMojo {
        private static final I18N MOCK_I18N = mockI18N();

        TestDependencyUpdatesReportMojo() {
            super(
                    MOCK_I18N,
                    mockRepositorySystem(),
                    mockAetherRepositorySystem(),
                    null,
                    new ReportRendererFactoryImpl(MOCK_I18N));
            siteTool = MockUtils.mockSiteTool();

            project = new MavenProject();
            project.setOriginalModel(new Model());
            project.getOriginalModel().setDependencyManagement(new DependencyManagement());
            project.getModel().setDependencyManagement(new DependencyManagement());

            session = mockMavenSession();
        }

        public TestDependencyUpdatesReportMojo withDependencies(Dependency... dependencies) {
            project.setDependencies(Arrays.asList(dependencies));
            return this;
        }

        public TestDependencyUpdatesReportMojo withAetherRepositorySystem(
                org.eclipse.aether.RepositorySystem repositorySystem) {
            this.aetherRepositorySystem = repositorySystem;
            return this;
        }

        public TestDependencyUpdatesReportMojo withOriginalDependencyManagement(
                Dependency... originalDependencyManagement) {
            project.getOriginalModel()
                    .getDependencyManagement()
                    .setDependencies(Arrays.asList(originalDependencyManagement));
            return this;
        }

        public TestDependencyUpdatesReportMojo withDependencyManagement(Dependency... dependencyManagement) {
            project.getModel().getDependencyManagement().setDependencies(Arrays.asList(dependencyManagement));
            return this;
        }

        public TestDependencyUpdatesReportMojo withOnlyUpgradable(boolean onlyUpgradable) {
            this.onlyUpgradable = onlyUpgradable;
            return this;
        }

        public TestDependencyUpdatesReportMojo withProcessDependencyManagement(boolean processDependencyManagement) {
            this.processDependencyManagement = processDependencyManagement;
            return this;
        }

        public TestDependencyUpdatesReportMojo withProcessDependencyManagementTransitive(
                boolean processDependencyManagementTransitive) {
            this.processDependencyManagementTransitive = processDependencyManagementTransitive;
            return this;
        }

        public TestDependencyUpdatesReportMojo withOnlyProjectDependencies(boolean onlyProjectDependencies) {
            this.onlyProjectDependencies = onlyProjectDependencies;
            return this;
        }

        public TestDependencyUpdatesReportMojo withRuleSet(RuleSet ruleSet) {
            this.ruleSet = ruleSet;
            return this;
        }

        public TestDependencyUpdatesReportMojo withIgnoredVersions(Set<String> ignoredVersions) {
            this.ignoredVersions = ignoredVersions;
            return this;
        }

        public TestDependencyUpdatesReportMojo withAllowSnapshots(boolean allowSnapshots) {
            this.allowSnapshots = allowSnapshots;
            return this;
        }

        public TestDependencyUpdatesReportMojo withOriginalProperty(String name, String value) {
            project.getOriginalModel().getProperties().put(name, value);
            return this;
        }
    }

    private static Dependency dependencyOf(String artifactId) {
        return DependencyBuilder.dependencyWith("groupA", artifactId, "1.0.0", "default", "pom", SCOPE_COMPILE);
    }

    private static Dependency dependencyOf(String artifactId, String version) {
        return DependencyBuilder.dependencyWith("groupA", artifactId, version, "default", "pom", SCOPE_COMPILE);
    }

    @Test
    public void testOnlyUpgradableDependencies() throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withOnlyUpgradable(true)
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                        put("artifactC", new String[] {"1.0.0", "2.0.0"});
                    }
                }))
                .withDependencies(
                        dependencyOf("artifactA", "1.0.0"),
                        dependencyOf("artifactB", "1.0.0"),
                        dependencyOf("artifactC", "2.0.0"))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, containsString("artifactA"));
        assertThat(output, not(containsString("artifactB")));
        assertThat(output, not(containsString("artifactC")));
    }

    @Test
    public void testOnlyUpgradableWithOriginalDependencyManagement()
            throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withOriginalDependencyManagement(
                        dependencyOf("artifactA"), dependencyOf("artifactB"), dependencyOf("artifactC"))
                .withProcessDependencyManagement(true)
                .withOnlyUpgradable(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, allOf(containsString("artifactA"), containsString("artifactB")));
        assertThat(output, not(containsString("artifactC")));
    }

    @Test
    public void testOnlyUpgradableWithTransitiveDependencyManagement()
            throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withDependencyManagement(
                        dependencyOf("artifactA"), dependencyOf("artifactB"), dependencyOf("artifactC"))
                .withProcessDependencyManagement(true)
                .withProcessDependencyManagementTransitive(true)
                .withOnlyUpgradable(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, allOf(containsString("artifactA"), containsString("artifactB")));
        assertThat(output, not(containsString("artifactC")));
    }

    @Test
    public void testOnlyProjectDependencies() throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withDependencies(dependencyOf("artifactA"))
                .withDependencyManagement(
                        dependencyOf("artifactA"), dependencyOf("artifactB"), dependencyOf("artifactC"))
                .withProcessDependencyManagement(true)
                .withOnlyProjectDependencies(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, containsString("artifactA"));
        assertThat(output, not(anyOf(containsString("artifactB"), containsString("artifactC"))));
    }

    @Test
    public void testOnlyProjectDependenciesWithIgnoredVersions()
            throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withDependencies(dependencyOf("artifactA"))
                .withDependencyManagement(
                        dependencyOf("artifactA"), dependencyOf("artifactB"), dependencyOf("artifactC"))
                .withProcessDependencyManagement(true)
                .withOnlyProjectDependencies(true)
                .withIgnoredVersions(Collections.singleton("2.0.0"))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString().replaceAll("\n", "");
        assertThat(output, containsString("report.noUpdatesAvailable"));
    }

    /**
     * Dependencies should be rendered in alphabetical order
     */
    @Test
    public void testDependenciesInAlphabeticalOrder() throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("amstrad", new String[] {"1.0.0", "2.0.0"});
                        put("atari", new String[] {"1.0.0", "2.0.0"});
                        put("commodore", new String[] {"1.0.0", "2.0.0"});
                        put("spectrum", new String[] {"1.0.0", "2.0.0"});
                    }
                }))
                .withDependencies(
                        dependencyOf("spectrum"),
                        dependencyOf("atari"),
                        dependencyOf("amstrad"),
                        dependencyOf("commodore"))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString().replaceAll("\n", "");
        assertThat(output, Matchers.stringContainsInOrder("amstrad", "atari", "commodore", "spectrum"));
    }

    /**
     * Dependency updates for dependency should override those for dependency management
     */
    @Test
    public void testDependenciesShouldOverrideDependencyManagement()
            throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withProcessDependencyManagement(true)
                .withProcessDependencyManagementTransitive(true)
                .withDependencies(dependencyOf("artifactA", "2.0.0"), dependencyOf("artifactB"))
                .withDependencyManagement(dependencyOf("artifactA"))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString().replaceAll("\n", "");
        assertThat(output, Matchers.stringContainsInOrder("artifactB"));
    }

    @Test
    public void testWrongReportBounds() throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withOnlyUpgradable(true)
                .withDependencies(dependencyOf("test-artifact"))
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("test-artifact", new String[] {"1.0.0", "2.0.0-M1"});
                    }
                }))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString().replaceAll("\n", "").replaceAll("\r", "");
        assertThat(
                output,
                allOf(
                        matchesPattern(".*<td>report.overview.numNewerMajorAvailable</td>\\s*<td>1</td>.*"),
                        matchesPattern(".*<td>report.overview.numUpToDate</td>\\s*<td>0</td>.*")));
    }

    @Test
    public void testIt001Overview() throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withOnlyUpgradable(true)
                .withDependencies(dependencyOf("test-artifact", "1.1"))
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("test-artifact", new String[] {
                            "1.1.0-2", "1.1", "1.1.1", "1.1.1-2", "1.1.2", "1.1.3", "1.2", "1.2.1", "1.2.2", "1.3",
                            "2.0", "2.1", "3.0"
                        });
                    }
                }))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString()
                .replaceAll("<[^>]+>", " ")
                .replaceAll("&[^;]+;", " ")
                .replaceAll("\\s+", " ");
        assertThat(
                "Did not generate summary correctly",
                output,
                containsString("groupA test-artifact 1.1 compile pom default 1.1.0-2 1.1.3 1.3 3.0"));
    }

    @Test
    public void testResolvedVersionsWithoutTransitiveDependencyManagement()
            throws IOException, MavenReportException, IllegalAccessException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReportMojo()
                .withOriginalDependencyManagement(
                        dependencyOf("artifactA", "1.0.0"), dependencyOf("artifactB", "${mycomponent.version}"))
                .withDependencyManagement(
                        dependencyOf("artifactA", "1.0.0"), dependencyOf("artifactB", "${mycomponent.version}"))
                .withProcessDependencyManagement(true)
                .withProcessDependencyManagementTransitive(false)
                .withOnlyUpgradable(false)
                .withOriginalProperty("mycomponent.version", "1.2.3")
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, Matchers.stringContainsInOrder("artifactA", "1.0.0", "artifactB", "1.2.3"));
    }
}
