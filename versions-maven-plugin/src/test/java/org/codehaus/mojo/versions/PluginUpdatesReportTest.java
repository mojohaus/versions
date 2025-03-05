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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Set;

import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.reporting.ReportRendererFactoryImpl;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockI18N;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.matchesPattern;
import static org.mockito.Mockito.mock;

/**
 * Basic tests for {@linkplain PluginUpdatesReport}.
 *
 * @author Andrzej Jarmoniuk
 */
public class PluginUpdatesReportTest {
    private static class TestPluginUpdatesReport extends PluginUpdatesReport {
        static final I18N MOCK_I18N = mockI18N();

        TestPluginUpdatesReport() {
            super(
                    MOCK_I18N,
                    new ArtifactFactory(mockArtifactHandlerManager()),
                    mockAetherRepositorySystem(),
                    null,
                    new ReportRendererFactoryImpl(MOCK_I18N));
            siteTool = MockUtils.mockSiteTool();

            project = new MavenProject();
            project.setBuild(new Build());
            project.getBuild().setPluginManagement(new PluginManagement());

            session = mockMavenSession();
            mojoExecution = mock(MojoExecution.class);
        }

        public TestPluginUpdatesReport withPlugins(Plugin... plugins) {
            project.getBuild().setPlugins(Arrays.asList(plugins));
            return this;
        }

        public TestPluginUpdatesReport withAetherRepositorySystem(RepositorySystem repositorySystem) {
            this.repositorySystem = repositorySystem;
            return this;
        }

        public TestPluginUpdatesReport withPluginManagement(Plugin... pluginManagement) {
            project.getBuild().getPluginManagement().setPlugins(Arrays.asList(pluginManagement));
            return this;
        }

        public TestPluginUpdatesReport withOnlyUpgradable(boolean onlyUpgradable) {
            this.onlyUpgradable = onlyUpgradable;
            return this;
        }

        public TestPluginUpdatesReport withOnlyProjectPlugins(boolean onlyProjectPlugins) {
            this.onlyProjectPlugins = onlyProjectPlugins;
            return this;
        }

        public TestPluginUpdatesReport withRuleSet(RuleSet ruleSet) {
            this.ruleSet = ruleSet;
            return this;
        }

        public TestPluginUpdatesReport withIgnoredVersions(Set<String> ignoredVersions) {
            this.ignoredVersions = ignoredVersions;
            return this;
        }
    }

    private static Plugin pluginOf(String artifactId) {
        return pluginOf(artifactId, "1.0.0");
    }

    private static Plugin pluginOf(String artifactId, String version) {
        return new Plugin() {
            {
                setGroupId("defaultGroup");
                setArtifactId(artifactId);
                setVersion(version);
            }
        };
    }

    @Test
    public void testOnlyUpgradablePlugins() throws IOException, MavenReportException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                        put("artifactC", new String[] {"1.0.0", "2.0.0"});
                    }
                }))
                .withPlugins(
                        pluginOf("artifactA", "1.0.0"), pluginOf("artifactB", "1.0.0"), pluginOf("artifactC", "2.0.0"))
                .withOnlyUpgradable(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, containsString("artifactA"));
        assertThat(output, not(containsString("artifactB")));
        assertThat(output, not(containsString("artifactC")));
    }

    @Test
    public void testOnlyUpgradableWithPluginManagement() throws IOException, MavenReportException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
                .withAetherRepositorySystem(mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                        put("artifactC", new String[] {"1.0.0", "2.0.0"});
                    }
                }))
                .withPluginManagement(
                        pluginOf("artifactA", "1.0.0"), pluginOf("artifactB", "1.0.0"), pluginOf("artifactC", "2.0.0"))
                .withOnlyUpgradable(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, containsString("artifactA"));
        assertThat(output, not(containsString("artifactB")));
        assertThat(output, not(containsString("artifactC")));
    }

    @Test
    public void testOnlyProjectPlugins() throws IOException, MavenReportException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
                .withPlugins(pluginOf("artifactA"))
                .withPluginManagement(pluginOf("artifactA"), pluginOf("artifactB"), pluginOf("artifactC"))
                .withOnlyUpgradable(true)
                .withOnlyProjectPlugins(true)
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(output, containsString("artifactA"));
        assertThat(output, not(anyOf(containsString("artifactB"), containsString("artifactC"))));
    }

    @Test
    public void testOnlyProjectPluginsWithIgnoredVersions() throws IOException, MavenReportException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
                .withPlugins(pluginOf("artifactA"))
                .withPluginManagement(pluginOf("artifactA"), pluginOf("artifactB"), pluginOf("artifactC"))
                .withOnlyUpgradable(true)
                .withOnlyProjectPlugins(true)
                .withIgnoredVersions(Collections.singleton("2.0.0"))
                .generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output =
                os.toString().replaceAll("\\s", " ").replaceAll("<[^>]+>", " ").replaceAll("&[^;]+;", " ");
        assertThat(output, matchesPattern(".*\\breport.overview.numNewerVersionAvailable\\s+0\\b.*"));
    }
}
