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

import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.project.MavenProject;
import org.apache.maven.rtinfo.RuntimeInformation;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PluginUpdatesAggregateReportTest {
    private static class TestReport extends PluginUpdatesAggregateReport {
        private final Map<MavenProject, VersionsHelper> helpers = new IdentityHashMap<>();

        TestReport(MavenProject project, ReportRendererFactory rendererFactory, boolean onlyUpgradable) {
            super(null, new ArtifactFactory(mockArtifactHandlerManager()), null, null, rendererFactory);
            this.project = project;
            this.onlyUpgradable = onlyUpgradable;
            runtimeInformation = mock(RuntimeInformation.class);
            when(runtimeInformation.getMavenVersion()).thenReturn("3.9.11");
        }

        @Override
        public VersionsHelper getHelper() {
            return helpers.get(project);
        }

        @Override
        protected VersionsHelper createHelper(MavenProject project) {
            return helpers.get(project);
        }
    }

    @Test
    public void childUpdatesSurviveMergingWithAnUpToDateRoot() throws Exception {
        for (boolean managed : new boolean[] {false, true}) {
            for (boolean onlyUpgradable : new boolean[] {false, true}) {
                PluginUpdatesDetails result = aggregate(managed, onlyUpgradable, new String[] {"1.0"});
                assertEquals(Collections.singletonList("2.0"), versions(result));
                assertEquals(2, result.getDependencyVersions().size());
            }
        }
    }

    @Test
    public void availableVersionsFromEveryProjectAreCombined() throws Exception {
        for (boolean managed : new boolean[] {false, true}) {
            for (boolean onlyUpgradable : new boolean[] {false, true}) {
                PluginUpdatesDetails result = aggregate(managed, onlyUpgradable, new String[] {"1.0", "3.0"});
                assertEquals(Arrays.asList("2.0", "3.0"), versions(result));
            }
        }
    }

    private static List<String> versions(PluginUpdatesDetails details) {
        return Arrays.stream(details.getVersions(false)).map(Object::toString).collect(Collectors.toList());
    }

    private PluginUpdatesDetails aggregate(boolean managed, boolean onlyUpgradable, String[] rootVersions)
            throws Exception {
        MavenProject root = project("root", managed);
        MavenProject child = project("child", managed);
        root.setCollectedProjects(Collections.singletonList(child));
        ReportRendererFactory rendererFactory = mock(ReportRendererFactory.class);
        when(rendererFactory.createReportRenderer(anyString(), isNull(), any(), any(), anyBoolean()))
                .thenReturn(mock(ReportRenderer.class));
        TestReport report = new TestReport(root, rendererFactory, onlyUpgradable);
        report.helpers.put(root, helper("root-dependency", rootVersions));
        report.helpers.put(child, helper("child-dependency", "1.0", "2.0"));
        report.doGenerateReport(Locale.ROOT, null);
        ArgumentCaptor<PluginUpdatesModel> captured = ArgumentCaptor.forClass(PluginUpdatesModel.class);
        verify(rendererFactory)
                .createReportRenderer(anyString(), isNull(), eq(Locale.ROOT), captured.capture(), eq(false));
        Map<Dependency, PluginUpdatesDetails> updates = managed
                ? captured.getValue().getArtifactManagementUpdates()
                : captured.getValue().getArtifactUpdates();
        assertEquals(1, updates.size());
        PluginUpdatesDetails result = updates.values().iterator().next();
        assertEquals("1.0", result.getVersion());
        return result;
    }

    private static MavenProject project(String artifactId, boolean managed) {
        MavenProject project = new MavenProject();
        project.setArtifactId(artifactId);
        project.setBuild(new Build());
        Plugin plugin = new Plugin();
        plugin.setGroupId("example");
        plugin.setArtifactId("example-plugin");
        plugin.setVersion("1.0");
        Dependency dependency = new Dependency();
        dependency.setGroupId("example");
        dependency.setArtifactId(artifactId + "-dependency");
        dependency.setVersion("1.0");
        plugin.addDependency(dependency);
        if (managed) {
            PluginManagement management = new PluginManagement();
            management.addPlugin(plugin);
            project.getBuild().setPluginManagement(management);
        } else {
            project.getBuild().addPlugin(plugin);
        }
        return project;
    }

    private static VersionsHelper helper(String dependencyId, String... versions) throws Exception {
        VersionsHelper helper = mock(VersionsHelper.class);
        when(helper.lookupArtifactVersions(any(Artifact.class), eq(true)))
                .thenAnswer(invocation -> new ArtifactVersions(
                        invocation.getArgument(0),
                        Arrays.stream(versions)
                                .map(ArtifactVersionService::getArtifactVersion)
                                .collect(Collectors.toList())));
        Dependency dependency = new Dependency();
        dependency.setGroupId("example");
        dependency.setArtifactId(dependencyId);
        dependency.setVersion("1.0");
        ArtifactFactory factory = new ArtifactFactory(mockArtifactHandlerManager());
        Map<Dependency, ArtifactVersions> dependencies = new TreeMap<>(DependencyComparator.INSTANCE);
        dependencies.put(dependency, new ArtifactVersions(factory.createArtifact(dependency), Collections.emptyList()));
        when(helper.lookupDependenciesUpdates(any(), eq(false), eq(false))).thenReturn(dependencies);
        return helper;
    }
}
