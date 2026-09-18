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
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.junit.Test;
import org.mockito.Mockito;

import static org.codehaus.mojo.versions.utils.ArtifactVersionService.getArtifactVersion;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class PluginUpdatesAnalyzerTest {
    private final ProjectBuilder builder = mock(ProjectBuilder.class);
    private final VersionsHelper helper = mock(VersionsHelper.class);
    private final MavenProject project = new MavenProject();
    private final ArtifactFactory factory = new ArtifactFactory(mockArtifactHandlerManager());

    private PluginUpdatesAnalyzer analyzer(String minimum) throws Exception {
        return analyzer(minimum, false);
    }

    private PluginUpdatesAnalyzer analyzer(String minimum, boolean allowSnapshots) throws Exception {
        project.setBuild(new Build());
        if (minimum != null) {
            Prerequisites prerequisites = new Prerequisites();
            prerequisites.setMaven(minimum);
            project.getModel().setPrerequisites(prerequisites);
        }
        when(helper.lookupArtifactVersions(any(Artifact.class), eq(true)))
                .thenAnswer(invocation -> new ArtifactVersions(
                        invocation.getArgument(0),
                        Arrays.asList("1.0", "2.0", "3.0").stream()
                                .map(ArtifactVersionService::getArtifactVersion)
                                .collect(Collectors.toList())));
        when(builder.build(any(Artifact.class), eq(true), any(ProjectBuildingRequest.class)))
                .thenAnswer(invocation -> {
                    Artifact artifact = invocation.getArgument(0);
                    MavenProject pluginProject = new MavenProject();
                    Prerequisites prerequisites = new Prerequisites();
                    prerequisites.setMaven(artifact.getVersion().equals("3.0") ? "4.0" : "3.6.3");
                    pluginProject.getModel().setPrerequisites(prerequisites);
                    ProjectBuildingResult result = mock(ProjectBuildingResult.class);
                    when(result.getProject()).thenReturn(pluginProject);
                    return result;
                });
        MavenSession session = mockMavenSession();
        when(session.getRequest()).thenReturn(new DefaultMavenExecutionRequest());
        return new PluginUpdatesAnalyzer(
                factory,
                helper,
                builder,
                session,
                project,
                new SystemStreamLog(),
                getArtifactVersion("3.9.11"),
                allowSnapshots);
    }

    private Plugin plugin() {
        Plugin plugin = new Plugin();
        plugin.setGroupId("localhost");
        plugin.setArtifactId("dummy-maven-plugin");
        plugin.setVersion("1.0");
        return plugin;
    }

    @Test
    public void compatibleUpdatesAndUpgradeOpportunitiesShareCachedPrerequisites() throws Exception {
        PluginUpdatesAnalyzer analyzer = analyzer("3.6.3");
        PluginUpdateAnalysis analysis = analyzer.analyze(plugin(), "1.0");
        assertEquals("2.0", analysis.getCompatibleVersion().toString());
        assertEquals("3.6.3", analysis.getRequiredMavenVersion().toString());
        assertEquals(
                "3.0",
                analysis.getMavenUpgrades().get(getArtifactVersion("4.0")).toString());
        analyzer.analyze(plugin(), "1.0");
        verify(builder, times(3)).build(any(Artifact.class), eq(true), any(ProjectBuildingRequest.class));
        verify(helper, times(1)).lookupArtifactVersions(any(Artifact.class), eq(true));
    }

    @Test
    public void unspecifiedVersionUsesRunningMavenWithoutInventingDeclaredMinimum() throws Exception {
        PluginUpdatesAnalyzer analyzer = analyzer(null);
        PluginUpdateAnalysis analysis = analyzer.analyze(plugin(), null);
        assertNull(analyzer.getMinimumMaven());
        assertNull(analysis.getCompatibleVersion());
        assertEquals("2.0", analysis.getEffectiveVersion());
        assertEquals(2, analysis.getMavenUpgrades().size());
    }

    @Test
    public void unavailablePomIsNotCompatibleAndFailureIsCached() throws Exception {
        PluginUpdatesAnalyzer analyzer = analyzer("4.0");
        Mockito.doThrow(new ProjectBuildingException(
                        "localhost:dummy-maven-plugin:3.0", "unavailable", (Exception) null))
                .when(builder)
                .build(
                        argThat((Artifact a) -> a.getVersion().equals("3.0")),
                        eq(true),
                        any(ProjectBuildingRequest.class));
        PluginUpdateAnalysis analysis = analyzer.analyze(plugin(), "1.0");
        assertEquals("2.0", analysis.getCompatibleVersion().toString());
        analyzer.analyze(plugin(), "1.0");
        verify(builder, times(3)).build(any(Artifact.class), eq(true), any(ProjectBuildingRequest.class));
    }

    @Test
    public void snapshotsUseTheSameSelectionPolicy() throws Exception {
        for (boolean snapshots : new boolean[] {false, true}) {
            PluginUpdatesAnalyzer analyzer = analyzer("4.0", snapshots);
            when(helper.lookupArtifactVersions(any(Artifact.class), eq(true)))
                    .thenAnswer(invocation -> new ArtifactVersions(
                            invocation.getArgument(0),
                            Arrays.asList(
                                    getArtifactVersion("1.0"),
                                    getArtifactVersion("2.0"),
                                    getArtifactVersion("3.0-SNAPSHOT"))));
            assertEquals(
                    snapshots ? "3.0-SNAPSHOT" : "2.0",
                    analyzer.analyze(plugin(), "1.0").getCompatibleVersion().toString());
        }
    }

    @Test
    public void missingPluginPrerequisitesAddNoConstraint() throws Exception {
        PluginUpdatesAnalyzer analyzer = analyzer(null);
        ProjectBuildingResult result = mock(ProjectBuildingResult.class);
        when(result.getProject()).thenReturn(new MavenProject());
        org.mockito.Mockito.doReturn(result)
                .when(builder)
                .build(any(Artifact.class), eq(true), any(ProjectBuildingRequest.class));
        PluginUpdateAnalysis analysis = analyzer.analyze(plugin(), "1.0");
        assertEquals("3.0", analysis.getCompatibleVersion().toString());
        assertNull(analysis.getRequiredMavenVersion());
    }

    @Test
    public void cachesDoNotLeakBetweenProjectAnalyses() throws Exception {
        analyzer("3.6.3").analyze(plugin(), "1.0");
        analyzer("3.6.3").analyze(plugin(), "1.0");
        verify(builder, times(6)).build(any(Artifact.class), eq(true), any(ProjectBuildingRequest.class));
    }
}
