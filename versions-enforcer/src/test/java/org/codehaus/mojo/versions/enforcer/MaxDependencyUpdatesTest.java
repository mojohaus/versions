package org.codehaus.mojo.versions.enforcer;
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Properties;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.enforcer.rule.api.EnforcerLogger;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.eclipse.aether.RepositorySystem;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class MaxDependencyUpdatesTest {

    @Mock
    private EnforcerLogger enforcerLogger;

    @Mock
    private MavenProject project;

    @Mock
    private MavenSession mavenSession;

    @Mock
    private RepositorySystem repositorySystem;

    @Mock
    private MojoExecution mojoExecution;

    private ArtifactFactory artifactFactory;

    private ArtifactHandlerManager artifactHandlerManager;

    private MaxDependencyUpdates maxDependencyUpdates;

    @BeforeEach
    public void setup() {
        Properties emptyProperties = new Properties();
        when(mavenSession.getUserProperties()).thenReturn(emptyProperties);
        when(mavenSession.getSystemProperties()).thenReturn(emptyProperties);
        artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        maxDependencyUpdates = new MaxDependencyUpdates(
                project,
                artifactFactory,
                artifactHandlerManager,
                repositorySystem,
                Collections.emptyMap(),
                mavenSession,
                mojoExecution);
        maxDependencyUpdates.setLog(enforcerLogger);
    }

    @Test
    void testRuleFailsByMaxUpdatesExceeded() {
        when(project.getDependencies())
                .thenReturn(asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactA")
                                .withVersion("1.0.0")
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactB")
                                .withVersion("1.0.0")
                                .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(repositorySystem, new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "2.0.0"});
                put("artifactB", new String[] {"1.0.0", "2.0.0"});
            }
        });

        maxDependencyUpdates.maxUpdates = 1;
        Exception e = assertThrows(EnforcerRuleException.class, () -> maxDependencyUpdates.execute());
        assertThat(e.getMessage(), containsString("More than 1 upgradable artifacts detected"));
    }

    @Test
    void testRulePassesByMaxUpdatesNotExceeded() {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                repositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "2.0.0"}));

        maxDependencyUpdates.maxUpdates = 1;
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testRulePassesByMaxUpdatesNotExceededDependencyIncludes() {

        when(project.getDependencies())
                .thenReturn(asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactA")
                                .withVersion("1.0.0")
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactB")
                                .withVersion("1.0.0")
                                .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(repositorySystem, new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "2.0.0"});
                put("artifactB", new String[] {"1.0.0"});
            }
        });

        maxDependencyUpdates.dependencyIncludes = singletonList("group:artifactB");
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testRulePassesByMaxUpdatesNotExceededDependencyExcludes() {

        when(project.getDependencies())
                .thenReturn(asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactA")
                                .withVersion("1.0.0")
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactB")
                                .withVersion("1.0.0")
                                .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(repositorySystem, new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "2.0.0"});
                put("artifactB", new String[] {"1.0.0"});
            }
        });

        maxDependencyUpdates.dependencyExcludes = singletonList("group:artifactA");
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testRulePassesByMaxUpdatesNotExceededDependencyIncludesExcludes() {

        when(project.getDependencies())
                .thenReturn(asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactA")
                                .withVersion("1.0.0")
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("group")
                                .withArtifactId("artifactB")
                                .withVersion("1.0.0")
                                .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(repositorySystem, new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "2.0.0"});
                put("artifactB", new String[] {"1.0.0"});
            }
        });

        maxDependencyUpdates.dependencyIncludes = singletonList("group:*");
        maxDependencyUpdates.dependencyExcludes = singletonList("group:artifactA");
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testIgnoreSubIncrementalUpdates() {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                repositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1"}));

        maxDependencyUpdates.ignoreSubIncrementalUpdates = true;
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testIgnoreIncrementalUpdates() {
        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                repositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1", "1.0.1"}));

        maxDependencyUpdates.ignoreIncrementalUpdates = true;
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }

    @Test
    void testIgnoreMinorUpdates() {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                repositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1", "1.0.1", "1.1.0"}));

        maxDependencyUpdates.ignoreMinorUpdates = true;
        assertDoesNotThrow(() -> maxDependencyUpdates.execute());
    }
}
