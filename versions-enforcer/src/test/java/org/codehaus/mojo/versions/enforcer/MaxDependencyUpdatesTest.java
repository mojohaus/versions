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

import java.util.HashMap;

import org.apache.maven.enforcer.rule.api.EnforcerLogger;
import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.repository.exception.ComponentLookupException;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class MaxDependencyUpdatesTest {

    @Mock
    private EnforcerLogger enforcerLogger;

    @Mock
    private MavenProject project;

    @Mock
    private MavenSession mavenSession;

    @Mock
    private RepositorySystem repositorySystem;

    @Mock
    private org.eclipse.aether.RepositorySystem aetherRepositorySystem;

    @InjectMocks
    private MaxDependencyUpdates maxDependencyUpdates;

    @Before
    public void setup() {
        maxDependencyUpdates.setLog(enforcerLogger);
        MockUtils.prepareRepositorySystemMock(repositorySystem);
    }

    @Test
    public void testRuleFailsByMaxUpdatesExceeded() throws ExpressionEvaluationException, ComponentLookupException {
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

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0", "2.0.0"});
                    }
                });

        try {
            maxDependencyUpdates.maxUpdates = 1;
            maxDependencyUpdates.execute();

            fail("EnforcerRuleException should have been thrown");
        } catch (EnforcerRuleException e) {
            assertThat(e.getMessage(), containsString("More than 1 upgradable artifacts detected"));
        }
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceeded() throws Exception {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "2.0.0"}));

        maxDependencyUpdates.maxUpdates = 1;
        maxDependencyUpdates.execute();
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyIncludes() throws Exception {

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

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                    }
                });

        maxDependencyUpdates.dependencyIncludes = singletonList("group:artifactB");
        maxDependencyUpdates.execute();
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyExcludes() throws Exception {

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

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                    }
                });

        maxDependencyUpdates.dependencyExcludes = singletonList("group:artifactA");
        maxDependencyUpdates.execute();
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyIncludesExcludes() throws Exception {

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

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, new HashMap<String, String[]>() {
                    {
                        put("artifactA", new String[] {"1.0.0", "2.0.0"});
                        put("artifactB", new String[] {"1.0.0"});
                    }
                });

        maxDependencyUpdates.dependencyIncludes = singletonList("group:*");
        maxDependencyUpdates.dependencyExcludes = singletonList("group:artifactA");
        maxDependencyUpdates.execute();
    }

    @Test
    public void testIgnoreSubIncrementalUpdates() throws Exception {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1"}));

        maxDependencyUpdates.ignoreSubIncrementalUpdates = true;
        maxDependencyUpdates.execute();
    }

    @Test
    public void testIgnoreIncrementalUpdates() throws Exception {
        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1", "1.0.1"}));

        maxDependencyUpdates.ignoreIncrementalUpdates = true;
        maxDependencyUpdates.execute();
    }

    @Test
    public void testIgnoreMinorUpdates() throws Exception {

        when(project.getDependencies())
                .thenReturn(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        MockUtils.prepareAetherRepositorySystemMockForVersionRange(
                aetherRepositorySystem, singletonMap("artifactA", new String[] {"1.0.0", "1.0.0-1", "1.0.1", "1.1.0"}));

        maxDependencyUpdates.ignoreMinorUpdates = true;
        maxDependencyUpdates.execute();
    }
}
