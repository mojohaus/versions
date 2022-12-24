package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.stream.XMLStreamException;

import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

/**
 * Unit tests for {@link UseNextVersionsMojo}
 */
public class UseNextVersionsMojoTest {

    private UseNextVersionsMojo mojo;
    private TestChangeRecorder changeRecorder;

    @Before
    public void setUp() throws Exception {
        RepositorySystem repositorySystemMock = mockRepositorySystem();
        org.eclipse.aether.RepositorySystem aetherRepositorySystem =
                mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("dependency-artifact", new String[] {"1.0.0", "1.1.0-SNAPSHOT"});
                    }
                });
        changeRecorder = new TestChangeRecorder();
        mojo = new UseNextVersionsMojo(repositorySystemMock, aetherRepositorySystem, null, changeRecorder.asTestMap()) {
            {
                reactorProjects = emptyList();
                session = mockMavenSession();
                project = new MavenProject() {
                    {
                        setModel(new Model() {
                            {
                                setGroupId("default-group");
                                setArtifactId("project-artifact");
                                setVersion("1.0.0-SNAPSHOT");
                            }
                        });
                        setDependencies(Collections.singletonList(DependencyBuilder.dependencyWith(
                                "default-group",
                                "dependency-artifact",
                                "1.1.0-SNAPSHOT",
                                "default",
                                "pom",
                                SCOPE_COMPILE)));
                    }
                };
            }
        };
        setVariableValueToObject(mojo, "processDependencyManagement", false);
    }

    @Test
    public void testNoNewerVersions() {

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        assertThat(changeRecorder.getChanges(), Matchers.empty());
    }

    @Test
    public void testFindANewerVersion() throws IllegalAccessException {
        setVariableValueToObject(
                mojo, "aetherRepositorySystem", mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("dependency-artifact", new String[] {"1.0.0", "1.1.0-SNAPSHOT", "1.1.1", "2.0.0"});
                    }
                }));
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact", "1.1.0-SNAPSHOT", "1.1.1")));
    }

    @Test
    public void testAllowDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "1.0.1-SNAPSHOT"});
            }
        });
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.1-SNAPSHOT")
                        .build()));
        mojo.allowDowngrade = true;

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class)))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "artifactA",
                        "1.0.1-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testDisallowDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.aetherRepositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactA", new String[] {"1.0.0", "1.0.1-SNAPSHOT"});
            }
        });
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.1-SNAPSHOT")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class)))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), empty());
    }
}
