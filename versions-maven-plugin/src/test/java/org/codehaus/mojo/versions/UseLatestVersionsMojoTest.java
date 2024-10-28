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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.eclipse.aether.RepositorySystem;
import org.hamcrest.core.Is;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singleton;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

public class UseLatestVersionsMojoTest {
    private UseLatestVersionsMojo mojo;
    private TestChangeRecorder changeRecorder;

    @Before
    public void setUp() throws Exception {
        RepositorySystem repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put(
                        "dependency-artifact",
                        new String[] {"1.1.1-SNAPSHOT", "1.1.0", "1.1.0-SNAPSHOT", "1.0.0", "1.0.0-SNAPSHOT", "0.9.0"});
                put("poison-artifact", new String[] {
                    "1.1.1.1-SNAPSHOT", "1.1.1.0", "1.1.1.0-SNAPSHOT", "1.0.0.0", "1.0.0.0-SNAPSHOT", "0.9.0.0"
                });
                put("other-artifact", new String[] {"1.0", "2.0"});
            }
        });

        changeRecorder = new TestChangeRecorder();

        mojo =
                new UseLatestVersionsMojo(
                        mockArtifactHandlerManager(), repositorySystem, null, changeRecorder.asTestMap()) {
                    {
                        reactorProjects = emptyList();
                        MavenProject project = new MavenProject() {
                            {
                                setModel(new Model() {
                                    {
                                        setGroupId("default-group");
                                        setArtifactId("project-artifact");
                                        setVersion("1.0.0-SNAPSHOT");

                                        setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("dependency-artifact")
                                                .withVersion("1.1.1-SNAPSHOT")
                                                .withType("pom")
                                                .withClassifier("default")
                                                .withScope(SCOPE_COMPILE)
                                                .build()));

                                        setDependencyManagement(new DependencyManagement());
                                        getDependencyManagement()
                                                .setDependencies(
                                                        Collections.singletonList(DependencyBuilder.newBuilder()
                                                                .withGroupId("default-group")
                                                                .withArtifactId("dependency-artifact")
                                                                .withVersion("1.1.1-SNAPSHOT")
                                                                .withType("pom")
                                                                .withClassifier("default")
                                                                .withScope(SCOPE_COMPILE)
                                                                .build()));
                                    }
                                });
                            }
                        };
                        setProject(project);

                        session = mockMavenSession();
                    }
                };
        setVariableValueToObject(mojo, "processDependencyManagement", false);
    }

    @Test
    public void testDependenciesDowngradeIncremental()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testDependenciesDowngradeMinor()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        mojo.getProject()
                .getModel()
                .setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("dependency-artifact")
                        .withVersion("1.1.0-SNAPSHOT")
                        .withType("pom")
                        .withClassifier("default")
                        .withScope(SCOPE_COMPILE)
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact",
                        "1.1.0-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testDependenciesDowngradeMajor()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", true);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact",
                        "1.1.1-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testDependencyManagementDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencyManagement", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testParentDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processParent", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        mojo.getProject()
                .setParentArtifact(new DefaultArtifact(
                        "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "compile", "pom", "default", null));
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId(mojo.getProject().getParentArtifact().getGroupId());
                setArtifactId(mojo.getProject().getParentArtifact().getArtifactId());
                setVersion(mojo.getProject().getParentArtifact().getVersion());
            }
        });

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testPoisonDependencyVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Arrays.asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("dependency-artifact")
                                .withVersion("1.1.1-SNAPSHOT")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("poison-artifact")
                                .withVersion("1.1.1.1-SNAPSHOT")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build()));

        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "allowDowngrade", true);

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        // So, the regular update should take place despite an irregular, or — if I may — "poison", dependency
        // being present in the dependency list
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0")));
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "ignoredVersions", singleton("1.1.0"));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), Is.is(empty()));
    }

    @Test
    public void testIncludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Arrays.asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("dependency-artifact")
                                .withVersion("0.9.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("other-artifact")
                                .withVersion("1.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build()));
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:other-artifact"});

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), hasSize(1));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "other-artifact", "1.0", "2.0")));
    }

    @Test
    public void testExcludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Arrays.asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("dependency-artifact")
                                .withVersion("0.9.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("other-artifact")
                                .withVersion("1.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build()));
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "excludes", new String[] {"default-group:other-artifact"});

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), hasSize(1));
        assertThat(
                changeRecorder.getChanges(),
                not(hasItem(new DefaultDependencyVersionChange("default-group", "other-artifact", "1.0", "2.0"))));
    }
}
