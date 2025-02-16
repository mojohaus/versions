package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.startsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;

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

/**
 * Unit tests for {@link ForceReleasesMojo}
 */
public class ForceReleasesMojoTest extends AbstractMojoTestCase {
    private TestChangeRecorder changeRecorder;
    private ForceReleasesMojo mojo;

    @Mock
    private Log log;

    private PomHelper pomHelper;

    private ArtifactFactory artifactFactory;

    @Mock
    private ExpressionEvaluator expressionEvaluator;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        openMocks(this);
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        MavenSession session = mockMavenSession();

        changeRecorder = new TestChangeRecorder();
        mojo = new ForceReleasesMojo(artifactFactory, mockAetherRepositorySystem(), null, changeRecorder.asTestMap());
        setVariableValueToObject(mojo, "reactorProjects", emptyList());
        mojo.project = new MavenProject() {
            {
                setModel(new Model() {
                    {
                        setGroupId("default-group");
                        setArtifactId("default-artifact");
                        setVersion("1.0.0");
                    }
                });
            }
        };
        mojo.session = session;
        mojo.mojoExecution = mock(MojoExecution.class);
    }

    @Test
    public void testProcessParent()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processParent", true);
        mojo.getProject().setParent(new MavenProject(new Model() {
            {
                setGroupId("default-group");
                setArtifactId("artifactA");
                setVersion("1.0.0-SNAPSHOT");
            }
        }));
        mojo.getProject()
                .setParentArtifact(new DefaultArtifact(
                        "default-group", "artifactA", "1.0.0-SNAPSHOT", SCOPE_COMPILE, "pom", "default", null));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), anyString()))
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
                        "1.0.0-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testReplaceSnapshotWithRelease()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0-SNAPSHOT")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class), any()))
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
                        "1.0.0-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testUpgrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.1.0-SNAPSHOT")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class), any()))
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
                        "1.1.0-SNAPSHOT", "2.0.0")));
    }

    @Test
    public void testDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("2.1.0-SNAPSHOT")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class), any()))
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
                        "2.1.0-SNAPSHOT", "2.0.0")));
    }

    @Test
    public void testFailIfNotReplaced()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.repositorySystem = mockAetherRepositorySystem(singletonMap("test-artifact", new String[] {}));
        mojo.getProject()
                .setDependencies(singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("test-artifact")
                        .withVersion("1.0.0-SNAPSHOT")
                        .build()));
        mojo.failIfNotReplaced = true;

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(
                            any(), anyString(), anyString(), anyString(), anyString(), any(Model.class), any()))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
            fail("MojoExecutionException is expected");
        } catch (MojoExecutionException e) {
            assertThat(e.getMessage(), startsWith("No matching"));
        }
    }
}
