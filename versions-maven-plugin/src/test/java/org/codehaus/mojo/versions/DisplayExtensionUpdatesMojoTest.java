package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Build;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.ExtensionBuilder;
import org.codehaus.mojo.versions.utils.TestVersionChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Basic tests for {@linkplain DisplayExtensionUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayExtensionUpdatesMojoTest {
    private DisplayExtensionUpdatesMojo mojo;
    private Path tempPath;

    @Mock
    private Log log;

    private PomHelper pomHelper;

    private ArtifactFactory artifactFactory;

    @Mock
    private ExpressionEvaluator expressionEvaluator;

    @Before
    public void setUp() throws IllegalAccessException, IOException, MojoExecutionException {
        openMocks(this);
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        mojo = new DisplayExtensionUpdatesMojo(
                artifactFactory, mockAetherRepositorySystem(), null, TestVersionChangeRecorder.asTestMap());
        File baseDir = mock(File.class);
        Path basePath = mock(Path.class);
        doReturn(basePath).when(baseDir).toPath();
        doReturn(basePath).when(basePath).resolve(ArgumentMatchers.<Path>any());
        doReturn(basePath).when(basePath).resolve(ArgumentMatchers.<String>any());
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

            @Override
            public File getBasedir() {
                return baseDir;
            }
        };
        mojo.project.setRemoteArtifactRepositories(emptyList());
        mojo.project.setPluginArtifactRepositories(emptyList());
        mojo.session = mockMavenSession(mojo.project);
        tempPath = Files.createTempFile("display-extension-updates-", ".log");
        mojo.outputFile = tempPath.toFile();
        mojo.outputEncoding = "UTF-8";
        setVariableValueToObject(mojo, "processCoreExtensions", false);
        // turning interpolateExtensions off so that we don't need to bother with the model tree
        mojo.interpolateProperties = false;
        mojo.mojoExecution = mock(MojoExecution.class);

        mojo.setPluginContext(new HashMap<String, Object>() {
            {
                put(
                        "org.codehaus.mojo.versions.AbstractVersionsDisplayMojo.outputFile",
                        singleton(tempPath.toFile().getCanonicalPath()));
            }
        });
    }

    @After
    public void tearDown() throws IOException {
        Files.deleteIfExists(tempPath);
    }

    @Test
    public void testNoBuildExists()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("*"));
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }
    }

    @Test
    public void testIncludesMakesSetEmpty()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("other-group"));
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());

        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(Files.readAllLines(tempPath), empty());
    }

    @Test
    public void testIncludesMakesSetNonEmpty()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("default-group"));
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());

        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(
                String.join("", Files.readAllLines(tempPath)),
                containsString("default-group:artifactA ... 1.0.0 -> 2.0.0"));
    }

    @Test
    public void testIncludesExcludesMakesSetEmpty()
            throws MojoExecutionException, MojoFailureException, IllegalAccessException, IOException {
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("default-group"));
        setVariableValueToObject(mojo, "extensionExcludes", singletonList("default-group:artifactA"));

        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(Files.readAllLines(tempPath), empty());
    }

    @Test
    public void testMajorUpdates()
            throws MojoExecutionException, MojoFailureException, IOException, IllegalAccessException {
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("*"));
        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactA")
                        .withVersion("1.0.0")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(String.join("", Files.readAllLines(tempPath)), containsString("1.0.0 -> 2.0.0"));
    }

    @Test
    public void testMinorUpdates()
            throws MojoExecutionException, MojoFailureException, IOException, IllegalAccessException {
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("*"));
        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactB")
                        .withVersion("1.0.0")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(String.join("", Files.readAllLines(tempPath)), containsString("1.0.0 -> 1.1.0"));
    }

    @Test
    public void testIncrementalUpdates()
            throws MojoExecutionException, MojoFailureException, IOException, IllegalAccessException {
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("*"));
        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("artifactD")
                        .withVersion("1.0.0")
                        .build()));
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("artifactD", new String[] {"1.0.0", "1.0.1"});
            }
        });

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
        }

        assertThat(String.join("", Files.readAllLines(tempPath)), containsString("1.0.0 -> 1.0.1"));
    }

    @Test
    public void testProblemCausingArtifact()
            throws MojoExecutionException, MojoFailureException, IOException, IllegalAccessException {
        setVariableValueToObject(mojo, "extensionExcludes", emptyList());
        setVariableValueToObject(mojo, "extensionIncludes", singletonList("*"));
        mojo.getProject().setBuild(new Build());
        mojo.getProject()
                .getBuild()
                .setExtensions(Collections.singletonList(ExtensionBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("problem-causing-artifact")
                        .withVersion("1.0.0")
                        .build()));

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.getChildModels(ArgumentMatchers.any(MavenProject.class), any()))
                    .then(i -> Collections.singletonMap(null, ((MavenProject) i.getArgument(0)).getModel()));
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertThat(e.getCause(), instanceOf(VersionRetrievalException.class));
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertThat(vre.getArtifact().map(Artifact::getArtifactId).orElse(""), equalTo("problem-causing-artifact"));
        }
    }
}
