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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.model.Build;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.utils.ExtensionBuilder;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Basic tests for {@linkplain DisplayExtensionUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayExtensionUpdatesMojoTest {
    private DisplayExtensionUpdatesMojo mojo;
    private Path tempPath;

    @Before
    public void setUp() throws IllegalAccessException, IOException {
        mojo = new DisplayExtensionUpdatesMojo(mockRepositorySystem(), mockAetherRepositorySystem(), null, null);
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
        mojo.project.setRemoteArtifactRepositories(emptyList());
        mojo.project.setPluginArtifactRepositories(emptyList());
        mojo.session = mockMavenSession(mojo.project);
        tempPath = Files.createTempFile("display-extension-updates-", ".log");
        mojo.outputFile = tempPath.toFile();
        mojo.outputEncoding = "UTF-8";
        setVariableValueToObject(mojo, "processCoreExtensions", false);
        // turning interpolateExtensions off so that we don't need to bother with the model tree
        mojo.interpolateProperties = false;

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
}
