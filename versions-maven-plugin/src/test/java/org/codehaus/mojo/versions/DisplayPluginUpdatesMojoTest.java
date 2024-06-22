package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingException;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.apache.commons.codec.CharEncoding.UTF_8;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Contains unit tests for {@link DisplayPluginUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayPluginUpdatesMojoTest extends AbstractMojoTestCase {

    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    private Path outputPath;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        tempDir = TestUtils.createTempDir("display-plugin-updates");
        outputPath = Files.createTempFile(tempDir, "output", "");
    }

    @After
    public void tearDown() throws Exception {
        try {
            TestUtils.tearDownTempDir(tempDir);
        } finally {
            super.tearDown();
        }
    }

    private static ProjectBuilder mockupProjectBuilder() {
        Prerequisites prerequisites = new Prerequisites();
        prerequisites.setMaven("3.6.3");
        MavenProject mavenProject = mock(MavenProject.class);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        ProjectBuildingResult projectBuildingResult = mock(ProjectBuildingResult.class);
        when(projectBuildingResult.getProject()).thenReturn(mavenProject);
        ProjectBuilder projectBuilder = mock(ProjectBuilder.class);
        try {
            when(projectBuilder.build(any(Artifact.class), anyBoolean(), any(ProjectBuildingRequest.class)))
                    .thenReturn(projectBuildingResult);
        } catch (ProjectBuildingException e) {
            throw new RuntimeException(e);
        }
        return projectBuilder;
    }

    private DisplayPluginUpdatesMojo createMojo() throws Exception {
        DisplayPluginUpdatesMojo mojo =
                (DisplayPluginUpdatesMojo) mojoRule.lookupConfiguredMojo(tempDir.toFile(), "display-plugin-updates");
        mojo.outputEncoding = UTF_8;
        mojo.outputFile = outputPath.toFile();
        mojo.setPluginContext(new HashMap<>());
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("default-plugin", new String[] {"1.0.0"});
                put("maven-enforcer-plugin", new String[] {"3.0.0"});
            }
        });
        setVariableValueToObject(mojo, "projectBuilder", mockupProjectBuilder());
        return mojo;
    }

    @Test
    public void testNoEnforcer() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/display-plugin-updates/issue-990/no-enforcer.xml"),
                tempDir.resolve("pom.xml"));

        DisplayPluginUpdatesMojo mojo = createMojo();
        mojo.execute();

        List<String> output = Files.readAllLines(outputPath);
        assertThat(output, hasItem(containsString("Using the minimum version of Maven: 3.3.9")));
    }

    @Test
    public void testNoPrerequisites() throws Exception {
        Files.copy(
                Paths.get("src/test/resources/org/codehaus/mojo/display-plugin-updates/issue-990/no-prerequisites.xml"),
                tempDir.resolve("pom.xml"));

        DisplayPluginUpdatesMojo mojo = createMojo();
        mojo.execute();

        List<String> output = Files.readAllLines(outputPath);
        assertThat(output, hasItem(containsString("Using the minimum version of Maven: 3.3.9")));
    }

    @Test
    public void testPrerequisitesGreaterThanEnforcer() throws Exception {
        Files.copy(
                Paths.get(
                        "src/test/resources/org/codehaus/mojo/display-plugin-updates/issue-990/prerequisites-greater.xml"),
                tempDir.resolve("pom.xml"));

        DisplayPluginUpdatesMojo mojo = createMojo();
        mojo.execute();

        List<String> output = Files.readAllLines(outputPath);
        assertThat(output, hasItem(containsString("Using the minimum version of Maven: 3.3.9")));
    }

    @Test
    public void testPrerequisitesLesserThanEnforcer() throws Exception {
        Files.copy(
                Paths.get(
                        "src/test/resources/org/codehaus/mojo/display-plugin-updates/issue-990/prerequisites-lesser.xml"),
                tempDir.resolve("pom.xml"));

        DisplayPluginUpdatesMojo mojo = createMojo();
        mojo.execute();

        List<String> output = Files.readAllLines(outputPath);
        assertThat(output, hasItem(containsString("Using the minimum version of Maven: 3.3.9")));
    }
}
