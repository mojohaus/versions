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

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.apache.maven.api.plugin.testing.Basedir;
import org.apache.maven.api.plugin.testing.InjectMojo;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContainsInOrder;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotContains;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Plugin version updates of the {@code use-latest-*} and {@code use-next-*} goals with {@code processPlugins}.
 * In the test repository, {@code dummy-maven-plugin} 3.1 requires Maven 3.8.4 and the older versions Maven 2.0.6.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/use-plugin-versions")
class UsePluginVersionsTest {

    private static ProjectBuilder projectBuilder() throws Exception {
        ProjectBuilder projectBuilder = mock(ProjectBuilder.class);
        when(projectBuilder.build(any(Artifact.class), anyBoolean(), any(ProjectBuildingRequest.class)))
                .thenAnswer(invocation -> {
                    Artifact artifact = invocation.getArgument(0);
                    Prerequisites prerequisites = new Prerequisites();
                    prerequisites.setMaven("3.1".equals(artifact.getVersion()) ? "3.8.4" : "2.0.6");
                    MavenProject project = mock(MavenProject.class);
                    when(project.getPrerequisites()).thenReturn(prerequisites);
                    ProjectBuildingResult result = mock(ProjectBuildingResult.class);
                    when(result.getProject()).thenReturn(project);
                    return result;
                });
        return projectBuilder;
    }

    private String run(AbstractVersionsUpdaterMojo mojo) throws Exception {
        mojo.repositorySystem = ItRepository.repositorySystem();
        setVariableValueToObject(mojo, "projectBuilder", projectBuilder());
        mojo.execute();
        return new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "sections/pom.xml")
    void updatesEverySectionUpToTheMinimumMaven(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContainsInOrder(
                pom,
                "<pluginManagement>",
                "<version>3.0</version>",
                "<artifactId>dummy-maven-plugin</artifactId>",
                "<version>3.0</version>",
                "<reporting>",
                "<version>${dummy.plugin.version}</version>",
                "<profile>",
                "<version>3.0</version>");
        assertTextContains(pom, "<dummy.plugin.version>1.0</dummy.plugin.version>");
        assertTextNotContains(pom, "<version>3.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "no-floor/pom.xml")
    void usesTheGoalsOwnMinimumWithoutAProjectMinimum(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>3.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "disabled/pom.xml")
    void leavesPluginsAloneByDefault(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "no-major/pom.xml")
    void respectsSegmentLimits(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>2.1</version>");
    }

    @Test
    @InjectMojo(goal = "use-latest-releases", pom = "excluded/pom.xml")
    void respectsExcludes(AbstractVersionsUpdaterMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<version>1.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "sections/pom.xml")
    void nextReleasesTakesTheNextCompatibleVersion(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContainsInOrder(
                pom,
                "<pluginManagement>",
                "<version>2.1</version>",
                "<artifactId>dummy-maven-plugin</artifactId>",
                "<version>2.0</version>",
                "<profile>",
                "<version>3.0</version>");
    }

    @Test
    @InjectMojo(goal = "use-next-releases", pom = "chained/pom.xml")
    void doesNotChainUpdatesOfOnePlugin(AbstractVersionsUpdaterMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContainsInOrder(
                pom,
                "<pluginManagement>",
                "<version>2.0</version>",
                "<artifactId>dummy-maven-plugin</artifactId>",
                "<version>2.1</version>",
                "<profile>",
                "<version>3.0</version>");
    }
}
