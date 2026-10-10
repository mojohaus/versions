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

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class UseLatestVersionsMojoTest extends UseLatestVersionsMojoTestBase {

    @Override
    protected UseLatestVersionsMojoBase createMojo() throws IllegalAccessException, MojoExecutionException {
        return new UseLatestVersionsMojo(artifactFactory, createRepositorySystem(), null, changeRecorder.asTestMap()) {
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
                                        .setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
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
                setVariableValueToObject(this, "processDependencyManagement", false);
            }
        };
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

        tryUpdate();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "default", "1.1.1-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
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

        tryUpdate();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "default", "1.1.0-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
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

        tryUpdate();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "default", "1.1.1-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
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

        tryUpdate();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "default", "1.1.1-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
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

        tryUpdate();
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "1.1.1-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
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

        tryUpdate();
        // So, the regular update should take place despite an irregular, or — if I may — "poison", dependency
        // being present in the dependency list
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "dependency-artifact", "default", "1.1.1-SNAPSHOT", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testDontUpgradeToPreReleaseByDefault()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("pre-release-artifact")
                        .withVersion("1.0.0")
                        .withType("pom")
                        .withClassifier("default")
                        .withScope(SCOPE_COMPILE)
                        .build()));

        // Use all defaults to test that allowPreReleases=false (default) filters out pre-releases

        tryUpdate();
        // Should upgrade to 1.1.0, not 1.2.0-beta1 (pre-release should be filtered out by default)
        // Available: 1.2.0-beta1, 1.1.0, 1.0.0
        // With allowPreReleases=false (default), beta is excluded and 1.1.0 is selected
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "pre-release-artifact", "default", "1.0.0", "1.1.0")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }

    @Test
    public void testUpgradeToPreReleaseWhenAllowed()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
                        .withGroupId("default-group")
                        .withArtifactId("pre-release-artifact")
                        .withVersion("1.0.0")
                        .withType("pom")
                        .withClassifier("default")
                        .withScope(SCOPE_COMPILE)
                        .build()));

        setVariableValueToObject(mojo, "allowPreReleases", true);

        tryUpdate();
        // When allowPreReleases is true, beta version can be selected
        // Available: 1.2.0-beta1, 1.1.0, 1.0.0
        // Beta version 1.2.0-beta1 is highest and will be selected
        assertTrue(
                changeRecorder
                        .getChanges()
                        .contains(new DefaultDependencyVersionChange(
                                "default-group", "pre-release-artifact", "default", "1.0.0", "1.2.0-beta1")),
                () -> String.valueOf(changeRecorder.getChanges()));
    }
}
