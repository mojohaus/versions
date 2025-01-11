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

import java.util.HashMap;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.hamcrest.Matchers;
import org.junit.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.mockito.Mockito.mock;

public class UseLatestReleasesMojoTest extends UseLatestVersionsMojoTestBase {

    @Override
    protected UseLatestReleasesMojo createMojo() throws IllegalAccessException, MojoExecutionException {
        return new UseLatestReleasesMojo(artifactFactory, createRepositorySystem(), null, changeRecorder.asTestMap()) {
            {
                reactorProjects = emptyList();
                mojoExecution = mock(MojoExecution.class);
                MavenProject project = new MavenProject() {
                    {
                        setModel(new Model() {
                            {
                                setGroupId("default-group");
                                setArtifactId("project-artifact");
                                setVersion("1.0.0-SNAPSHOT");

                                setDependencies(singletonList(DependencyBuilder.newBuilder()
                                        .withGroupId("default-group")
                                        .withArtifactId("dependency-artifact")
                                        .withVersion("0.9.0")
                                        .withScope(SCOPE_COMPILE)
                                        .withType("jar")
                                        .withClassifier("default")
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
    public void testDontUpgradeToBeta()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", false);

        tryUpdate();
        assertThat(changeRecorder.getChanges(), Matchers.empty());
    }

    @Test
    public void testAllowDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
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
        setVariableValueToObject(mojo, "allowDowngrade", true);

        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "artifactA",
                        "1.0.1-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testDisallowDowngrade()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
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

        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }
}
