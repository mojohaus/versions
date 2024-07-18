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

import javax.xml.stream.XMLStreamException;

import java.util.List;
import java.util.function.UnaryOperator;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.resolution.VersionRequest;
import org.eclipse.aether.resolution.VersionResolutionException;
import org.eclipse.aether.resolution.VersionResult;
import org.junit.Test;
import org.mockito.MockedStatic;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link LockSnapshotsMojo}
 */
public class LockSnapshotsMojoTest {

    private LockSnapshotsMojo createMojo(RepositorySystem repositorySystem) {
        return new LockSnapshotsMojo(null, repositorySystem, null, null) {
            {
                reactorProjects = emptyList();
                project = new MavenProject(new Model() {
                    {
                        setGroupId("default-group");
                        setArtifactId("default-project");
                        setVersion("1.0-SNAPSHOT");
                    }

                    @Override
                    public void setDependencies(List<Dependency> dependencies) {
                        super.setDependencies(singletonList(
                                DependencyBuilder.dependencyWith("default-group", "default-artifact", "1.0-SNAPSHOT")));
                    }
                });
                session = MockUtils.mockMavenSession();
            }
        };
    }

    private RepositorySystem mockRepositorySystem(UnaryOperator<String> versionProducer)
            throws VersionResolutionException {
        RepositorySystem repositorySystem = mock(RepositorySystem.class);
        when(repositorySystem.resolveVersion(any(), any())).then(i -> {
            VersionRequest request = i.getArgument(1);
            return new VersionResult(request)
                    .setVersion(versionProducer.apply(request.getArtifact().getVersion()));
        });
        return repositorySystem;
    }

    @Test
    public void testNoTimestampedDependencyFoundNull()
            throws XMLStreamException, MojoExecutionException, VersionResolutionException {
        RepositorySystem repositorySystem = mockRepositorySystem(v -> null);

        LockSnapshotsMojo mojo = createMojo(repositorySystem);
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenThrow(new RuntimeException("Not supposed to modify the dependency"));
            mojo.lockSnapshots(null, mojo.project.getDependencies());
        }
    }

    @Test
    public void testNoTimestampedDependencyFoundSameVersion()
            throws XMLStreamException, MojoExecutionException, VersionResolutionException {
        RepositorySystem repositorySystem = mockRepositorySystem(UnaryOperator.identity());

        LockSnapshotsMojo mojo = createMojo(repositorySystem);
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenThrow(new RuntimeException("Not supposed to modify the dependency"));
            mojo.lockSnapshots(null, mojo.project.getDependencies());
        }
    }

    @Test
    public void testNoTimestampedParentFoundNull()
            throws XMLStreamException, MojoExecutionException, VersionResolutionException {
        RepositorySystem repositorySystem = mockRepositorySystem(v -> null);

        LockSnapshotsMojo mojo = createMojo(repositorySystem);
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenThrow(new RuntimeException("Not supposed to modify the parent"));
            mojo.lockParentSnapshot(
                    null,
                    new MavenProject(new Model() {
                        {
                            setGroupId("default-group");
                            setArtifactId("default-parent");
                            setVersion("1.0-SNAPSHOT");
                        }
                    }) {
                        {
                            setArtifact(new DefaultArtifact(
                                    "default-group",
                                    "default-parent",
                                    "1.0-SNAPSHOT",
                                    "compile",
                                    "pom",
                                    null,
                                    new DefaultArtifactHandlerStub("jar")));
                        }
                    });
        }
    }

    @Test
    public void testNoTimestampedParentFoundSameVersion()
            throws XMLStreamException, MojoExecutionException, VersionResolutionException {
        RepositorySystem repositorySystem = mockRepositorySystem(UnaryOperator.identity());

        LockSnapshotsMojo mojo = createMojo(repositorySystem);
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenThrow(new RuntimeException("Not supposed to modify the parent"));
            mojo.lockParentSnapshot(
                    null,
                    new MavenProject(new Model() {
                        {
                            setGroupId("default-group");
                            setArtifactId("default-parent");
                            setVersion("1.0-SNAPSHOT");
                        }
                    }) {
                        {
                            setArtifact(new DefaultArtifact(
                                    "default-group",
                                    "default-parent",
                                    "1.0-SNAPSHOT",
                                    "compile",
                                    "pom",
                                    null,
                                    new DefaultArtifactHandlerStub("jar")));
                        }
                    });
        }
    }
}
