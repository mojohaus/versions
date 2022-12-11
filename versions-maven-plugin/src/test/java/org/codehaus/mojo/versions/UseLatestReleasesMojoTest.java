package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.HashMap;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
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
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

public class UseLatestReleasesMojoTest {
    private UseLatestReleasesMojo mojo;
    private TestChangeRecorder changeRecorder;

    @Before
    public void setUp() throws Exception {
        RepositorySystem repositorySystemMock = mock(RepositorySystem.class);
        when(repositorySystemMock.createDependencyArtifact(any(Dependency.class)))
                .thenAnswer(invocation -> {
                    Dependency dependency = invocation.getArgument(0);
                    return new DefaultArtifact(
                            dependency.getGroupId(),
                            dependency.getArtifactId(),
                            dependency.getVersion(),
                            dependency.getScope(),
                            dependency.getType(),
                            dependency.getClassifier() != null ? dependency.getClassifier() : "default",
                            new DefaultArtifactHandlerStub("default"));
                });

        org.eclipse.aether.RepositorySystem aetherRepositorySystem =
                mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("dependency-artifact", new String[] {"0.9.0", "1.0.0-beta"});
                    }
                });

        changeRecorder = new TestChangeRecorder();

        mojo =
                new UseLatestReleasesMojo(
                        repositorySystemMock, aetherRepositorySystem, null, changeRecorder.asTestMap()) {
                    {
                        reactorProjects = emptyList();
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
                    }
                };
    }

    @Test
    public void testDontUpgradeToBeta()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "allowSnapshots", false);
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", false);

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), Matchers.empty());
    }
}
