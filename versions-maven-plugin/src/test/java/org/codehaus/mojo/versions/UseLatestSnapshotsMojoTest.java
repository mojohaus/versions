package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;

public class UseLatestSnapshotsMojoTest extends UseLatestVersionsMojoTestBase {
    @Override
    protected UseLatestVersionsMojoBase createMojo() throws IllegalAccessException, MojoExecutionException {
        return new UseLatestSnapshotsMojo(artifactFactory, createRepositorySystem(), null, changeRecorder.asTestMap()) {
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
                allowMajorUpdates = true;
                allowMinorUpdates = true;
                allowIncrementalUpdates = true;
                setVariableValueToObject(this, "processDependencyManagement", false);
            }
        };
    }

    @Test
    @Override
    public void testIncludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        testIncludeFilter("2.0-SNAPSHOT");
    }

    @Test
    @Override
    public void testExcludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        testExcludeFilter("2.0-SNAPSHOT");
    }
}
