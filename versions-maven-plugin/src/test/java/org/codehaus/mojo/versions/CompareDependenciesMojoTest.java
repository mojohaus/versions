package org.codehaus.mojo.versions;

import java.nio.file.Paths;
import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ProjectMojoTest
public class CompareDependenciesMojoTest {

    protected ProjectMojos mojos;

    @BeforeEach
    public void injectMojos(ProjectMojos mojos) {
        this.mojos = mojos;
    }

    @Test
    public void testCompareVersionlessDependencies() throws Exception {
        CompareDependenciesMojo mojo = (CompareDependenciesMojo) mojos.lookup(
                "compare-dependencies",
                Paths.get("src/test/resources/org/codehaus/mojo/compare-dependencies/versionless"));
        ProjectBuildingResult result = mock(ProjectBuildingResult.class);
        when(result.getProblems()).thenReturn(Collections.emptyList());
        when(result.getProject()).thenReturn(new MavenProject(new Model() {
            {
                setDependencyManagement(new DependencyManagement() {
                    {
                        setDependencies(Collections.singletonList(DependencyBuilder.newBuilder()
                                .withGroupId("defaultGroup")
                                .withArtifactId("artifactA")
                                .withScope("test")
                                .withVersion("1.0.0")
                                .build()));
                    }
                });
            }
        }));

        ProjectBuilder projectBuilder = mock(ProjectBuilder.class);
        when(projectBuilder.build(any(Artifact.class), anyBoolean(), any(ProjectBuildingRequest.class)))
                .thenReturn(result);
        setVariableValueToObject(mojo, "projectBuilder", projectBuilder);
        setVariableValueToObject(mojo, "remotePom", "defaultGroup:other-artifact:1.0.0");

        mojo.execute();
    }
}
