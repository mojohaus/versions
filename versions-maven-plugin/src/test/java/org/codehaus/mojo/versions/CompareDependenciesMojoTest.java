package org.codehaus.mojo.versions;

import java.io.File;
import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.project.ProjectBuildingResult;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.Rule;
import org.junit.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class CompareDependenciesMojoTest extends AbstractMojoTestCase {

    @Rule
    public final MojoRule mojoRule = new MojoRule(this);

    @Test
    public void testCompareVersionlessDependencies() throws Exception {
        CompareDependenciesMojo mojo = (CompareDependenciesMojo) mojoRule.lookupConfiguredMojo(
                new File("src/test/resources/org/codehaus/mojo/compare-dependencies/versionless"),
                "compare-dependencies");
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
