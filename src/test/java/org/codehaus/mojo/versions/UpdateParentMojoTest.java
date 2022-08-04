package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifact;
import org.junit.Test;

import javax.xml.stream.XMLStreamException;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class UpdateParentMojoTest {

    @Test
    public void testArtifactIdDoesNotExist() throws MojoExecutionException, XMLStreamException, MojoFailureException {
        UpdateParentMojo mojo = new UpdateParentMojo() {
            {
                project = new MavenProject();
                project.setParent(new MavenProject());
                reactorProjects = Collections.emptyList();
                forceUpdate = true;

                artifactFactory = mock(ArtifactFactory.class);
                when(artifactFactory.createDependencyArtifact(anyString(), anyString(), any(VersionRange.class),
                        anyString(), anyString(), anyString()))
                        .thenReturn(new ProjectArtifact(project));
            }

            protected ArtifactVersion findLatestVersion(Artifact artifact, VersionRange versionRange,
                                                        Boolean allowingSnapshots, boolean usePluginRepositories) {
                return null;
            }
        };
        mojo.update(null);
    }
}
