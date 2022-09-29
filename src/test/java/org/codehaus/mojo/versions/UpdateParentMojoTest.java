package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.Collections;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.ProjectArtifact;
import org.apache.maven.repository.RepositorySystem;
import org.junit.Test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class UpdateParentMojoTest
{

    @Test
    public void testArtifactIdDoesNotExist() throws MojoExecutionException, XMLStreamException, MojoFailureException
    {
        UpdateParentMojo mojo = new UpdateParentMojo()
        {
            {
                project = new MavenProject();
                project.setParent( new MavenProject() );
                reactorProjects = Collections.emptyList();
                forceUpdate = true;

                repositorySystem = mock( RepositorySystem.class );
                artifactMetadataSource = mock( ArtifactMetadataSource.class );
                when( repositorySystem.createDependencyArtifact( any() ) )
                    .thenReturn( new ProjectArtifact( project ) );
            }

            protected ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                         Boolean allowingSnapshots, boolean usePluginRepositories )
            {
                return null;
            }
        };
        mojo.update( null );
    }
}
