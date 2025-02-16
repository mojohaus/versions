package org.codehaus.mojo.versions.utils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static java.util.Optional.of;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ArtifactFactoryTest {
    @Mock
    ArtifactHandlerManager artifactHandlerManager;

    @BeforeEach
    public void beforeEach() {
        when(artifactHandlerManager.getArtifactHandler(any())).thenAnswer(i -> of(i.getArgument(0))
                .map(type -> new DefaultArtifactHandler(String.valueOf(type)) {
                    {
                        if (type.equals("maven-plugin")) {
                            setExtension("jar");
                        }
                    }
                })
                .get());
    }

    @Test
    void createPluginArtifact() throws Exception {
        ArtifactFactory artifactFactory = new ArtifactFactory(artifactHandlerManager);
        Artifact artifact = artifactFactory.createMavenPluginArtifact("groupId", "artifactId", "version");
        assertThat("groupId", equalTo(artifact.getGroupId()));
        assertThat("artifactId", equalTo(artifact.getArtifactId()));
        assertThat("version", equalTo(artifact.getVersion()));
        assertThat("maven-plugin", equalTo(artifact.getType()));
        assertThat("runtime", equalTo(artifact.getScope()));
        assertThat("jar", equalTo(artifact.getArtifactHandler().getExtension()));
    }
}
