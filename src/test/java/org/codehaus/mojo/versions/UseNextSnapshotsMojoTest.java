package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import javax.xml.stream.XMLStreamException;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.when;


@RunWith(MockitoJUnitRunner.class)
public class UseNextSnapshotsMojoTest {
    @Mock
    private MavenProject mavenProject;
    @Mock
    private ModifiedPomXMLEventReader pom;
    @Mock
    private DependencyManagement dependencyManagement;
    @Mock
    private Dependency dependency;
    @Mock
    private VersionsHelper versionsHelper;
    @Mock
    private Artifact artifact;
    @Mock
    private Model model;

    private UseNextSnapshotsMojo useNextSnapshotsMojo;

    private final String currentVersion = "1.2.3";
    private final String snapshotVersion = "1.2.4-SNAPSHOT";

    @Before
    public void setUp() throws Exception {
        useNextSnapshotsMojo = new UseNextSnapshotsMojo();
        useNextSnapshotsMojo.project = mavenProject;
        setProperty(AbstractVersionsUpdaterMojo.class, "helper", versionsHelper);
        when(mavenProject.getModel()).thenReturn(model);
        when(model.getProperties()).thenReturn(new Properties());
        VersionComparator versionComparator = new MavenVersionComparator();
        when(mavenProject.getDependencyManagement()).thenReturn(dependencyManagement);
        when(dependencyManagement.getDependencies()).thenReturn(Collections.singletonList(dependency));
        final List<ArtifactVersion> artifactVersionList = Arrays.asList((ArtifactVersion) new DefaultArtifactVersion(currentVersion),
                new DefaultArtifactVersion(snapshotVersion));
        final ArtifactVersions artifactVersions = new ArtifactVersions(artifact, artifactVersionList, versionComparator);
        when(versionsHelper.lookupArtifactVersions(any(Artifact.class), anyBoolean())).thenReturn(artifactVersions);
    }

    @Test
    public void update() throws MojoFailureException, XMLStreamException, MojoExecutionException {
        useNextSnapshotsMojo.update(pom);
    }

    /**
     * Cannot verify it any further than it doesn't throw any exceptions due to use of static PomHelper.setDependencyVersion
     * @throws MojoFailureException
     * @throws XMLStreamException
     * @throws MojoExecutionException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    @Test
    public void updateWithDependencyManagement() throws MojoFailureException, XMLStreamException, MojoExecutionException, NoSuchFieldException, IllegalAccessException {
        setProperty(AbstractVersionsDependencyUpdaterMojo.class, "processDependencyManagement", true);
        when(dependency.getVersion()).thenReturn(currentVersion);
        useNextSnapshotsMojo.update(pom);
    }

    /**
     * Cannot verify it any further than it doesn't throw NPE due to use of static PomHelper.setDependencyVersion
     * @throws MojoFailureException
     * @throws XMLStreamException
     * @throws MojoExecutionException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    @Test
    public void updateWithDependencyManagementWithoutVersion() throws MojoFailureException, XMLStreamException, MojoExecutionException, NoSuchFieldException, IllegalAccessException {
        setProperty(AbstractVersionsDependencyUpdaterMojo.class, "processDependencyManagement", true);
        useNextSnapshotsMojo.update(pom);
    }

    private <T> void setProperty(Class<? super UseNextSnapshotsMojo> mojoClass, String name, T value) throws NoSuchFieldException, IllegalAccessException {
        final Field field = mojoClass.getDeclaredField(name);
        field.setAccessible(true);
        field.set(useNextSnapshotsMojo, value);
    }
}