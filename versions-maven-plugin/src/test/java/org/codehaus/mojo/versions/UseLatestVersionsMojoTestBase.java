package org.codehaus.mojo.versions;

import javax.xml.stream.XMLStreamException;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.eclipse.aether.RepositorySystem;
import org.hamcrest.core.Is;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;

public abstract class UseLatestVersionsMojoTestBase {
    protected UseLatestVersionsMojoBase mojo;

    protected TestChangeRecorder changeRecorder;

    @Mock
    protected Log log;

    protected PomHelper pomHelper;

    protected ArtifactFactory artifactFactory;

    @Mock
    protected ExpressionEvaluator expressionEvaluator;

    protected abstract UseLatestVersionsMojoBase createMojo() throws IllegalAccessException, MojoExecutionException;

    @Before
    public void setUp() throws Exception {
        openMocks(this);
        changeRecorder = new TestChangeRecorder();
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        pomHelper = new PomHelper(artifactFactory, expressionEvaluator);
        mojo = createMojo();
        mojo.mojoExecution = Mockito.mock(MojoExecution.class);
    }

    protected RepositorySystem createRepositorySystem() {
        return mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("dependency-artifact", new String[] {
                    "1.1.1-SNAPSHOT", "1.1.0", "1.1.0-SNAPSHOT", "1.0.0", "1.0.0-beta", "1.0.0-SNAPSHOT", "0.9.0"
                });
                put("poison-artifact", new String[] {
                    "1.1.1.1-SNAPSHOT", "1.1.1.0", "1.1.1.0-SNAPSHOT", "1.0.0.0", "1.0.0.0-SNAPSHOT", "0.9.0.0"
                });
                put("other-artifact", new String[] {"1.0", "2.0", "2.0-SNAPSHOT"});
            }
        });
    }

    protected void tryUpdate()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setDependencyVersion(any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(true);
            pomHelper
                    .when(() -> PomHelper.getRawModel(any(MavenProject.class)))
                    .thenReturn(mojo.getProject().getModel());
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), anyString()))
                    .thenReturn(true);
            mojo.update(null);
        }
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "ignoredVersions", new HashSet<String>() {
            {
                add("1.0.0");
                add("1.0.0-beta");
                add("1.1.0");
                add("1.0.0-SNAPSHOT");
                add("1.1.0-SNAPSHOT");
                add("1.1.1-SNAPSHOT");
            }
        });

        tryUpdate();
        assertThat(changeRecorder.getChanges(), Is.is(empty()));
    }

    protected void testIncludeFilter(String expectedNewVersion)
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Arrays.asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("dependency-artifact")
                                .withVersion("0.9.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("other-artifact")
                                .withVersion("1.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build()));
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:other-artifact"});

        tryUpdate();
        assertThat(changeRecorder.getChanges(), hasSize(1));
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "other-artifact", "1.0", expectedNewVersion)));
    }

    @Test
    public void testIncludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        testIncludeFilter("2.0");
    }

    protected void testExcludeFilter(String expectedNewVersion)
            throws IllegalAccessException, MojoExecutionException, XMLStreamException, MojoFailureException,
                    VersionRetrievalException {
        mojo.getProject()
                .getModel()
                .setDependencies(Arrays.asList(
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("dependency-artifact")
                                .withVersion("0.9.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build(),
                        DependencyBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("other-artifact")
                                .withVersion("1.0")
                                .withType("pom")
                                .withClassifier("default")
                                .withScope(SCOPE_COMPILE)
                                .build()));
        setVariableValueToObject(mojo, "processDependencies", true);
        setVariableValueToObject(mojo, "excludes", new String[] {"default-group:other-artifact"});

        tryUpdate();
        assertThat(changeRecorder.getChanges(), hasSize(1));
        assertThat(
                changeRecorder.getChanges(),
                not(hasItem(new DefaultDependencyVersionChange(
                        "default-group", "other-artifact", "1.0", expectedNewVersion))));
    }

    @Test
    public void testExcludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        testExcludeFilter("2.0");
    }
}
