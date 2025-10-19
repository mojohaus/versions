package org.codehaus.mojo.versions;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.codehaus.mojo.versions.utils.TestVersionChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static java.util.Collections.emptyList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;

public class UseLatestVersionsRaceConditionTest {
    protected UseLatestVersionsMojoBase mojo;

    protected TestVersionChangeRecorder changeRecorder;

    @Mock
    protected Log log;

    protected PomHelper pomHelper;

    protected ArtifactFactory artifactFactory;

    @Mock
    protected ExpressionEvaluator expressionEvaluator;

    @Before
    public void setUp() throws Exception {
        openMocks(this);
        changeRecorder = new TestVersionChangeRecorder();
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        pomHelper = new PomHelper(artifactFactory, expressionEvaluator);
        mojo = createMojo();
        mojo.mojoExecution = Mockito.mock(MojoExecution.class);
    }

    private static String getRulesString() {
        return "<ruleset>\n"
                + "  <ignoreVersions>\n"
                + "    <ignoreVersion type=\"regex\">.*-alpha</ignoreVersion>\n"
                + "    <ignoreVersion type=\"regex\">.*-beta</ignoreVersion>\n"
                + "    <ignoreVersion type=\"illegalType\">illegalVersion</ignoreVersion>\n"
                + "  </ignoreVersions>\n"
                + "  <rules>\n"
                + "    <rule groupId=\"com.mycompany.maven\">\n"
                + "      <ignoreVersions>\n"
                + "        <ignoreVersion>one</ignoreVersion>\n"
                + "        <ignoreVersion>two</ignoreVersion>\n"
                + "        <ignoreVersion>1.2.0</ignoreVersion>\n"
                + "        <ignoreVersion type=\"illegalType\">illegalVersion</ignoreVersion>\n"
                + "      </ignoreVersions>\n"
                + "    </rule>\n"
                + "  </rules>\n"
                + "</ruleset>";
    }

    protected UseLatestVersionsMojoBase createMojo() throws IllegalAccessException, MojoExecutionException {
        AtomicBoolean inUse = new AtomicBoolean(false);
        Wagon testWagon = new TestWagonStub(file -> {
            try {
                assertThat("Resource is in use", inUse.compareAndSet(false, true), is(true));
                Files.write(file.toPath(), getRulesString().getBytes(StandardCharsets.UTF_8));
                Thread.sleep(200);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                inUse.set(false);
            }
        });
        return new UseLatestVersionsMojo(
                artifactFactory,
                MockUtils.mockAetherRepositorySystem(),
                Collections.singletonMap("proto", testWagon),
                TestVersionChangeRecorder.asTestMap()) {
            {
                changeRecorder = (TestVersionChangeRecorder) getChangeRecorder();
                reactorProjects = emptyList();
                MavenProject project = new MavenProject() {
                    {
                        setModel(new Model() {
                            {
                                setGroupId("default-group");
                                setArtifactId("project-artifact");
                                setVersion("1.0.0-SNAPSHOT");

                                setDependencies(Arrays.asList(
                                        DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("artifactA")
                                                .withVersion("1.0.0")
                                                .withType("pom")
                                                .withClassifier("default")
                                                .withScope(SCOPE_COMPILE)
                                                .build(),
                                        DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("artifactB")
                                                .withVersion("1.0.0")
                                                .withType("pom")
                                                .withClassifier("default")
                                                .withScope(SCOPE_COMPILE)
                                                .build(),
                                        DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("artifactC")
                                                .withVersion("1.0.0")
                                                .withType("pom")
                                                .withClassifier("default")
                                                .withScope(SCOPE_COMPILE)
                                                .build()));
                            }
                        });
                    }
                };
                setProject(project);
                session = mockMavenSession();
                setVariableValueToObject(this, "rulesUri", "proto://localhost/rules.xml");
            }
        };
    }

    @Test
    public void testConcurrency() throws Exception {
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

        assertThat(
                changeRecorder.getChanges(),
                allOf(
                        hasItem(new DependencyVersionChange()
                                .withKind(DependencyChangeKind.DEPENDENCY_UPDATE)
                                .withGroupId("default-group")
                                .withArtifactId("artifactA")
                                .withOldVersion("1.0.0")
                                .withNewVersion("2.0.0")),
                        hasItem(new DependencyVersionChange()
                                .withKind(DependencyChangeKind.DEPENDENCY_UPDATE)
                                .withGroupId("default-group")
                                .withArtifactId("artifactB")
                                .withOldVersion("1.0.0")
                                .withNewVersion("1.1.0"))));
    }
}
