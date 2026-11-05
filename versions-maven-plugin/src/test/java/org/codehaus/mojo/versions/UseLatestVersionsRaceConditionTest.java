package org.codehaus.mojo.versions;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.eclipse.aether.spi.connector.transport.GetTask;
import org.eclipse.aether.spi.connector.transport.Transporter;
import org.eclipse.aether.spi.connector.transport.TransporterProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static java.util.Collections.emptyList;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;

public class UseLatestVersionsRaceConditionTest {
    protected UseLatestVersionsMojoBase mojo;

    protected TestChangeRecorder changeRecorder;

    @Mock
    protected Log log;

    protected PomHelper pomHelper;

    protected ArtifactFactory artifactFactory;

    @Mock
    protected ExpressionEvaluator expressionEvaluator;

    @BeforeEach
    public void setUp() throws Exception {
        openMocks(this);
        changeRecorder = new TestChangeRecorder();
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

    protected UseLatestVersionsMojoBase createMojo() throws Exception {
        AtomicBoolean inUse = new AtomicBoolean(false);
        Transporter transporter = Mockito.mock(Transporter.class);
        Mockito.doAnswer(invocation -> {
                    GetTask task = invocation.getArgument(0);
                    try {
                        assertTrue(inUse.compareAndSet(false, true), "Resource is in use");
                        try (OutputStream os = task.newOutputStream()) {
                            os.write(getRulesString().getBytes(StandardCharsets.UTF_8));
                        }
                        Thread.sleep(200);
                    } finally {
                        inUse.set(false);
                    }
                    return null;
                })
                .when(transporter)
                .get(any(GetTask.class));
        TransporterProvider transporterProvider = Mockito.mock(TransporterProvider.class);
        Mockito.when(transporterProvider.newTransporter(any(), any())).thenReturn(transporter);
        return new UseLatestVersionsMojo(
                artifactFactory,
                MockUtils.mockAetherRepositorySystem(),
                null,
                null,
                transporterProvider,
                changeRecorder.asTestMap()) {
            {
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

        assertTrue(
                changeRecorder
                        .getChanges()
                        .containsAll(Arrays.asList(
                                new DefaultDependencyVersionChange(
                                        "default-group", "artifactA", "default", "1.0.0", "2.0.0"),
                                new DefaultDependencyVersionChange(
                                        "default-group", "artifactB", "default", "1.0.0", "1.1.0"))),
                () -> "changes: " + changeRecorder.getChanges());
    }
}
