package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.codehaus.mojo.versions.utils.TestVersionChangeRecorder;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.eclipse.aether.RepositorySystem;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;

import static java.util.Collections.singleton;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.stringContainsInOrder;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.openMocks;

public class DisplayParentUpdatesMojoTest {
    private DisplayParentUpdatesMojo mojo;

    private static ArtifactHandlerManager artifactHandlerManager;

    private static RepositorySystem repositorySystem;

    private static ArtifactFactory artifactFactory;

    @Mock
    private static ExpressionEvaluator expressionEvaluator;

    @Mock
    private static Log log;

    private Path tempDir;

    private Path tempFile;

    @BeforeClass
    public static void setUpStatic() throws MojoExecutionException {
        artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("parent-artifact", new String[] {"0.9.0", "1.0.0", "1.0.1-SNAPSHOT"});
                put("issue-670-artifact", new String[] {"0.0.1-1", "0.0.1-1-impl-SNAPSHOT"});
                put("dummy-parent2", new String[] {"1.0", "2.0", "3.0", "3.0-alpha-1", "3.0-beta-1"});
                put("test-incremental", new String[] {"1.0.0", "1.1.0", "1.1.1", "2.0.0"});
                put("unknown-artifact", new String[0]);
            }
        });
    }

    @Before
    public void setUp() throws IllegalAccessException, IOException, MojoExecutionException {
        tempDir = TestUtils.createTempDir("display-property-updates");
        tempFile = Files.createTempFile(tempDir, "output", "");
        mojo =
                new DisplayParentUpdatesMojo(
                        artifactFactory, repositorySystem, null, TestVersionChangeRecorder.asTestMap()) {
                    {
                        setProject(createProject());
                        reactorProjects = Collections.emptyList();
                        session = mockMavenSession();
                        mojoExecution = mock(MojoExecution.class);
                    }
                };
        mojo.outputFile = tempFile.toFile();
        mojo.setPluginContext(new HashMap<>());
        openMocks(this);
    }

    @After
    public void tearDown() throws Exception {
        TestUtils.tearDownTempDir(tempDir);
    }

    private MavenProject createProject() {
        return new MavenProject() {
            {
                setModel(new Model() {
                    {
                        setGroupId("default-group");
                        setArtifactId("project-artifact");
                        setVersion("1.0.1-SNAPSHOT");
                    }
                });

                setParent(new MavenProject() {
                    {
                        setGroupId("default-group");
                        setArtifactId("parent-artifact");
                        setVersion("1.0.1-SNAPSHOT");
                    }
                });
            }
        };
    }

    @Test
    public void testArtifactIdDoesNotExist()
            throws MojoExecutionException, InvalidVersionSpecificationException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("unknown-artifact");
                setVersion("1.0.1-SNAPSHOT");
            }
        });

        Artifact artifact = new DefaultArtifact(
                "default-group",
                "unknown-artifact",
                "1.0.1-SNAPSHOT",
                SCOPE_COMPILE,
                "pom",
                "default",
                new DefaultArtifactHandlerStub("default"));
        assertThat(
                mojo.findLatestVersion(artifact, VersionRange.createFromVersionSpec("1.0.1-SNAPSHOT"), null, false),
                is(nullValue()));
    }

    @Test
    public void testParentDowngradeAllowed() throws MojoExecutionException, MojoFailureException, IOException {
        mojo.allowDowngrade = true;
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder(
                        "The parent project has a newer version:",
                        "default-group:parent-artifact",
                        "1.0.1-SNAPSHOT -> 1.0.0"));
    }

    @Test
    public void testParentDowngradeForbidden() throws MojoExecutionException, MojoFailureException, IOException {
        mojo.allowDowngrade = false;
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder(
                        "The parent project is the latest version:",
                        "default-group:parent-artifact",
                        "1.0.1-SNAPSHOT"));
    }

    @Test
    public void testParentDowngradeAllowedWithRange()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.allowDowngrade = true;
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
            }
        });

        ArtifactVersion newVersion = mojo.resolveTargetVersion("[1.0.1-SNAPSHOT,)");
        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("1.0.0"));
    }

    @Test
    public void testParentDowngradeForbiddenWithRange()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.allowDowngrade = false;
        ArtifactVersion newVersion = mojo.resolveTargetVersion("[1.0.1-SNAPSHOT,)");
        assertThat(newVersion, nullValue());
    }

    @Test
    public void testAllowSnapshots()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.allowSnapshots = true;
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("issue-670-artifact");
            }
        });

        ArtifactVersion newVersion = mojo.resolveTargetVersion("0.0.1-1");
        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("0.0.1-1-impl-SNAPSHOT"));
    }

    @Test
    public void testAllowSnapshotsWithParentVersion() throws MojoExecutionException, MojoFailureException, IOException {
        mojo.allowSnapshots = true;
        mojo.parentVersion = "0.0.1-1-impl-SNAPSHOT";
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder(
                        "The parent project is the latest version:",
                        "default-group:parent-artifact",
                        "0.0.1-1-impl-SNAPSHOT"));
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, IllegalAccessException, VersionRetrievalException,
                    InvalidVersionSpecificationException, InvalidSegmentException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
            }
        });
        setVariableValueToObject(mojo, "ignoredVersions", singleton("1.0.0"));
        assertThat(mojo.resolveTargetVersion("0.9.0"), nullValue());
    }

    @Test
    public void testSkipResolutionDowngradeUnknownVersion()
            throws MojoExecutionException, MojoFailureException, IOException {
        testSkipResolution("0.8.0");
    }

    @Test
    public void testSkipResolutionDowngrade() throws MojoExecutionException, MojoFailureException, IOException {
        testSkipResolution("0.9.0");
    }

    @Test
    public void testSkipResolutionUpgradeUnknownVersion()
            throws MojoExecutionException, MojoFailureException, IOException {
        testSkipResolution("2.0.0");
    }

    private void testSkipResolution(String version) throws IOException, MojoExecutionException, MojoFailureException {
        mojo.parentVersion = version;
        mojo.skipResolution = true;
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("1.0.0");
            }
        });
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder(
                        "The parent project is the latest version:", "default-group:parent-artifact", version));
    }

    @Test
    public void testShouldUpgradeToSnapshot() throws MojoExecutionException, MojoFailureException, IOException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[0,1.0.1-SNAPSHOT]";
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder(
                        "The parent project has a newer version:",
                        "default-group:parent-artifact",
                        "[0,1.0.1-SNAPSHOT]",
                        "1.0.1-SNAPSHOT"));
    }

    @Test
    public void testAllowMinorUpdates()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.8.0");
            }
        });
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = true;
        mojo.allowIncrementalUpdates = true;

        ArtifactVersion newVersion = mojo.resolveTargetVersion("0.8.0");

        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("0.9.0"));
    }

    @Test
    public void testAllowIncrementalUpdates()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("test-incremental");
            }
        });
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.allowIncrementalUpdates = true;

        ArtifactVersion newVersion = mojo.resolveTargetVersion("1.1.0");

        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("1.1.1"));
    }

    @Test
    public void testParentVersionRange() throws MojoExecutionException, MojoFailureException, IOException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("1.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        mojo.execute();

        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder("The parent project has a newer version:", "dummy-parent2", "[,3.0-!)", "2.0"));
    }

    @Test
    public void testParentVersionRange2()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IOException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("2.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        mojo.execute();
        assertThat(
                String.join("", Files.readAllLines(tempFile)),
                stringContainsInOrder("The parent project is the latest version:", "dummy-parent2", "[,3.0-!)"));
    }

    @Test
    public void testProblemCausingArtifact() throws MojoFailureException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("problem-causing-artifact");
                setVersion("1.0.0");
            }
        });
        try {
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertThat(e.getCause(), instanceOf(VersionRetrievalException.class));
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertThat(vre.getArtifact().map(Artifact::getArtifactId).orElse(""), equalTo("problem-causing-artifact"));
        }
    }
}
