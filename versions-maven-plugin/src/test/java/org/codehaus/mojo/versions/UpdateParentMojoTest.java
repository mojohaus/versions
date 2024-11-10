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

import java.util.Collections;
import java.util.HashMap;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.eclipse.aether.RepositorySystem;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.MockedStatic;

import static java.util.Collections.singleton;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.apache.maven.plugin.testing.ArtifactStubFactory.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

public class UpdateParentMojoTest {
    private TestChangeRecorder changeRecorder;

    private UpdateParentMojo mojo;

    private static ArtifactHandlerManager artifactHandlerManager;

    private static RepositorySystem repositorySystem;

    @BeforeClass
    public static void setUpStatic() {
        artifactHandlerManager = mockArtifactHandlerManager();
        repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("parent-artifact", new String[] {"0.9.0", "1.0.0", "1.0.1-SNAPSHOT"});
                put("issue-670-artifact", new String[] {"0.0.1-1", "0.0.1-1-impl-SNAPSHOT"});
                put("dummy-parent2", new String[] {"1.0", "2.0", "3.0", "3.0-alpha-1", "3.0-beta-1"});
                put("test-incremental", new String[] {"1.0.0", "1.1.0", "1.1.1", "2.0.0"});
                put("unknown-artifact", new String[0]);
                put("mojo-parent", new String[] {"70", "78", "86"});
            }
        });
    }

    @Before
    public void setUp() throws IllegalAccessException {
        changeRecorder = new TestChangeRecorder();

        mojo = new UpdateParentMojo(artifactHandlerManager, repositorySystem, null, changeRecorder.asTestMap()) {
            {
                setProject(createProject());
                reactorProjects = Collections.emptyList();
                session = mockMavenSession();
            }
        };
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
    @SuppressWarnings("deprecation")
    public void testArtifactIdDoesNotExist()
            throws VersionRetrievalException, MojoExecutionException, XMLStreamException, MojoFailureException,
                    InvalidVersionSpecificationException, VersionRetrievalException {
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

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
    }

    @Test
    public void testParentDowngradeAllowed()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.allowDowngrade = true;
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "parent-artifact", "1.0.1-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testParentDowngradeForbidden()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.allowDowngrade = false;
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), is(empty()));
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
                setVersion("[1.0.1-SNAPSHOT,)");
            }
        });
        ArtifactVersion newVersion = mojo.resolveTargetVersion();
        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("1.0.0"));
    }

    @Test
    public void testParentDowngradeForbiddenWithRange()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException {
        mojo.allowDowngrade = false;
        mojo.parentVersion = "[1.0.1-SNAPSHOT,)";
        ArtifactVersion newVersion = mojo.resolveTargetVersion();
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
                setVersion("0.0.1-1");
            }
        });

        ArtifactVersion newVersion = mojo.resolveTargetVersion();
        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("0.0.1-1-impl-SNAPSHOT"));
    }

    @Test
    public void testAllowSnapshotsWithParentVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.allowSnapshots = true;
        mojo.parentVersion = "0.0.1-1-impl-SNAPSHOT";
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("issue-670-artifact");
                setVersion("0.0.1-1");
            }
        });

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "issue-670-artifact", "0.0.1-1", "0.0.1-1-impl-SNAPSHOT")));
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, IllegalAccessException, VersionRetrievalException,
                    InvalidVersionSpecificationException, InvalidSegmentException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        });
        setVariableValueToObject(mojo, "ignoredVersions", singleton("1.0.0"));
        assertThat(mojo.resolveTargetVersion(), nullValue());
    }

    @Test
    public void testSkipResolutionDowngradeUnknownVersion() throws VersionRetrievalException {
        testSkipResolution("0.8.0");
    }

    @Test
    public void testSkipResolutionDowngrade() throws VersionRetrievalException {
        testSkipResolution("0.9.0");
    }

    @Test
    public void testSkipResolutionUpgradeUnknownVersion() throws VersionRetrievalException {
        testSkipResolution("2.0.0");
    }

    private void testSkipResolution(String version) throws VersionRetrievalException {
        mojo.parentVersion = version;
        mojo.skipResolution = true;
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("1.0.0");
            }
        });

        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        } catch (MojoExecutionException | XMLStreamException | MojoFailureException e) {
            throw new RuntimeException(e);
        }

        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "parent-artifact", "1.0.0", version)));
    }

    @Test
    public void testShouldUpgradeToSnapshot()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[0,1.0.1-SNAPSHOT]";
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "parent-artifact", "0.9.0", "1.0.1-SNAPSHOT")));
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
        mojo.parentVersion = "0.8.0";
        ArtifactVersion newVersion = mojo.resolveTargetVersion();

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
                setVersion("1.1.0");
            }
        });
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.allowIncrementalUpdates = true;
        ArtifactVersion newVersion = mojo.resolveTargetVersion();

        assertThat(newVersion, notNullValue());
        assertThat(newVersion.toString(), is("1.1.1"));
    }

    @Test
    public void testParentVersionRange()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("1.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "dummy-parent2", "1.0", "2.0")));
    }

    @Test
    public void testParentVersionRange2()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("2.0");
            }
        });
        mojo.allowSnapshots = true;
        mojo.parentVersion = "[,3.0-!)";
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), empty());
    }

    /*
     * Reproduces issue 1060
     */
    @Test
    public void testIssue1060VersionRangeAllowDowngradeFalse()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("2.0");
            }
        });
        mojo.parentVersion = "[,2.9-!)";
        mojo.allowDowngrade = false;
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), empty());
    }

    /*
     * Reproduces issue 1060
     */
    @Test
    public void testIssue1060VersionRangeAllowDowngradeTrue()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("mojo-parent");
                setVersion("78");
            }
        });
        mojo.parentVersion = "[,79-!)";
        mojo.allowDowngrade = true;
        try (MockedStatic<PomHelper> pomHelper = mockStatic(PomHelper.class)) {
            pomHelper
                    .when(() -> PomHelper.setProjectParentVersion(any(), any()))
                    .thenReturn(true);
            mojo.update(null);
        }
        assertThat(changeRecorder.getChanges(), empty());
    }
}
