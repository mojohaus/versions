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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.change.DefaultDependencyVersionChange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.eclipse.aether.RepositorySystem;
import org.junit.BeforeClass;
import org.junit.Test;

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
import static org.hamcrest.Matchers.nullValue;

public class UpdateParentMojoTest extends UseLatestVersionsMojoTestBase {
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

    @Override
    protected UseLatestVersionsMojoBase createMojo() throws IllegalAccessException, MojoExecutionException {
        return new UpdateParentMojo(artifactFactory, repositorySystem, null, changeRecorder.asTestMap()) {
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

                setParentArtifact(new DefaultArtifact(
                        getParent().getGroupId(),
                        getParent().getArtifactId(),
                        getParent().getVersion(),
                        SCOPE_COMPILE,
                        "pom",
                        "default",
                        null));
            }
        };
    }

    @Test
    public void testIgnoredVersions()
            throws MojoExecutionException, IllegalAccessException, VersionRetrievalException, XMLStreamException,
                    MojoFailureException {
        MavenProject parent = new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        };
        mojo.getProject().setParent(parent);
        mojo.getProject()
                .setParentArtifact(new DefaultArtifact(
                        parent.getGroupId(),
                        parent.getArtifactId(),
                        parent.getVersion(),
                        SCOPE_COMPILE,
                        "pom",
                        "default",
                        null));
        setVariableValueToObject(mojo, "ignoredVersions", singleton("1.0.0"));
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }

    @Test
    public void testIncludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        MavenProject parent = new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("other-artifact");
                setVersion("0.9.0");
            }
        };
        mojo.getProject().setParent(parent);
        mojo.getProject()
                .setParentArtifact(new DefaultArtifact(
                        parent.getGroupId(),
                        parent.getArtifactId(),
                        parent.getVersion(),
                        SCOPE_COMPILE,
                        "pom",
                        "default",
                        null));
        setVariableValueToObject(mojo, "includes", new String[] {"default-group:parent-artifact"});
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }

    @Test
    public void testExcludeFilter()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, IllegalAccessException,
                    VersionRetrievalException {
        MavenProject parent = new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        };
        mojo.getProject().setParent(parent);
        mojo.getProject()
                .setParentArtifact(new DefaultArtifact(
                        parent.getGroupId(),
                        parent.getArtifactId(),
                        parent.getVersion(),
                        SCOPE_COMPILE,
                        "pom",
                        "default",
                        null));
        setVariableValueToObject(mojo, "excludes", new String[] {"default-group:parent-artifact"});
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }

    @Test
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

        tryUpdate();
    }

    @Test
    public void testParentDowngradeAllowed()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        setVariableValueToObject(mojo, "allowDowngrade", true);
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "parent-artifact", "1.0.1-SNAPSHOT", "1.0.0")));
    }

    @Test
    public void testParentDowngradeForbidden()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        setVariableValueToObject(mojo, "allowDowngrade", false);
        tryUpdate();
        assertThat(changeRecorder.getChanges(), is(empty()));
    }

    @Test
    public void testParentDowngradeAllowedWithRange()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException, IllegalAccessException, XMLStreamException, MojoFailureException {
        setVariableValueToObject(mojo, "allowDowngrade", true);
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("[1.0.1-SNAPSHOT,)");
            }
        });
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "parent-artifact", "[1.0.1-SNAPSHOT,)", "1.0.0")));
    }

    @Test
    public void testParentDowngradeForbiddenWithRange()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException, IllegalAccessException, XMLStreamException, MojoFailureException {
        setVariableValueToObject(mojo, "allowDowngrade", false);
        setVariableValueToObject(mojo, "parentVersion", "[1.0.1-SNAPSHOT,)");
        tryUpdate();
        assertThat(changeRecorder.getChanges(), is(empty()));
    }

    @Test
    public void testAllowSnapshots()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException, IllegalAccessException, XMLStreamException, MojoFailureException {
        setVariableValueToObject(mojo, "allowSnapshots", true);
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("issue-670-artifact");
                setVersion("0.0.1-1");
            }
        });
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "issue-670-artifact", "0.0.1-1", "0.0.1-1-impl-SNAPSHOT")));
    }

    @Test
    public void testAllowSnapshotsWithParentVersion()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        setVariableValueToObject(mojo, "allowSnapshots", true);
        setVariableValueToObject(mojo, "parentVersion", "0.0.1-1-impl-SNAPSHOT");
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("issue-670-artifact");
                setVersion("0.0.1-1");
            }
        });

        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "issue-670-artifact", "0.0.1-1", "0.0.1-1-impl-SNAPSHOT")));
    }

    @Test
    public void testSkipResolutionDowngradeUnknownVersion()
            throws VersionRetrievalException, MojoExecutionException, XMLStreamException, MojoFailureException,
                    IllegalAccessException {
        testSkipResolution("0.8.0");
    }

    @Test
    public void testSkipResolutionDowngrade()
            throws VersionRetrievalException, MojoExecutionException, XMLStreamException, MojoFailureException,
                    IllegalAccessException {
        testSkipResolution("0.9.0");
    }

    @Test
    public void testSkipResolutionUpgradeUnknownVersion()
            throws VersionRetrievalException, MojoExecutionException, XMLStreamException, MojoFailureException,
                    IllegalAccessException {
        testSkipResolution("2.0.0");
    }

    private void testSkipResolution(String version)
            throws VersionRetrievalException, IllegalAccessException, MojoExecutionException, XMLStreamException,
                    MojoFailureException {
        setVariableValueToObject(mojo, "parentVersion", version);
        setVariableValueToObject(mojo, "skipResolution", true);
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("1.0.0");
            }
        });

        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "parent-artifact", "1.0.0", version)));
    }

    @Test
    public void testShouldUpgradeToSnapshot()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.9.0");
            }
        });
        setVariableValueToObject(mojo, "allowSnapshots", true);
        setVariableValueToObject(mojo, "parentVersion", "[0,1.0.1-SNAPSHOT]");
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange(
                        "default-group", "parent-artifact", "0.9.0", "1.0.1-SNAPSHOT")));
    }

    @Test
    public void testAllowMinorUpdates()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException, IllegalAccessException, XMLStreamException, MojoFailureException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("parent-artifact");
                setVersion("0.8.0");
            }
        });
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", true);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        setVariableValueToObject(mojo, "parentVersion", "0.8.0");
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "parent-artifact", "0.8.0", "0.9.0")));
    }

    @Test
    public void testAllowIncrementalUpdates()
            throws MojoExecutionException, VersionRetrievalException, InvalidVersionSpecificationException,
                    InvalidSegmentException, IllegalAccessException, XMLStreamException, MojoFailureException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("test-incremental");
                setVersion("1.1.0");
            }
        });
        setVariableValueToObject(mojo, "allowMajorUpdates", false);
        setVariableValueToObject(mojo, "allowMinorUpdates", false);
        setVariableValueToObject(mojo, "allowIncrementalUpdates", true);
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "test-incremental", "1.1.0", "1.1.1")));
    }

    @Test
    public void testParentVersionRange()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("1.0");
            }
        });
        setVariableValueToObject(mojo, "allowSnapshots", true);
        setVariableValueToObject(mojo, "parentVersion", "[,3.0-!)");
        tryUpdate();
        assertThat(
                changeRecorder.getChanges(),
                hasItem(new DefaultDependencyVersionChange("default-group", "dummy-parent2", "1.0", "2.0")));
    }

    @Test
    public void testParentVersionRange2()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("2.0");
            }
        });
        setVariableValueToObject(mojo, "allowSnapshots", true);
        setVariableValueToObject(mojo, "parentVersion", "[,3.0-!)");
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }

    /*
     * Reproduces issue 1060
     */
    @Test
    public void testIssue1060VersionRangeAllowDowngradeFalse()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("dummy-parent2");
                setVersion("2.0");
            }
        });
        setVariableValueToObject(mojo, "parentVersion", "[,2.9-!)");
        setVariableValueToObject(mojo, "allowDowngrade", false);
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }

    /*
     * Reproduces issue 1060
     */
    @Test
    public void testIssue1060VersionRangeAllowDowngradeTrue()
            throws MojoExecutionException, XMLStreamException, MojoFailureException, VersionRetrievalException,
                    IllegalAccessException {
        mojo.getProject().setParent(new MavenProject() {
            {
                setGroupId("default-group");
                setArtifactId("mojo-parent");
                setVersion("78");
            }
        });
        setVariableValueToObject(mojo, "parentVersion", "[,79-!)");
        setVariableValueToObject(mojo, "allowDowngrade", true);
        tryUpdate();
        assertThat(changeRecorder.getChanges(), empty());
    }
}
