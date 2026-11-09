package org.codehaus.mojo.versions.utils;

import java.util.Optional;
import java.util.Properties;
import java.util.Set;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MavenProjectUtilsTest {

    @Test
    public void testInterpolateVersion() {
        Dependency dep = DependencyBuilder.newBuilder()
                .withGroupId("groupA")
                .withArtifactId("artifactA")
                .withVersion("${param}")
                .build();
        MavenProject proj = new MavenProject() {
            {
                setOriginalModel(new Model() {
                    {
                        setProperties(new Properties() {
                            {
                                setProperty("param", "1.0.0");
                            }
                        });
                    }
                });
            }
        };
        // resolve version from model properties if necessary (e.g. "${mycomponent.myversion}"
        Optional<String> result = MavenProjectUtils.interpolateVersion(dep.getVersion(), proj);
        assertEquals(Optional.of("1.0.0"), result);
    }

    @Test
    public void testInterpolateVersionNoChange() {
        MavenProject proj = new MavenProject() {
            {
                setOriginalModel(new Model());
            }
        };
        Optional<String> result = MavenProjectUtils.interpolateVersion("${param}", proj);
        assertNull(result.orElse(null));
    }

    @Test
    public void testInterpolateVersionDependencyNoChange() {
        Dependency dep = DependencyBuilder.newBuilder()
                .withGroupId("groupA")
                .withArtifactId("artifactA")
                .withVersion("${param}")
                .build();
        MavenProject proj = new MavenProject() {
            {
                setOriginalModel(new Model());
            }
        };
        // resolve version from model properties if necessary (e.g. "${mycomponent.myversion}"
        Optional<String> result = MavenProjectUtils.interpolateVersion(dep.getVersion(), proj);
        assertEquals(Optional.empty(), result);
    }

    @Test
    public void testMultiLevelInterpolatedVersion() {
        MavenProject proj = new MavenProject() {
            {
                setOriginalModel(new Model() {
                    {
                        setProperties(new Properties() {
                            {
                                setProperty("param1", "${param2}");
                                setProperty("param2", "${param3}");
                                setProperty("param3", "1.0.0");
                            }
                        });
                    }
                });
            }
        };
        Optional<String> result = MavenProjectUtils.interpolateVersion("${param1}", proj);
        assertEquals(Optional.of("1.0.0"), result);
    }

    private static MavenProject projectWithRawDependencyManagement(Dependency... dependencies) {
        Model model = new Model();
        model.setGroupId("com.example");
        model.setArtifactId("parent");
        model.setVersion("1.0-SNAPSHOT");
        model.getProperties().setProperty("bom.artifactId", "bom");

        Model originalModel = model.clone();
        DependencyManagement dependencyManagement = new DependencyManagement();
        for (Dependency dependency : dependencies) {
            dependencyManagement.addDependency(dependency);
        }
        originalModel.setDependencyManagement(dependencyManagement);

        MavenProject project = new MavenProject(model);
        project.setOriginalModel(originalModel);
        return project;
    }

    @Test
    public void testRawDependencyManagementCoordinatesAreInterpolated() throws Exception {
        MavenProject project = projectWithRawDependencyManagement(DependencyBuilder.newBuilder()
                .withGroupId("${project.groupId}")
                .withArtifactId("${bom.artifactId}")
                .withVersion("${project.version}")
                .withType("pom")
                .withScope("import")
                .build());

        Set<Dependency> result =
                MavenProjectUtils.extractDependenciesFromDependencyManagement(project, false, new SystemStreamLog());

        assertEquals(1, result.size());
        Dependency dependency = result.iterator().next();
        assertEquals("com.example", dependency.getGroupId());
        assertEquals("bom", dependency.getArtifactId());
        assertEquals("1.0-SNAPSHOT", dependency.getVersion());
    }

    @Test
    public void testUninterpolatableDependencyManagementEntryIsSkipped() throws Exception {
        MavenProject project = projectWithRawDependencyManagement(
                DependencyBuilder.newBuilder()
                        .withGroupId("${undefined.groupId}")
                        .withArtifactId("bom")
                        .withVersion("1.0")
                        .build(),
                DependencyBuilder.newBuilder()
                        .withGroupId("org.example")
                        .withArtifactId("lib")
                        .withVersion("2.0")
                        .build());

        Set<Dependency> result =
                MavenProjectUtils.extractDependenciesFromDependencyManagement(project, false, new SystemStreamLog());

        assertEquals(1, result.size());
        assertTrue(result.stream().allMatch(d -> "lib".equals(d.getArtifactId())));
    }

    @Test
    public void testInterpolateCoordinatesWithEmbeddedExpression() {
        MavenProject project = projectWithRawDependencyManagement();
        Dependency dependency = MavenProjectUtils.interpolateCoordinates(
                DependencyBuilder.newBuilder()
                        .withGroupId("${pom.groupId}.sub")
                        .withArtifactId("${bom.artifactId}-${project.artifactId}")
                        .withVersion("${project.version}")
                        .build(),
                project);

        assertEquals("com.example.sub", dependency.getGroupId());
        assertEquals("bom-parent", dependency.getArtifactId());
        assertEquals("1.0-SNAPSHOT", dependency.getVersion());
    }
}
