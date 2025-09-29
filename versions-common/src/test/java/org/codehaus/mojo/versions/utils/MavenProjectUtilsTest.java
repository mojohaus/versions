package org.codehaus.mojo.versions.utils;

import java.util.Optional;
import java.util.Properties;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import static java.util.Optional.empty;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.Is.is;

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
        assertThat(result, is(Optional.of("1.0.0")));
    }

    @Test
    public void testInterpolateVersionNoChange() {
        MavenProject proj = new MavenProject() {
            {
                setOriginalModel(new Model());
            }
        };
        Optional<String> result = MavenProjectUtils.interpolateVersion("${param}", proj);
        assertThat(result.orElse(null), is(nullValue()));
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
        assertThat(result, is(empty()));
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
        assertThat(result, is(Optional.of("1.0.0")));
    }
}
