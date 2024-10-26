package org.codehaus.mojo.versions.utils;

import java.util.Properties;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.utils.MavenProjectUtils.interpolateVersion;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
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
        Dependency result = interpolateVersion(dep, proj);
        assertThat(result.getVersion(), is("1.0.0"));
    }

    @Test
    public void testImmutability() {
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
        assertThat(interpolateVersion(dep, proj), not(sameInstance(dep)));
    }

    @Test
    public void testVersionlessDependency() {
        Dependency dep = DependencyBuilder.newBuilder()
                .withGroupId("groupA")
                .withArtifactId("artifactA")
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
        Dependency result = interpolateVersion(dep, proj);
        assertThat(result.getVersion(), nullValue());
    }
}
