package org.codehaus.mojo.versions.utils;
/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.File;
import java.io.IOException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Extension;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link CoreExtensionUtils}
 *
 * @author Andrzej Jarmoniuk
 */
public class CoreExtensionUtilsTest {

    @Test
    public void testNoExtensions() throws XmlPullParserException, IOException {
        MavenProject project = mock(MavenProject.class);
        when(project.getBasedir())
                .thenReturn(
                        new File("src/test/resources/org/codehaus/mojo/versions/utils/core-extensions/no-extensions"));
        MavenSession session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        assertThat(CoreExtensionUtils.getCoreExtensions(project).findAny(), is(Optional.empty()));
    }

    @Test
    public void testExtensionsFound() throws XmlPullParserException, IOException {
        MavenProject project = mock(MavenProject.class);
        when(project.getBasedir())
                .thenReturn(new File("src/test/resources/org/codehaus/mojo/versions/utils/core-extensions"));
        MavenSession session = mock(MavenSession.class);
        when(session.getCurrentProject()).thenReturn(project);
        Set<Extension> extensions =
                CoreExtensionUtils.getCoreExtensions(project).collect(Collectors.toSet());
        assertThat(
                extensions,
                hasItems(
                        ExtensionBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("artifactA")
                                .withVersion("1.0.0")
                                .build(),
                        ExtensionBuilder.newBuilder()
                                .withGroupId("default-group")
                                .withArtifactId("artifactB")
                                .withVersion("2.0.0")
                                .build()));
    }
}
