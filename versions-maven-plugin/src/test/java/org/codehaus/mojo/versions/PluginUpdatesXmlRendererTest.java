package org.codehaus.mojo.versions;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.model.Plugin;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.reporting.model.PluginUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.xml.DependencyUpdatesXmlReportRenderer;
import org.codehaus.mojo.versions.xml.PluginUpdatesXmlReportRenderer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonMap;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

/**
 * Basic tests for {@linkplain DependencyUpdatesXmlReportRenderer}.
 *
 * @author Andrzej Jarmoniuk
 */
public class PluginUpdatesXmlRendererTest {
    private Path tempFile;

    @Before
    public void setUp() throws IOException {
        tempFile = Files.createTempFile("xml-plugin-report", "");
    }

    @After
    public void tearDown() throws IOException {
        if (tempFile != null && Files.exists(tempFile)) {
            Files.delete(tempFile);
        }
    }

    @Test
    public void testReportGeneration() throws IOException {
        PluginUpdatesModel pluginUpdates = new PluginUpdatesModel(
                singletonMap(
                        pluginOf("default-group", "artifactA", "1.0.0"),
                        new PluginUpdatesDetails(
                                new ArtifactVersions(
                                        artifactOf("default-group", "artifactA", "1.0.0"),
                                        Stream.of("1.0.0", "1.0.1", "1.1.0", "2.0.0")
                                                .map(ArtifactVersionService::getArtifactVersion)
                                                .collect(Collectors.toList()),
                                        new MavenVersionComparator()),
                                singletonMap(
                                        DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("artifactB")
                                                .withVersion("1.0.0")
                                                .build(),
                                        new ArtifactVersions(
                                                artifactOf("default-group", "artifactB", "1.0.0"),
                                                Stream.of("1.0.0", "1.0.1-SNAPSHOT", "1.1.0-rc1", "2.0.0")
                                                        .map(ArtifactVersionService::getArtifactVersion)
                                                        .collect(Collectors.toList()),
                                                new MavenVersionComparator())),
                                false)),
                emptyMap());
        new PluginUpdatesXmlReportRenderer(pluginUpdates, tempFile, false).render();

        String output = String.join("", Files.readAllLines(tempFile)).replaceAll(">\\s*<", "><");

        assertThat(output, containsString("<usingLastVersion>0</usingLastVersion>"));
        assertThat(output, containsString("<nextVersionAvailable>0</nextVersionAvailable>"));
        assertThat(output, containsString("<nextIncrementalAvailable>1</nextIncrementalAvailable>"));
        assertThat(output, containsString("<nextMinorAvailable>0</nextMinorAvailable>"));
        assertThat(output, containsString("<nextMajorAvailable>0</nextMajorAvailable>"));
        assertThat(output, containsString("<dependencyUpdates>1</dependencyUpdates>"));

        assertThat(output, containsString("<currentVersion>1.0.0</currentVersion>"));
        assertThat(output, containsString("<lastVersion>2.0.0</lastVersion>"));
        assertThat(output, containsString("<incremental>1.0.1</incremental>"));
        assertThat(output, containsString("<minor>1.1.0</minor>"));
        assertThat(output, containsString("<major>2.0.0</major>"));
        assertThat(output, containsString("<status>incremental available</status>"));
    }

    private static DefaultArtifact artifactOf(String groupId, String artifactId, String version) {
        return new DefaultArtifact(groupId, artifactId, version, SCOPE_COMPILE, "jar", "default", null);
    }

    private static Plugin pluginOf(String groupId, String artifactId, String version) {
        return new Plugin() {
            {
                setGroupId(groupId);
                setArtifactId(artifactId);
                setVersion(version);
            }
        };
    }
}
