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
import java.util.Arrays;

import org.apache.maven.artifact.DefaultArtifact;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.reporting.model.DependencyUpdatesModel;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.xml.DependencyUpdatesXmlReportRenderer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyMap;
import static java.util.Collections.singletonMap;
import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.codehaus.mojo.versions.TextAssertions.assertContains;

/**
 * Basic tests for {@linkplain DependencyUpdatesXmlReportRenderer}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DependencyUpdatesXmlRendererTest {
    private Path tempFile;

    @BeforeEach
    public void setUp() throws IOException {
        tempFile = Files.createTempFile("xml-dependency-report", "");
    }

    @AfterEach
    public void tearDown() throws IOException {
        if (tempFile != null && Files.exists(tempFile)) {
            Files.delete(tempFile);
        }
    }

    @Test
    public void testReportGeneration() throws IOException {
        new DependencyUpdatesXmlReportRenderer(
                        new DependencyUpdatesModel(
                                singletonMap(
                                        DependencyBuilder.newBuilder()
                                                .withGroupId("default-group")
                                                .withArtifactId("artifactA")
                                                .withVersion("1.0.0")
                                                .build(),
                                        new ArtifactVersions(
                                                new DefaultArtifact(
                                                        "default-group",
                                                        "artifactA",
                                                        "1.0.0",
                                                        SCOPE_COMPILE,
                                                        "jar",
                                                        "default",
                                                        null),
                                                Arrays.asList(
                                                        ArtifactVersionService.getArtifactVersion("1.0.0"),
                                                        ArtifactVersionService.getArtifactVersion("1.0.1"),
                                                        ArtifactVersionService.getArtifactVersion("1.1.0"),
                                                        ArtifactVersionService.getArtifactVersion("2.0.0")))),
                                emptyMap()),
                        tempFile,
                        false)
                .render();
        String output = String.join("", Files.readAllLines(tempFile)).replaceAll(">\\s*<", "><");

        assertContains(output, "<usingLastVersion>0</usingLastVersion>");
        assertContains(output, "<nextVersionAvailable>0</nextVersionAvailable>");
        assertContains(output, "<nextIncrementalAvailable>1</nextIncrementalAvailable>");
        assertContains(output, "<nextMinorAvailable>0</nextMinorAvailable>");
        assertContains(output, "<nextMajorAvailable>0</nextMajorAvailable>");

        assertContains(output, "<currentVersion>1.0.0</currentVersion>");
        assertContains(output, "<lastVersion>2.0.0</lastVersion>");
        assertContains(output, "<incremental>1.0.1</incremental>");
        assertContains(output, "<minor>1.1.0</minor>");
        assertContains(output, "<major>2.0.0</major>");
        assertContains(output, "<status>incremental available</status>");
    }
}
