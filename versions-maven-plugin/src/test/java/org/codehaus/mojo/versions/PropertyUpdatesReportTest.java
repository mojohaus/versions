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

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Locale;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.doxia.tools.SiteTool;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.eclipse.aether.RepositorySystem;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockSiteTool;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotContains;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Unit tests for {@link PropertyUpdatesReport}
 */
@ProjectMojoTest
public class PropertyUpdatesReportTest {
    protected ProjectMojos mojos;

    @BeforeEach
    public void injectMojos(ProjectMojos mojos) {
        this.mojos = mojos;
    }

    private static final RepositorySystem AETHER_REPOSITORY_SYSTEM = mockAetherRepositorySystem();
    private static final SiteTool SITE_TOOL = mockSiteTool();

    @Test
    public void testIncludeParentTrueShouldContainProperty() throws Exception {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();

        PropertyUpdatesReport mojo = (PropertyUpdatesReport) mojos.lookup(
                "property-updates-report",
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367/child"));
        setVariableValueToObject(mojo, "siteTool", SITE_TOOL);
        setVariableValueToObject(mojo, "repositorySystem", AETHER_REPOSITORY_SYSTEM);
        setVariableValueToObject(mojo, "includeParent", true);
        mojo.generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString()
                .replaceAll("<[^>]+>", " ")
                .replaceAll("&[^;]+;", " ")
                .replaceAll("\\s+", " ");
        assertTextContains(output, "${ver}");
    }

    @Test
    public void testIncludeParentFalseShouldNotContainProperty() throws Exception {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();

        PropertyUpdatesReport mojo = (PropertyUpdatesReport) mojos.lookup(
                "property-updates-report",
                Paths.get("src/test/resources/org/codehaus/mojo/display-property-updates/issue-367/child"));
        setVariableValueToObject(mojo, "siteTool", SITE_TOOL);
        setVariableValueToObject(mojo, "repositorySystem", AETHER_REPOSITORY_SYSTEM);
        setVariableValueToObject(mojo, "includeParent", false);
        mojo.generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString()
                .replaceAll("<[^>]+>", " ")
                .replaceAll("&[^;]+;", " ")
                .replaceAll("\\s+", " ");
        assertTextNotContains(output, "${ver}");
    }

    @Test
    public void testProblemCausingArtifact() throws Exception {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();

        PropertyUpdatesReport mojo = (PropertyUpdatesReport) mojos.lookup(
                "property-updates-report",
                Paths.get("src/test/resources/org/codehaus/mojo/update-properties/problem-causing"));
        setVariableValueToObject(mojo, "siteTool", SITE_TOOL);
        setVariableValueToObject(mojo, "repositorySystem", AETHER_REPOSITORY_SYSTEM);

        try {
            mojo.generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());
            fail("Should throw an exception");
        } catch (MavenReportException e) {
            assertInstanceOf(MojoExecutionException.class, e.getCause());
            MojoExecutionException moe = (MojoExecutionException) e.getCause();
            assertInstanceOf(VersionRetrievalException.class, moe.getCause());
            VersionRetrievalException vre = (VersionRetrievalException) moe.getCause();
            assertEquals(
                    "problem-causing-artifact",
                    vre.getArtifact().map(Artifact::getArtifactId).orElse(""));
        }
    }
}
