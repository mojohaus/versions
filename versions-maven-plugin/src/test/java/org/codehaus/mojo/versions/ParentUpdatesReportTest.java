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
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.reporting.ReportRendererFactoryImpl;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.plexus.i18n.I18N;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockI18N;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.mockito.Mockito.mock;

/**
 * Basic tests for {@linkplain ParentUpdatesReport}.
 *
 * @author Andrzej Jarmoniuk
 */
public class ParentUpdatesReportTest {
    private static final I18N MOCK_I18N = mockI18N();

    @Test
    public void testAllowSnapshots() throws IOException, MavenReportException {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new ParentUpdatesReport(
                MOCK_I18N,
                mock(ArtifactFactory.class),
                mockAetherRepositorySystem(new HashMap<String, String[]>() {
                    {
                        put("default-artifact", new String[] {"1.0.0", "1.0.1", "1.1.0", "2.0.0", "2.0.1-SNAPSHOT"});
                    }
                }),
                null,
                new ReportRendererFactoryImpl(MOCK_I18N)) {
            {
                allowSnapshots = true;
                project = new MavenProject(new Model() {
                    {
                        setGroupId("default-group");
                        setArtifactId("child-artifact");
                        setVersion("1.0.0");
                    }
                });
                project.setParent(new MavenProject(new Model() {
                    {
                        setGroupId("default-group");
                        setArtifactId("default-artifact");
                        setVersion("1.0.0");
                    }
                }));
                reactorProjects = new ArrayList<>();
                project.setParentArtifact(new DefaultArtifact(
                        project.getParent().getGroupId(),
                        project.getParent().getArtifactId(),
                        project.getParent().getVersion(),
                        Artifact.SCOPE_COMPILE,
                        "pom",
                        "default",
                        new DefaultArtifactHandlerStub("default")));

                session = mockMavenSession();
                mojoExecution = mock(MojoExecution.class);
            }
        }.generate(sinkFactory.createSink(os), sinkFactory, Locale.getDefault());

        String output = os.toString();
        assertThat(
                output,
                allOf(
                        containsString("1.0.0"),
                        containsString("1.0.1"),
                        containsString("1.1.0"),
                        containsString("2.0.0"),
                        containsString("2.0.1-SNAPSHOT")));
    }
}
