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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;

import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.codehaus.plexus.component.configurator.ComponentConfigurationException;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import static java.lang.String.join;
import static org.apache.commons.io.FileUtils.copyDirectory;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.core.Is.is;

/**
 * Unit tests for {@link RevertMojo}
 *
 * @author Andrzej Jarmoniuk
 */
public class RevertMojoTest extends AbstractMojoTestCase {
    private Path pomDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        pomDir = Files.createTempDirectory("revert-");
    }

    @After
    public void tearDown() throws Exception {
        try {
            if (pomDir != null && pomDir.toFile().exists()) {
                Arrays.stream(Objects.requireNonNull(pomDir.toFile().listFiles()))
                        .forEach(File::delete);
                pomDir.toFile().delete();
            }
        } finally {
            super.tearDown();
        }
    }

    private MavenProject readMavenProject(Path pomFilePath) throws Exception {
        MavenExecutionRequest request = new DefaultMavenExecutionRequest();
        request.setBaseDirectory(pomFilePath.getParent().toFile());
        ProjectBuildingRequest configuration = request.getProjectBuildingRequest();
        configuration.setRepositorySession(new DefaultRepositorySystemSession());
        MavenProject project = lookup(ProjectBuilder.class)
                .build(pomFilePath.toFile(), configuration)
                .getProject();
        Assert.assertNotNull(project);
        return project;
    }

    private Mojo lookupConfiguredMojo(Path pomFilePath, String goal) throws Exception, ComponentConfigurationException {
        MavenProject project = readMavenProject(pomFilePath);
        MavenSession session = newMavenSession(project);
        MojoExecution execution = newMojoExecution(goal);
        return lookupConfiguredMojo(session, execution);
    }

    @Test
    public void testRevert() throws Exception {
        copyDirectory(
                new File(getBasedir(), "target/test-classes/org/codehaus/mojo/revert/issue-265"), pomDir.toFile());
        RevertMojo myMojo = (RevertMojo) lookupConfiguredMojo(pomDir.resolve("aggregate/pom.xml"), "revert");
        myMojo.execute();

        assertThat(join("\n", Files.readAllLines(pomDir.resolve("aggregate/pom.xml"))), containsString("OLD"));
        assertThat(Files.exists(pomDir.resolve("aggregate/pom.xml.versionsBackup")), is(false));
        assertThat(join("\n", Files.readAllLines(pomDir.resolve("module-a/pom.xml"))), containsString("OLD"));
        assertThat(Files.exists(pomDir.resolve("module-a/pom.xml.versionsBackup")), is(false));
        assertThat(join("\n", Files.readAllLines(pomDir.resolve("module-b/pom.xml"))), containsString("OLD"));
        assertThat(Files.exists(pomDir.resolve("module-b/pom.xml.versionsBackup")), is(false));
    }

    @Test
    public void testRevertNonstandardProjectName() throws Exception {
        copyDirectory(
                new File(getBasedir(), "target/test-classes/org/codehaus/mojo/revert/issue-1227"), pomDir.toFile());
        RevertMojo myMojo = (RevertMojo) lookupConfiguredMojo(pomDir.resolve("pom-a.xml"), "revert");
        myMojo.execute();

        assertThat(join("\n", Files.readAllLines(pomDir.resolve("pom-a.xml"))), containsString("1.0.0-SNAPSHOT"));
        assertThat(Files.exists(pomDir.resolve("aggregate/pom.xml.versionsBackup")), is(false));
    }
}
