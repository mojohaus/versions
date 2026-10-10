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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static java.lang.String.join;
import static org.apache.commons.io.FileUtils.copyDirectory;
import static org.apache.maven.api.plugin.testing.MojoExtension.getBasedir;
import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for {@link RevertMojo}
 *
 * @author Andrzej Jarmoniuk
 */
@ProjectMojoTest
public class RevertMojoTest {
    private Path pomDir;

    private ProjectMojos mojos;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        pomDir = Files.createTempDirectory("revert-");
    }

    @AfterEach
    public void tearDown() throws Exception {
        if (pomDir != null && pomDir.toFile().exists()) {
            Arrays.stream(Objects.requireNonNull(pomDir.toFile().listFiles())).forEach(File::delete);
            pomDir.toFile().delete();
        }
    }

    @Test
    public void testRevert() throws Exception {
        copyDirectory(
                new File(getBasedir(), "target/test-classes/org/codehaus/mojo/revert/issue-265"), pomDir.toFile());
        RevertMojo myMojo = mojos.lookup("revert", pomDir.resolve("aggregate/pom.xml"));
        myMojo.execute();

        assertTextContains(join("\n", Files.readAllLines(pomDir.resolve("aggregate/pom.xml"))), "OLD");
        assertEquals(false, Files.exists(pomDir.resolve("aggregate/pom.xml.versionsBackup")));
        assertTextContains(join("\n", Files.readAllLines(pomDir.resolve("module-a/pom.xml"))), "OLD");
        assertEquals(false, Files.exists(pomDir.resolve("module-a/pom.xml.versionsBackup")));
        assertTextContains(join("\n", Files.readAllLines(pomDir.resolve("module-b/pom.xml"))), "OLD");
        assertEquals(false, Files.exists(pomDir.resolve("module-b/pom.xml.versionsBackup")));
    }

    @Test
    public void testRevertNonstandardProjectName() throws Exception {
        copyDirectory(
                new File(getBasedir(), "target/test-classes/org/codehaus/mojo/revert/issue-1227"), pomDir.toFile());
        RevertMojo myMojo = mojos.lookup("revert", pomDir.resolve("pom-a.xml"));
        myMojo.execute();

        assertTextContains(join("\n", Files.readAllLines(pomDir.resolve("pom-a.xml"))), "1.0.0-SNAPSHOT");
        assertEquals(false, Files.exists(pomDir.resolve("aggregate/pom.xml.versionsBackup")));
    }
}
