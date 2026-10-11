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
import java.nio.file.Path;
import java.util.HashMap;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.eclipse.aether.RepositorySystem;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Base class for {@link UpdatePropertiesMojo} and {@link UpdatePropertyMojo} test suites
 */
@ProjectMojoTest
public abstract class UpdatePropertiesMojoTestBase {
    protected ProjectMojos mojos;

    protected Path pomDir;
    protected RepositorySystem repositorySystem;
    protected TestChangeRecorder changeRecorder;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        pomDir = createTempDir("update-property");
        repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("default-artifact", new String[] {"1.0.0", "1.0.1-rc1", "1.1.0-alpha", "2.0.0-M1"});
            }
        });
    }

    @AfterEach
    public void tearDown() throws Exception {
        tearDownTempDir(pomDir);
    }

    protected <T extends Mojo> T setUpMojo(String goal) throws Exception {
        return configure(mojos.lookup(goal, pomDir));
    }

    /**
     * Looks up {@code goal} in a reactor of the {@code modules} of {@link #pomDir}, running in the first module,
     * and configures it like {@link #setUpMojo(String)}.
     */
    protected <T extends Mojo> T setUpReactorMojo(String goal, String... modules) throws Exception {
        return configure(mojos.lookupInReactor(goal, pomDir, modules));
    }

    private <T extends Mojo> T configure(T mojo) throws Exception {
        setVariableValueToObject(mojo, "repositorySystem", repositorySystem);
        setVariableValueToObject(mojo, "generateBackupPoms", false);
        setVariableValueToObject(mojo, "changeRecorderFormat", "none");
        changeRecorder = new TestChangeRecorder();
        setVariableValueToObject(mojo, "changeRecorders", changeRecorder.asTestMap());

        return mojo;
    }

    @SuppressWarnings("unchecked")
    protected <T extends Mojo> void testProblemCausingArtifact(String goal, Consumer<Mojo>... initializers)
            throws Exception {
        File projectDir = new File("src/test/resources/org/codehaus/mojo/update-properties/problem-causing");
        T mojo = mojos.lookup(goal, projectDir.toPath());
        Stream.of(initializers).forEach(m -> m.accept(mojo));
        try {
            mojo.execute();
            fail("Should throw an exception");
        } catch (MojoExecutionException e) {
            assertInstanceOf(VersionRetrievalException.class, e.getCause());
            VersionRetrievalException vre = (VersionRetrievalException) e.getCause();
            assertEquals(
                    "problem-causing-artifact",
                    vre.getArtifact().map(Artifact::getArtifactId).orElse(""));
        }
    }
}
