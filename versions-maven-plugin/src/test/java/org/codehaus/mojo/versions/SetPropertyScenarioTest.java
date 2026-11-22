package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TestUtils.copyDir;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Scenarios of {@link SetPropertyMojo} that used to be the {@code it-set-property-NNN} integration tests. Each test
 * runs the POM of the former IT.
 */
@ProjectMojoTest
class SetPropertyScenarioTest {

    private ProjectMojos mojos;

    private Path pomDir;

    @BeforeEach
    void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        pomDir = createTempDir("set-property-scenario");
    }

    @AfterEach
    void tearDown() throws Exception {
        tearDownTempDir(pomDir);
    }

    private String run(String fixture, String property, String newVersion) throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/" + fixture), pomDir);
        SetPropertyMojo mojo = mojos.lookup("set-property", pomDir);
        // the new version is given explicitly, so nothing is resolved from a repository
        mojo.repositorySystem = mock(RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));
        setVariableValueToObject(mojo, "property", property);
        setVariableValueToObject(mojo, "newVersion", newVersion);
        mojo.execute();
        return new String(Files.readAllBytes(pomDir.resolve("pom.xml")), StandardCharsets.UTF_8);
    }

    @Test
    void setsOnlyTheNamedProperty() throws Exception {
        String pom = run("it-001", "versionModuleTest", "9.5.0-20170604.123223-2");
        assertTextContains(pom, "<versionModule>1.2.3-SNAPSHOT</versionModule>");
        assertTextContains(pom, "<versionModuleTest>9.5.0-20170604.123223-2</versionModuleTest>");
    }

    @Test
    void setsOnlyTheOtherNamedProperty() throws Exception {
        String pom = run("it-002", "versionModule", "9.5.0-20170604.123456-2");
        assertTextContains(pom, "<versionModule>9.5.0-20170604.123456-2</versionModule>");
        assertTextContains(pom, "<versionModuleTest>1.2.3-SNAPSHOT</versionModuleTest>");
    }

    @Test
    void setsEveryPropertyOfACommaSeparatedList() throws Exception {
        String pom = run("it-003-multiple-properties", "versionModule,versionModuleTest", "9.5.0-20170604.123456-2");
        assertTextContains(pom, "<versionModule>9.5.0-20170604.123456-2</versionModule>");
        assertTextContains(pom, "<versionModuleTest>9.5.0-20170604.123456-2</versionModuleTest>");
    }
}
