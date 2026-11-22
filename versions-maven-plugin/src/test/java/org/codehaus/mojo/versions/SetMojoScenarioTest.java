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

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TestUtils.copyDir;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContainsPattern;

/**
 * Scenarios of {@link SetMojo} on a single POM that used to be the {@code it-set-NNN} and {@code it-encoding-001}
 * integration tests. Each test runs the POM of the former IT.
 */
@ProjectMojoTest
class SetMojoScenarioTest {

    private static final String FIXTURES = "src/test/resources/org/codehaus/mojo/set/";

    private ProjectMojos mojos;

    private Path pomDir;

    @BeforeEach
    void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        pomDir = createTempDir("set-scenario");
    }

    @AfterEach
    void tearDown() throws Exception {
        tearDownTempDir(pomDir);
    }

    /** Runs the set goal on the fixture with the given parameters, given as name and value pairs. */
    private String runIn(String fixture, Charset charset, Object... parameters) throws Exception {
        copyDir(Paths.get(FIXTURES + fixture), pomDir);
        execute(parameters);
        return new String(Files.readAllBytes(pomDir.resolve("pom.xml")), charset);
    }

    private void execute(Object... parameters) throws Exception {
        SetMojo mojo = mojos.lookup("set", pomDir);
        for (int i = 0; i < parameters.length; i += 2) {
            setVariableValueToObject(mojo, (String) parameters[i], parameters[i + 1]);
        }
        mojo.execute();
    }

    private String run(String fixture, Object... parameters) throws Exception {
        return runIn(fixture, StandardCharsets.UTF_8, parameters);
    }

    @Test
    void setsTheProjectVersion() throws Exception {
        assertTextContains(run("it-001", "newVersion", "2.0"), "<version>2.0</version>");
    }

    @Test
    void setsTheSameVersionAgainWithWildcards() throws Exception {
        // the second goal of the former it-set-001: the version is already 2.0 and the new version is 2.0 too
        run("it-001", "newVersion", "2.0");
        execute("newVersion", "2.0", "groupId", "*", "artifactId", "*", "oldVersion", "*");
        assertTextContains(
                new String(Files.readAllBytes(pomDir.resolve("pom.xml")), StandardCharsets.UTF_8),
                "<version>2.0</version>");
    }

    @Test
    void leavesTheVersionOfADependencyAlone() throws Exception {
        String pom = run("it-001-issue-76", "newVersion", "2.3");
        assertTextContains(pom, "<version>2.3</version>");
        assertTextContainsPattern(pom, "(?s)<artifactId>versions-maven-plugin</artifactId>\\s*<version>2.2</version>");
    }

    @Test
    void removesTheSnapshotQualifier() throws Exception {
        assertTextContains(run("it-010", "removeSnapshot", true), "<version>1.0</version>");
    }

    @Test
    void setsTheNextSnapshotOfAThreeDigitVersion() throws Exception {
        assertTextContains(run("it-014", "nextSnapshot", true), "<version>1.0.1-SNAPSHOT</version>");
    }

    @Test
    void updatesTheOutputTimestampOfAChangedPom() throws Exception {
        assertTextContainsPattern(
                run("it-019-outputTimestamp", "newVersion", "2.0"),
                "<project\\.build\\.outputTimestamp>\\d{4,}</project\\.build\\.outputTimestamp>");
    }

    @Test
    void leavesTheOutputTimestampAloneWhenUpdatingItIsDisabled() throws Exception {
        String pom = run("it-020-outputTimestamp-disabled", "newVersion", "2.0", "updateBuildOutputTimestamp", false);
        assertTextContains(pom, "<project.build.outputTimestamp>10</project.build.outputTimestamp>");
    }

    @Test
    void alwaysUpdatesTheOutputTimestampWithPolicyAlways() throws Exception {
        assertTextContainsPattern(
                run("it-021-policy-always", "newVersion", "1.0", "updateBuildOutputTimestampPolicy", "always"),
                "<project\\.build\\.outputTimestamp>\\d{4,}</project\\.build\\.outputTimestamp>");
    }

    @Test
    void leavesTheOutputTimestampAloneWhenNothingChangedWithPolicyOnchange() throws Exception {
        String pom = run("it-022-policy-onchange", "newVersion", "1.0", "updateBuildOutputTimestampPolicy", "onchange");
        assertTextContains(pom, "<project.build.outputTimestamp>10</project.build.outputTimestamp>");
    }

    @Test
    void neverUpdatesTheOutputTimestampWithPolicyNever() throws Exception {
        String pom = run("it-023-policy-never", "newVersion", "2.0", "updateBuildOutputTimestampPolicy", "never");
        assertTextContains(pom, "<version>2.0</version>");
        assertTextContains(pom, "<project.build.outputTimestamp>10</project.build.outputTimestamp>");
    }

    @Test
    void preservesTheEncodingOfThePom() throws Exception {
        Charset iso = Charset.forName("ISO-8859-15");
        String pom = runIn("it-encoding-001", iso, "newVersion", "2.0");
        // if the pom has not been modified, the test is not useful
        assertTextContains(pom, "<version>2.0</version>");
        assertTextContains(
                pom, "<name>Modify a POM stored with ISO-8859-15 encoding (check euro symbol: \u20AC)</name>");
    }
}
