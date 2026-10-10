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
import java.util.HashMap;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;

/**
 * Scenarios of the {@code use-latest-*}, {@code use-next-*} and {@code use-releases} goals that used to be the
 * {@code it-use-*} integration tests. Each test runs the POM of the former IT against the versions of its test
 * repository.
 */
public class UseGoalsScenarioTest extends AbstractMojoTestCase {

    @Rule
    public final MojoRule mojoRule = new MojoRule(this);

    private Path pomDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        pomDir = createTempDir("use-goals");
    }

    @After
    public void tearDown() throws Exception {
        try {
            tearDownTempDir(pomDir);
        } finally {
            super.tearDown();
        }
    }

    private String run(String goal, String scenario) throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/use-goals/" + scenario), pomDir);
        Mojo mojo = mojoRule.lookupConfiguredMojo(pomDir.toFile(), goal);
        setVariableValueToObject(mojo, "repositorySystem", mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("dummy-api", new String[] {
                    "1.0",
                    "1.0.1",
                    "1.1",
                    "1.1-SNAPSHOT",
                    "1.1.0-2",
                    "1.1.1",
                    "1.1.1-2",
                    "1.1.2",
                    "1.1.2-SNAPSHOT",
                    "1.1.3",
                    "1.2",
                    "1.2.1",
                    "1.2.2",
                    "1.3",
                    "1.9.1-SNAPSHOT",
                    "2.0",
                    "2.1",
                    "2.1.1-SNAPSHOT",
                    "3.0",
                    "3.1.1-SNAPSHOT",
                    "3.1.5-SNAPSHOT",
                    "3.4.0-SNAPSHOT"
                });
                put("update-api", new String[] {"1.9.5", "2.0.0-beta"});
                put(
                        "latest-versions-api",
                        new String[] {"2.0.8", "2.0.11", "2.1.0-M1", "2.2.1", "3.0", "3.0-beta-3", "3.1.0", "3.3.0"});
            }
        }));
        setVariableValueToObject(mojo, "generateBackupPoms", false);
        setVariableValueToObject(mojo, "changeRecorderFormat", "none");
        setVariableValueToObject(mojo, "changeRecorders", new TestChangeRecorder().asTestMap());
        mojo.execute();
        return new String(Files.readAllBytes(pomDir.resolve("pom.xml")), StandardCharsets.UTF_8);
    }

    @Test
    public void testUseLatestReleasesUpdatesToTheLatestRelease() throws Exception {
        assertThat(run("use-latest-releases", "use-latest-releases-001"), containsString("<version>3.0</version>"));
    }

    @Test
    public void testUseLatestReleasesWithoutMajorUpdates() throws Exception {
        assertThat(run("use-latest-releases", "use-latest-releases-004"), containsString("<version>1.3</version>"));
    }

    @Test
    public void testUseLatestReleasesSkipsABetaOfTheNextMajor() throws Exception {
        assertThat(
                run("use-latest-releases", "use-latest-releases-005"),
                not(containsString("<version>2.0.0-beta</version>")));
    }

    @Test
    public void testUseLatestVersionsUpdatesToTheLatestVersion() throws Exception {
        assertThat(run("use-latest-versions", "use-latest-versions-001"), containsString("<version>3.0</version>"));
    }

    @Test
    public void testUseLatestVersionsSkipsABetaOfTheNextMajor() throws Exception {
        assertThat(
                run("use-latest-versions", "use-latest-versions-004"),
                not(containsString("<version>2.0.0-beta</version>")));
    }

    @Test
    public void testUseLatestVersionsWithoutAnyUpdate() throws Exception {
        assertThat(run("use-latest-versions", "use-latest-versions-005"), containsString("<version>2.0.8</version>"));
    }

    @Test
    public void testUseLatestVersionsWithIncrementalUpdatesOnly() throws Exception {
        assertThat(run("use-latest-versions", "use-latest-versions-006"), containsString("<version>2.0.11</version>"));
    }

    @Test
    public void testUseLatestVersionsWithMinorUpdates() throws Exception {
        assertThat(run("use-latest-versions", "use-latest-versions-007"), containsString("<version>2.2.1</version>"));
    }

    @Test
    public void testUseLatestVersionsLeavesAManagedPropertyAlone() throws Exception {
        String pom = run("use-latest-versions", "use-latest-versions-008");
        assertThat(pom, containsString("<api.version>2.0.8</api.version>"));
        assertThat(pom, containsString("<version>${api.version}</version>"));
    }

    @Test
    public void testUseNextReleasesUpdatesToTheNextRelease() throws Exception {
        assertThat(run("use-next-releases", "use-next-releases-001"), containsString("<version>1.1.2</version>"));
    }

    @Test
    public void testUseNextReleasesIgnoresSnapshotsEvenWhenAllowed() throws Exception {
        assertThat(run("use-next-releases", "use-next-releases-002"), containsString("<version>1.1.2</version>"));
    }

    @Test
    public void testUseNextReleasesRespectsExcludes() throws Exception {
        assertThat(run("use-next-releases", "use-next-releases-004"), containsString("<version>1.1.1</version>"));
    }

    @Test
    public void testUseNextVersionsUpdatesToTheNextVersion() throws Exception {
        assertThat(run("use-next-versions", "use-next-versions-001"), containsString("<version>1.1.2</version>"));
    }

    @Test
    public void testUseNextVersionsWithoutSnapshots() throws Exception {
        assertThat(run("use-next-versions", "use-next-versions-002"), containsString("<version>1.1.2</version>"));
    }

    @Test
    public void testUseReleasesKeepsAReleaseVersion() throws Exception {
        assertThat(run("use-releases", "use-releases-001"), containsString("<version>1.1.1-2</version>"));
    }
}
