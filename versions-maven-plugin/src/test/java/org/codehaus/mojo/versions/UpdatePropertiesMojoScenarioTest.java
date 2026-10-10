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
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.function.Consumer;

import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

/**
 * Scenarios of {@link UpdatePropertiesMojo} that used to be the {@code it-update-properties-NNN} integration tests.
 * Each test runs the POM of the former IT against the versions of its test repository.
 */
public class UpdatePropertiesMojoScenarioTest extends UpdatePropertiesMojoTestBase {

    private static final String[] DUMMY_API = {
        "1.0", "1.0.1", "1.1", "1.1-SNAPSHOT", "1.1.0-2", "1.1.1", "1.1.1-2", "1.1.2", "1.1.2-SNAPSHOT", "1.1.3",
        "1.2", "1.2.1", "1.2.2", "1.3", "1.9.1-SNAPSHOT", "2.0", "2.1", "2.1.1-SNAPSHOT", "3.0", "3.1.1-SNAPSHOT",
        "3.1.5-SNAPSHOT", "3.4.0-SNAPSHOT"
    };

    private static final String[] DUMMY_IMPL = {"1.0", "1.1", "1.2", "1.3", "1.4", "2.0", "2.1", "2.2"};

    private String run(String scenario, Consumer<UpdatePropertiesMojo> configuration) throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-properties/" + scenario), pomDir);
        repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
            {
                put("dummy-api", DUMMY_API);
                put("dummy-impl", DUMMY_IMPL);
            }
        });
        UpdatePropertiesMojo mojo = setUpMojo("update-properties");
        configuration.accept(mojo);
        mojo.execute();
        return new String(Files.readAllBytes(pomDir.resolve("pom.xml")), StandardCharsets.UTF_8);
    }

    private String run(String scenario) throws Exception {
        return run(scenario, mojo -> {});
    }

    @Test
    public void testUpdatesPropertyConfiguredForADependency() throws Exception {
        assertThat(run("it-001"), containsString("<api>3.0</api>"));
    }

    @Test
    public void testUpdatesPropertyUsedInARangeAndKeepsTheRange() throws Exception {
        String pom = run("it-003");
        assertThat(pom, containsString("<api>3.0</api>"));
        assertThat(pom, containsString("<version>[${api}]</version>"));
    }

    @Test
    public void testUpdatesTwoConfiguredProperties() throws Exception {
        String pom = run("it-006");
        assertThat(pom, containsString("<api>3.0</api>"));
        assertThat(pom, containsString("<impl>2.2</impl>"));
    }

    @Test
    public void testUpdatesLinkedPropertyAndKeepsTheReference() throws Exception {
        String pom = run("it-007");
        assertThat(pom, containsString("<api>3.0</api>"));
        assertThat(pom, containsString("<version>${api}</version>"));
    }

    @Test
    public void testUpdatesPropertiesSharedBetweenDependencies() throws Exception {
        String pom = run("it-008");
        assertThat(pom, containsString("<api>3.0</api>"));
        assertThat(pom, containsString("<impl>2.2</impl>"));
    }

    @Test
    public void testUpdatesPropertyToTheConfiguredRange() throws Exception {
        assertThat(run("it-010"), containsString("<api>3.0</api>"));
    }

    @Test
    public void testStaysWithinTheMajorVersion() throws Exception {
        assertThat(run("it-015", mojo -> mojo.allowMajorUpdates = false), containsString("<api>2.1</api>"));
    }

    @Test
    public void testStaysWithinTheMajorVersionForConfiguredAndLinkedProperties() throws Exception {
        String pom = run("it-016", mojo -> mojo.allowMajorUpdates = false);
        assertThat(pom, containsString("<api>2.1</api>"));
        assertThat(pom, containsString("<impl>1.4</impl>"));
    }

    @Test
    public void testKeepsVersionWithoutIncrementalUpdate() throws Exception {
        assertThat(
                run("it-019", mojo -> {
                    mojo.allowMajorUpdates = false;
                    mojo.allowMinorUpdates = false;
                }),
                containsString("<api>2.0</api>"));
    }

    @Test
    public void testAppliesIncrementalUpdate() throws Exception {
        assertThat(
                run("it-021", mojo -> {
                    mojo.allowMajorUpdates = false;
                    mojo.allowMinorUpdates = false;
                }),
                containsString("<api>1.0.1</api>"));
    }

    @Test
    public void testKeepsVersionWithoutAnyUpdate() throws Exception {
        assertThat(
                run("it-022", mojo -> {
                    mojo.allowMajorUpdates = false;
                    mojo.allowMinorUpdates = false;
                    mojo.allowIncrementalUpdates = false;
                }),
                containsString("<api>1.0</api>"));
    }

    @Test
    public void testAppliesBuildNumberUpdateOnly() throws Exception {
        assertThat(
                run("it-023", mojo -> {
                    mojo.allowMajorUpdates = false;
                    mojo.allowMinorUpdates = false;
                    mojo.allowIncrementalUpdates = false;
                }),
                containsString("<api>1.1.0-2</api>"));
    }
}
