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

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

/**
 * Scenarios of {@link DisplayDependencyUpdatesMojo} that used to be the {@code it-display-dependency-updates-*}
 * integration tests. Each test runs the POM of the former IT against the versions of its test repository.
 */
public class DisplayDependencyUpdatesMojoScenarioTest extends AbstractMojoTestCase {

    @Rule
    public final MojoRule mojoRule = new MojoRule(this);

    private static final String[] DUMMY_API = {
        "1.0", "1.0.1", "1.1", "1.1-SNAPSHOT", "1.1.0-2", "1.1.1", "1.1.1-2", "1.1.2", "1.1.2-SNAPSHOT", "1.1.3",
        "1.2", "1.2.1", "1.2.2", "1.3", "1.9.1-SNAPSHOT", "2.0", "2.1", "2.1.1-SNAPSHOT", "3.0", "3.1.1-SNAPSHOT",
        "3.1.5-SNAPSHOT", "3.4.0-SNAPSHOT"
    };

    private static final String[] DUMMY_IMPL = {"1.0", "1.1", "1.2", "1.3", "1.4", "2.0", "2.1", "2.2"};

    private static final String DEPENDENCIES = "The following dependencies in Dependencies have newer versions:";

    private static final String DEPENDENCY_MANAGEMENT =
            "The following dependencies in Dependency Management have newer versions:";

    private static final String PLUGIN_MANAGEMENT =
            "The following dependencies in pluginManagement of plugins have newer versions:";

    private static final String PLUGIN_DEPENDENCIES =
            "The following dependencies in Plugin Dependencies have newer versions:";

    private interface Configuration {
        void apply(DisplayDependencyUpdatesMojo mojo) throws Exception;
    }

    private String run(String scenario, Configuration configuration) throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File("target/test-classes/org/codehaus/mojo/display-dependency-updates/" + scenario),
                    "display-dependency-updates");
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.setPluginContext(new HashMap<>());
            mojo.repositorySystem = mockAetherRepositorySystem(new HashMap<String, String[]>() {
                {
                    put("dummy-api", DUMMY_API);
                    put("dummy-impl", DUMMY_IMPL);
                }
            });
            configuration.apply(mojo);
            mojo.execute();
            return new String(Files.readAllBytes(tempFile.getPath()), StandardCharsets.UTF_8);
        }
    }

    private String run(String scenario) throws Exception {
        return run(scenario, mojo -> {});
    }

    private static String update(String artifact, String from, String to) {
        return "\\Qlocalhost:" + artifact + "\\E\\s*\\.*\\s*\\Q" + from + "\\E\\s+->\\s+\\Q" + to + "\\E\\b";
    }

    private static int count(String output, String regex) {
        Matcher matcher = Pattern.compile(regex).matcher(output);
        int count = 0;
        while (matcher.find()) {
            count++;
        }
        return count;
    }

    private static void assertListed(String output, String regex) {
        assertThat(output + "\nshould match " + regex, count(output, regex) > 0, is(true));
    }

    private static void assertNotListed(String output, String regex) {
        assertThat(output + "\nshould not match " + regex, count(output, regex), is(0));
    }

    @Test
    public void testRangeIsReportedWithItsUpdate() throws Exception {
        assertListed(run("it-002"), update("dummy-api", "[1.1,3.0)", "3.0"));
    }

    @Test
    public void testManagedVersionIsNotReportedForAnExplicitOlderVersion() throws Exception {
        assertNotListed(run("it-003"), "localhost:dummy-api .* 1.1");
    }

    @Test
    public void testManagedOlderVersionIsNotReportedForAnExplicitVersion() throws Exception {
        assertNotListed(run("it-004"), "localhost:dummy-api .* 1.1");
    }

    @Test
    public void testOutputFileReceivesTheUpdates() throws Exception {
        assertListed(run("it-007", mojo -> mojo.verbose = true), update("dummy-api", "1.1", "3.0"));
    }

    @Test
    public void testGroupIdFromAProperty() throws Exception {
        assertListed(run("issue-1001"), update("dummy-api", "1.0", "3.0"));
    }

    @Test
    public void testVersionlessDependencyIsListedOnce() throws Exception {
        assertThat(count(run("issue-973"), "\\Qlocalhost:dummy-api\\E.*->\\s+\\Q3.0\\E\\b"), is(1));
    }

    @Test
    public void testDependencyExcludesByScope() throws Exception {
        String output = run("issue-318-excludes", mojo -> {
            mojo.dependencyExcludes = Arrays.asList("*:*:*:*:*:compile", "*:*:*:*:*:test");
        });
        assertThat(output, containsString(DEPENDENCIES));
        assertListed(output, update("dummy-api", "1.0", "3.0"));
        assertNotListed(output, "dummy-impl");
    }

    @Test
    public void testDependencyIncludesWithWildcard() throws Exception {
        String output = run("issue-318-includes", mojo -> {
            mojo.dependencyIncludes = Arrays.asList("localhost:dummy-*:*:*:*:*");
        });
        assertListed(output, update("dummy-api", "1.0", "3.0"));
        assertListed(output, update("dummy-impl", "1.0", "2.2"));
    }

    @Test
    public void testDependencyIncludesWithSeveralFilters() throws Exception {
        String output = run("issue-318-includes", mojo -> {
            mojo.dependencyIncludes = Arrays.asList("*:dummy-api", "*:dummy-impl");
        });
        assertListed(output, update("dummy-api", "1.0", "3.0"));
        assertListed(output, update("dummy-impl", "1.0", "2.2"));
    }

    @Test
    public void testDependencyIncludesAndExcludes() throws Exception {
        String output = run("issue-318-includes", mojo -> {
            mojo.dependencyIncludes = Arrays.asList("localhost:dummy-*:*:*:*:*");
            mojo.dependencyExcludes = Arrays.asList("*:dummy-impl:*:*:*");
        });
        assertListed(output, update("dummy-api", "1.0", "3.0"));
        assertNotListed(output, "dummy-impl");
    }

    @Test
    public void testDependencyManagementIncludes() throws Exception {
        String output = run("issue-318-dependency-management-includes", mojo -> {
            mojo.processDependencies = false;
            setVariableValueToObject(
                    mojo, "dependencyManagementIncludes", Arrays.asList("*:*:*:*:*:null", "*:*:*:*:*:test"));
        });
        assertThat(output, containsString(DEPENDENCY_MANAGEMENT));
        assertListed(output, update("dummy-api", "1.0", "3.0"));
        assertListed(output, update("dummy-impl", "1.0", "2.2"));
    }

    /**
     * The five sections of issue 34, switched off one after another.
     */
    private void assertIssue34Sections(
            boolean dependencyManagement, boolean dependencies, boolean pluginManagement, boolean pluginDependencies)
            throws Exception {
        String output = run("issue-34", mojo -> {
            setVariableValueToObject(mojo, "processDependencyManagement", dependencyManagement);
            mojo.processDependencies = dependencies;
            setVariableValueToObject(mojo, "processPluginDependenciesInPluginManagement", pluginManagement);
            setVariableValueToObject(mojo, "processPluginDependencies", pluginDependencies);
        });
        assertSection(output, dependencyManagement, DEPENDENCY_MANAGEMENT, update("dummy-api", "1.0", "3.0"));
        assertSection(output, dependencies, DEPENDENCIES, update("dummy-api", "2.0", "3.0"));
        assertSection(output, pluginManagement, PLUGIN_MANAGEMENT, update("dummy-api", "1.2", "3.0"));
        assertSection(output, pluginDependencies, PLUGIN_DEPENDENCIES, update("dummy-api", "1.1", "3.0"));
    }

    private static void assertSection(String output, boolean expected, String header, String line) {
        if (expected) {
            assertThat(output, containsString(header));
            assertListed(output, line);
        } else {
            assertThat(output, not(containsString(header)));
            assertNotListed(output, line);
        }
    }

    @Test
    public void testIssue34AllSections() throws Exception {
        assertIssue34Sections(true, true, true, true);
    }

    @Test
    public void testIssue34WithoutPluginDependencies() throws Exception {
        assertIssue34Sections(true, true, true, false);
    }

    @Test
    public void testIssue34WithoutPluginDependenciesInPluginManagement() throws Exception {
        assertIssue34Sections(true, true, false, false);
    }

    @Test
    public void testIssue34WithoutDependencies() throws Exception {
        assertIssue34Sections(true, false, false, false);
    }

    @Test
    public void testIssue34WithoutAnything() throws Exception {
        assertIssue34Sections(false, false, false, false);
    }
}
