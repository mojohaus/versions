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
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.api.plugin.testing.Basedir;
import org.apache.maven.api.plugin.testing.InjectMojo;
import org.apache.maven.api.plugin.testing.MojoParameter;
import org.codehaus.mojo.versions.utils.CloseableTempFile;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.DisplayDependencyUpdatesMojoScenarioTest.Section.DEPENDENCIES;
import static org.codehaus.mojo.versions.DisplayDependencyUpdatesMojoScenarioTest.Section.DEPENDENCY_MANAGEMENT;
import static org.codehaus.mojo.versions.DisplayDependencyUpdatesMojoScenarioTest.Section.PLUGIN_DEPENDENCIES;
import static org.codehaus.mojo.versions.DisplayDependencyUpdatesMojoScenarioTest.Section.PLUGIN_MANAGEMENT;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContainsPattern;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotContainsPattern;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Scenarios of {@link DisplayDependencyUpdatesMojo} that used to be the {@code it-display-dependency-updates-*}
 * integration tests. Each test runs the POM of the former IT against the versions of its test repository.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/display-dependency-updates")
class DisplayDependencyUpdatesMojoScenarioTest {

    /** The sections of the output, each with the dummy-api version issue 34 lists in it. */
    enum Section {
        DEPENDENCY_MANAGEMENT("Dependency Management", "1.0"),
        DEPENDENCIES("Dependencies", "2.0"),
        PLUGIN_MANAGEMENT("pluginManagement of plugins", "1.2"),
        PLUGIN_DEPENDENCIES("Plugin Dependencies", "1.1");

        final String header;
        final String issue34Version;

        Section(String name, String issue34Version) {
            this.header = "The following dependencies in " + name + " have newer versions:";
            this.issue34Version = issue34Version;
        }
    }

    private String run(DisplayDependencyUpdatesMojo mojo) throws Exception {
        try (CloseableTempFile tempFile = new CloseableTempFile("display-dependency-updates")) {
            mojo.outputFile = tempFile.getPath().toFile();
            mojo.repositorySystem = ItRepository.repositorySystem();
            mojo.execute();
            return new String(Files.readAllBytes(tempFile.getPath()), StandardCharsets.UTF_8);
        }
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

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "it-002/pom.xml")
    void rangeIsReportedWithItsUpdate(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextContainsPattern(run(mojo), update("dummy-api", "[1.1,3.0)", "3.0"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "it-003/pom.xml")
    void managedVersionIsNotReportedForAnExplicitOlderVersion(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextNotContainsPattern(run(mojo), "localhost:dummy-api .* 1.1");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "it-004/pom.xml")
    void managedOlderVersionIsNotReportedForAnExplicitVersion(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextNotContainsPattern(run(mojo), "localhost:dummy-api .* 1.1");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "it-007/pom.xml")
    @MojoParameter(name = "verbose", value = "true")
    void outputFileReceivesTheUpdates(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextContainsPattern(run(mojo), update("dummy-api", "1.1", "3.0"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-1001/pom.xml")
    void groupIdFromAProperty(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextContainsPattern(run(mojo), update("dummy-api", "1.0", "3.0"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-1001/pom.xml")
    @MojoParameter(name = "showNewestPerSegment", value = "true")
    void newestVersionOfEachSegment(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertTextContainsPattern(run(mojo), update("dummy-api", "1.0", "1.0.1, 1.3, 3.0"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-1001/pom.xml")
    @MojoParameter(name = "showNewestPerSegment", value = "true")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    void newestVersionOfEachSegmentWithoutMajorUpdates(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "1.0.1, 1.3"));
        assertTextNotContainsPattern(output, "3\\.0");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "it-002/pom.xml")
    @MojoParameter(name = "showNewestPerSegment", value = "true")
    void newestVersionOfEachSegmentForARange(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContainsPattern(output, update("dummy-api", "[1.1,3.0)", "3.0"));
        assertTextNotContainsPattern(output, "3\\.0,");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-973/pom.xml")
    void versionlessDependencyIsListedOnce(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertEquals(1, count(run(mojo), "\\Qlocalhost:dummy-api\\E.*->\\s+\\Q3.0\\E\\b"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-318-excludes/pom.xml")
    @MojoParameter(name = "dependencyExcludes", value = "*:*:*:*:*:compile,*:*:*:*:*:test")
    void dependencyExcludesByScope(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContains(output, DEPENDENCIES.header);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "3.0"));
        assertTextNotContainsPattern(output, "dummy-impl");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-318-includes/pom.xml")
    @MojoParameter(name = "dependencyIncludes", value = "localhost:dummy-*:*:*:*:*")
    void dependencyIncludesWithWildcard(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "3.0"));
        assertTextContainsPattern(output, update("dummy-impl", "1.0", "2.2"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-318-includes/pom.xml")
    @MojoParameter(name = "dependencyIncludes", value = "*:dummy-api,*:dummy-impl")
    void dependencyIncludesWithSeveralFilters(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "3.0"));
        assertTextContainsPattern(output, update("dummy-impl", "1.0", "2.2"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-318-includes/pom.xml")
    @MojoParameter(name = "dependencyIncludes", value = "localhost:dummy-*:*:*:*:*")
    @MojoParameter(name = "dependencyExcludes", value = "*:dummy-impl:*:*:*")
    void dependencyIncludesAndExcludes(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "3.0"));
        assertTextNotContainsPattern(output, "dummy-impl");
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-318-dependency-management-includes/pom.xml")
    @MojoParameter(name = "processDependencies", value = "false")
    @MojoParameter(name = "dependencyManagementIncludes", value = "*:*:*:*:*:null,*:*:*:*:*:test")
    void dependencyManagementIncludes(DisplayDependencyUpdatesMojo mojo) throws Exception {
        String output = run(mojo);
        assertTextContains(output, DEPENDENCY_MANAGEMENT.header);
        assertTextContainsPattern(output, update("dummy-api", "1.0", "3.0"));
        assertTextContainsPattern(output, update("dummy-impl", "1.0", "2.2"));
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-34/pom.xml")
    void issue34ShowsAllSections(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertShownSections(run(mojo), DEPENDENCY_MANAGEMENT, DEPENDENCIES, PLUGIN_MANAGEMENT, PLUGIN_DEPENDENCIES);
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-34/pom.xml")
    @MojoParameter(name = "processPluginDependencies", value = "false")
    void issue34WithoutPluginDependencies(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertShownSections(run(mojo), DEPENDENCY_MANAGEMENT, DEPENDENCIES, PLUGIN_MANAGEMENT);
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-34/pom.xml")
    @MojoParameter(name = "processPluginDependencies", value = "false")
    @MojoParameter(name = "processPluginDependenciesInPluginManagement", value = "false")
    void issue34WithoutPluginManagement(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertShownSections(run(mojo), DEPENDENCY_MANAGEMENT, DEPENDENCIES);
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-34/pom.xml")
    @MojoParameter(name = "processPluginDependencies", value = "false")
    @MojoParameter(name = "processPluginDependenciesInPluginManagement", value = "false")
    @MojoParameter(name = "processDependencies", value = "false")
    void issue34WithDependencyManagementOnly(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertShownSections(run(mojo), DEPENDENCY_MANAGEMENT);
    }

    @Test
    @InjectMojo(goal = "display-dependency-updates", pom = "issue-34/pom.xml")
    @MojoParameter(name = "processPluginDependencies", value = "false")
    @MojoParameter(name = "processPluginDependenciesInPluginManagement", value = "false")
    @MojoParameter(name = "processDependencies", value = "false")
    @MojoParameter(name = "processDependencyManagement", value = "false")
    void issue34WithoutAnySection(DisplayDependencyUpdatesMojo mojo) throws Exception {
        assertShownSections(run(mojo));
    }

    /** Exactly these sections are in the output, each listing its dummy-api update. */
    private static void assertShownSections(String output, Section... expected) {
        Set<Section> shown = EnumSet.noneOf(Section.class);
        for (Section section : Section.values()) {
            if (output.contains(section.header)
                    && count(output, update("dummy-api", section.issue34Version, "3.0")) > 0) {
                shown.add(section);
            }
        }
        Set<Section> want = EnumSet.noneOf(Section.class);
        want.addAll(Arrays.asList(expected));
        assertEquals(want, shown, output);
    }
}
