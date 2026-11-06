package org.codehaus.mojo.versions.rule;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.IgnoreVersionHelper;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class DependabotRulesTest {

    private static final String CONFIG = String.join(
            "\n",
            "version: 2",
            "updates:",
            "  - package-ecosystem: \"maven\"",
            "    directory: \"/\"",
            "    ignore:",
            "      - dependency-name: \"jakarta.annotation:jakarta.annotation-api\"",
            "        versions: [ \"[2.0.0,)\" ]",
            "  - package-ecosystem: \"maven\"",
            "    directory: \"/\"",
            "    target-branch: \"maven-3.10.x\"",
            "    ignore:",
            "      - dependency-name: \"ch.qos.logback:*\"",
            "        versions: [ \"[1.3.0,)\" ]",
            "      - dependency-name: \"org.mockito:*\"",
            "        versions: [\">= 5.0.0\"]",
            "      - dependency-name: \"org.example:pinned\"",
            "      - dependency-name: \"org.example:majors\"",
            "        update-types: [\"version-update:semver-major\"]",
            "      - dependency-name: \"ch.qos.logback:logback-classic\"",
            "        versions: [\"[1.2.14]\"]",
            "        update-types: [\"version-update:semver-minor\"]",
            "  - package-ecosystem: \"github-actions\"",
            "    directory: \"/\"",
            "    target-branch: \"maven-3.10.x\"",
            "    ignore:",
            "      - dependency-name: \"actions/checkout\"",
            "");

    private final Log log = mock(Log.class);

    @TempDir
    Path dir;

    @Test
    void comparatorsBecomeRanges() {
        assertEquals("[5.0.0,)", DependabotRules.comparatorsToRange(">= 5.0.0"));
        assertEquals("(5.0.0,)", DependabotRules.comparatorsToRange("> 5.0.0"));
        assertEquals("(,2)", DependabotRules.comparatorsToRange("< 2"));
        assertEquals("[1.0,2.0)", DependabotRules.comparatorsToRange(">= 1.0, < 2.0"));
        assertEquals("[1.0,2.0]", DependabotRules.comparatorsToRange(">=1.0,<=2.0"));
        assertEquals("[1.2.3]", DependabotRules.comparatorsToRange("= 1.2.3"));
        assertNull(DependabotRules.comparatorsToRange(">= 1, >= 2"));
        assertNull(DependabotRules.comparatorsToRange("~> 1.2"));
    }

    @Test
    void versionItemsBecomeIgnoreVersions() {
        IgnoreVersion range = DependabotRules.toIgnoreVersion("[1.3.0, )");
        assertEquals(IgnoreVersion.TYPE_RANGE, range.getType());
        assertEquals("[1.3.0,)", range.getVersion());

        IgnoreVersion exact = DependabotRules.toIgnoreVersion("1.2.3");
        assertEquals(IgnoreVersion.TYPE_EXACT, exact.getType());
        assertEquals("1.2.3", exact.getVersion());

        IgnoreVersion prefix = DependabotRules.toIgnoreVersion("1.2.+");
        assertEquals(IgnoreVersion.TYPE_RANGE, prefix.getType());
        assertEquals("[1.2,1.3)", prefix.getVersion());
        assertEquals("[1,2)", DependabotRules.toIgnoreVersion("1.+").getVersion());

        assertNull(DependabotRules.toIgnoreVersion("2.*"));
        assertNull(DependabotRules.toIgnoreVersion("[1.3.0"));
        assertNull(DependabotRules.toIgnoreVersion(">= 2, < 1"));
        assertNull(DependabotRules.toIgnoreVersion(""));
    }

    @Test
    void anInclusiveLowerBoundAlsoIgnoresItsPreReleases() {
        IgnoreVersion preReleases = DependabotRules.preReleasesOfLowerBound(DependabotRules.toIgnoreVersion("[1.3.0,)"))
                .orElseThrow(AssertionError::new);
        assertTrue(IgnoreVersionHelper.isVersionIgnored("1.3.0-beta0", preReleases));
        assertTrue(IgnoreVersionHelper.isVersionIgnored("1.3.0.Beta1", preReleases));
        assertFalse(IgnoreVersionHelper.isVersionIgnored("1.2.13", preReleases));
        assertFalse(IgnoreVersionHelper.isVersionIgnored("1.3.01", preReleases));

        assertEquals(
                Optional.empty(), DependabotRules.preReleasesOfLowerBound(DependabotRules.toIgnoreVersion("(1.3.0,)")));
        assertEquals(Optional.empty(), DependabotRules.preReleasesOfLowerBound(DependabotRules.toIgnoreVersion("< 2")));
        assertEquals(
                Optional.empty(),
                DependabotRules.preReleasesOfLowerBound(DependabotRules.toIgnoreVersion("[2.0-rc1,)")));
    }

    @Test
    void namesMatchLikeDependabot() {
        List<IgnoreVersion> one = singletonList(DependabotRules.toIgnoreVersion("1.0"));

        DependabotRules.Ignore suffix = new DependabotRules.Ignore("*-core", one);
        assertTrue(suffix.matches("org.mockito", "mockito-core"));
        assertFalse(suffix.matches("org.mockito", "mockito-junit-jupiter"));

        assertFalse(new DependabotRules.Ignore("org.mockito", one).matches("org.mockito", "mockito-core"));
        assertTrue(new DependabotRules.Ignore("org.mockito*", one).matches("org.mockito", "mockito-core"));
        assertTrue(new DependabotRules.Ignore("Org.Mockito:Mockito-*", one).matches("org.mockito", "mockito-core"));
        assertFalse(new DependabotRules.Ignore("org.a.b:c", one).matches("orgxa.b", "c"), "'.' must be literal");
    }

    @Test
    void readsTheEntryForTheTargetBranch() throws Exception {
        List<DependabotRules.Ignore> ignores = DependabotRules.read(write(CONFIG), "maven-3.10.x", log);

        assertEquals(4, ignores.size(), ignores::toString);
        assertIgnore(ignores.get(0), "ch.qos.logback:*", IgnoreVersion.TYPE_RANGE, "[1.3.0,)");
        assertIgnore(ignores.get(1), "org.mockito:*", IgnoreVersion.TYPE_RANGE, "[5.0.0,)");
        assertIgnore(ignores.get(2), "org.example:pinned", IgnoreVersion.TYPE_REGEX, ".*");
        assertIgnore(ignores.get(3), "ch.qos.logback:logback-classic", IgnoreVersion.TYPE_RANGE, "[1.2.14]");
        verify(log).warn(contains("org.example:majors"));
        verify(log).warn(contains("ignore item for ch.qos.logback:logback-classic"));
    }

    @Test
    void fallsBackToTheDefaultBranchEntry() throws Exception {
        List<DependabotRules.Ignore> forUnknownBranch = DependabotRules.read(write(CONFIG), "feature/x", log);
        List<DependabotRules.Ignore> forNoBranch = DependabotRules.read(write(CONFIG), null, log);

        for (List<DependabotRules.Ignore> ignores : asList(forUnknownBranch, forNoBranch)) {
            assertEquals(1, ignores.size(), ignores::toString);
            assertIgnore(
                    ignores.get(0), "jakarta.annotation:jakarta.annotation-api", IgnoreVersion.TYPE_RANGE, "[2.0.0,)");
        }
    }

    @Test
    void rejectsInvalidYaml() throws IOException {
        Path config = write("updates: [\n");
        assertThrows(MojoExecutionException.class, () -> DependabotRules.read(config, null, log));
    }

    @Test
    void detectsTheCheckedOutBranch() throws IOException {
        Path repo = Files.createDirectories(dir.resolve("repo"));
        Files.createDirectories(repo.resolve(".git"));
        Files.write(repo.resolve(".git/HEAD"), "ref: refs/heads/maven-3.10.x\n".getBytes(StandardCharsets.UTF_8));
        Path github = Files.createDirectories(repo.resolve(".github"));
        assertEquals(Optional.of("maven-3.10.x"), DependabotRules.currentBranch(github));

        Path worktreeGitDir = Files.createDirectories(dir.resolve("gitdirs/wt"));
        Files.write(worktreeGitDir.resolve("HEAD"), "ref: refs/heads/topic\n".getBytes(StandardCharsets.UTF_8));
        Path worktree = Files.createDirectories(dir.resolve("wt"));
        Files.write(worktree.resolve(".git"), ("gitdir: " + worktreeGitDir + "\n").getBytes(StandardCharsets.UTF_8));
        assertEquals(Optional.of("topic"), DependabotRules.currentBranch(worktree));

        Files.write(repo.resolve(".git/HEAD"), "3f135b98c0ffee\n".getBytes(StandardCharsets.UTF_8));
        assertEquals(Optional.empty(), DependabotRules.currentBranch(github));
    }

    @Test
    void everyMatchingItemAppliesAlongsideTheBestFitRule() throws Exception {
        Path config = write(CONFIG);
        RuleSet ruleSet = new RuleSet();
        ruleSet.setRules(asList(rule("ch.qos.logback", "logback-classic", ".*-rc.*"), rule("org.*", "*", ".*-beta.*")));

        RuleService service = new RulesServiceBuilder()
                .withMavenSession(session(dir))
                .withLog(log)
                .withRuleSet(ruleSet)
                .withDependabotConfig(config.getFileName().toString())
                .withDependabotTargetBranch("maven-3.10.x")
                .build();

        // the more specific configured rule does not hide ch.qos.logback:*, and both Dependabot items apply
        assertTrue(isIgnored(service, "ch.qos.logback", "logback-classic", "1.6.6"));
        assertTrue(isIgnored(service, "ch.qos.logback", "logback-classic", "1.3.0-beta0"));
        assertTrue(isIgnored(service, "ch.qos.logback", "logback-classic", "1.2.14"));
        assertTrue(isIgnored(service, "ch.qos.logback", "logback-classic", "1.2.15-rc1"));
        assertFalse(isIgnored(service, "ch.qos.logback", "logback-classic", "1.2.13"));
        // the more specific Dependabot item does not hide the configured org.* rule either
        assertTrue(isIgnored(service, "org.mockito", "mockito-core", "5.24.0"));
        assertTrue(isIgnored(service, "org.mockito", "mockito-core", "4.11.1-beta1"));
        assertFalse(isIgnored(service, "org.mockito", "mockito-core", "4.11.0"));
        assertFalse(isIgnored(service, "org.example", "other", "9.9"));
        assertEquals(1, ruleSet.getRules().get(0).getIgnoreVersions().size(), "the configured rules must not change");
    }

    @Test
    void theBranchOfTheBuiltTreeSelectsTheEntries() throws Exception {
        Path exported = Files.createDirectories(dir.resolve("exported"));
        Path config = Files.write(exported.resolve("dependabot.yml"), CONFIG.getBytes(StandardCharsets.UTF_8));
        Path tree = Files.createDirectories(dir.resolve("tree"));
        Files.createDirectories(tree.resolve(".git"));
        Files.write(tree.resolve(".git/HEAD"), "ref: refs/heads/maven-3.10.x\n".getBytes(StandardCharsets.UTF_8));

        RuleService service = new RulesServiceBuilder()
                .withMavenSession(session(tree))
                .withLog(log)
                .withDependabotConfig(config.toString())
                .build();

        assertTrue(isIgnored(service, "org.mockito", "mockito-core", "5.24.0"));
        assertFalse(isIgnored(service, "jakarta.annotation", "jakarta.annotation-api", "3.0.0"));
    }

    @Test
    void builderFailsOnAMissingFile() {
        MavenSession session = session(new File(dir.toFile(), "absent").toPath());

        RulesServiceBuilder builder = new RulesServiceBuilder()
                .withMavenSession(session)
                .withLog(log)
                .withDependabotConfig(".github/dependabot.yml");
        MojoExecutionException e = assertThrows(MojoExecutionException.class, builder::build);
        assertTrue(e.getMessage().contains("dependabot.yml"), e::getMessage);
        verify(log, never()).info(anyString());
    }

    private static boolean isIgnored(RuleService service, String groupId, String artifactId, String version) {
        return service.getIgnoredVersions(groupId, artifactId).stream()
                .anyMatch(v -> IgnoreVersionHelper.isVersionIgnored(version, v));
    }

    private static void assertIgnore(DependabotRules.Ignore ignore, String name, String type, String version) {
        assertEquals(name, ignore.getDependencyName());
        assertEquals(type, ignore.getIgnoreVersions().get(0).getType(), ignore::toString);
        assertEquals(version, ignore.getIgnoreVersions().get(0).getVersion(), ignore::toString);
    }

    private static Rule rule(String groupId, String artifactId, String regex) {
        IgnoreVersion ignoreVersion = new IgnoreVersion();
        ignoreVersion.setType(IgnoreVersion.TYPE_REGEX);
        ignoreVersion.setVersion(regex);
        Rule rule = new Rule();
        rule.setGroupId(groupId);
        rule.setArtifactId(artifactId);
        rule.setIgnoreVersions(new ArrayList<>(singletonList(ignoreVersion)));
        return rule;
    }

    private static MavenSession session(Path reactorRoot) {
        MavenSession session = mock(MavenSession.class);
        MavenExecutionRequest request = mock(MavenExecutionRequest.class);
        when(session.getRequest()).thenReturn(request);
        when(request.getMultiModuleProjectDirectory()).thenReturn(reactorRoot.toFile());
        return session;
    }

    private Path write(String content) throws IOException {
        return Files.write(dir.resolve("dependabot.yml"), content.getBytes(StandardCharsets.UTF_8));
    }
}
