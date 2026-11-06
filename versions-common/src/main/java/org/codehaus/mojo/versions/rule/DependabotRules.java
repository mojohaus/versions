package org.codehaus.mojo.versions.rule;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;
import org.yaml.snakeyaml.error.YAMLException;

/**
 * Turns the {@code ignore} entries of a Dependabot configuration file into version rules.
 *
 * <p>Only the {@code package-ecosystem: maven} entries are read, and only those whose
 * {@code target-branch} matches the requested branch; when none matches, the entries without a
 * {@code target-branch} (the repository's default branch) are used.</p>
 *
 * <p>Each {@code ignore} item becomes an {@link Ignore}. As in Dependabot, its
 * {@code dependency-name} is matched case-insensitively against the whole
 * {@code groupId:artifactId}, with {@code *} matching any characters including {@code :}, and
 * every matching item applies, independently of the plugin's best-fit {@link
 * org.codehaus.mojo.versions.model.Rule rules}. A {@code versions} item is a Maven version range,
 * a comparator requirement such as {@code ">= 5.0.0"} or {@code ">= 1.0, < 2.0"}, a prefix such
 * as {@code 1.2.+}, or a single version; an item with neither {@code versions} nor
 * {@code update-types} ignores every version. {@code update-types} is relative to the version in
 * use and has no static equivalent, so it is skipped with a warning.</p>
 *
 * @since 2.25.0
 */
public final class DependabotRules {

    private static final Pattern COMPARATOR = Pattern.compile("\\s*(>=|<=|>|<|=)\\s*([^\\s,]+)\\s*");

    private static final Pattern PREFIX = Pattern.compile("((?:\\d+\\.)*)(\\d+)\\.\\+");

    private static final Pattern RELEASE = Pattern.compile("\\d+(\\.\\d+)*");

    private static final Pattern GIT_HEAD_REF = Pattern.compile("ref:\\s*refs/heads/(.+)");

    private DependabotRules() {}

    /**
     * One {@code ignore} item: the versions to ignore for the artifacts its name matches.
     */
    public static final class Ignore {
        private final String dependencyName;

        private final Pattern pattern;

        private final List<IgnoreVersion> ignoreVersions;

        Ignore(String dependencyName, List<IgnoreVersion> ignoreVersions) {
            this.dependencyName = dependencyName;
            // Dependabot's wildcard_match?: quote the parts between '*' and join them with ".*"
            List<String> parts = new ArrayList<>();
            for (String part : dependencyName.toLowerCase(Locale.ROOT).split("\\*", -1)) {
                parts.add(Pattern.quote(part));
            }
            this.pattern = Pattern.compile(String.join(".*", parts));
            this.ignoreVersions = Collections.unmodifiableList(ignoreVersions);
        }

        /**
         * Tells whether this item applies to an artifact.
         *
         * @param groupId the artifact's groupId
         * @param artifactId the artifact's artifactId
         * @return {@code true} if {@code dependency-name} matches {@code groupId:artifactId}
         */
        public boolean matches(String groupId, String artifactId) {
            return pattern.matcher((groupId + ':' + artifactId).toLowerCase(Locale.ROOT))
                    .matches();
        }

        /**
         * @return the {@code dependency-name} of the item
         */
        public String getDependencyName() {
            return dependencyName;
        }

        /**
         * @return the versions to ignore, never empty
         */
        public List<IgnoreVersion> getIgnoreVersions() {
            return ignoreVersions;
        }

        @Override
        public String toString() {
            return "Ignore[" + dependencyName + ", " + ignoreVersions + "]";
        }
    }

    /**
     * Reads the ignore items for the given branch.
     *
     * @param file the Dependabot configuration file
     * @param targetBranch the branch whose entries apply, or {@code null} for the default-branch entries
     * @param log the log to report the chosen entries and the skipped items to
     * @return the ignore items, never {@code null}
     * @throws MojoExecutionException if the file cannot be read or is not valid YAML
     */
    public static List<Ignore> read(Path file, String targetBranch, Log log) throws MojoExecutionException {
        Object document;
        try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
            document = new Yaml(new SafeConstructor(new LoaderOptions())).load(reader);
        } catch (IOException | YAMLException e) {
            throw new MojoExecutionException("Could not read the Dependabot configuration " + file, e);
        }

        List<Map<?, ?>> mavenEntries = new ArrayList<>();
        for (Object update : list(map(document).get("updates"))) {
            Map<?, ?> entry = map(update);
            if ("maven".equals(entry.get("package-ecosystem"))) {
                mavenEntries.add(entry);
            }
        }

        List<Map<?, ?>> selected = select(mavenEntries, targetBranch);
        if (selected.isEmpty()) {
            log.warn("No maven entry in " + file + " applies to "
                    + (targetBranch == null ? "the default branch" : "branch " + targetBranch));
            return Collections.emptyList();
        }
        boolean branchMatched =
                targetBranch != null && targetBranch.equals(selected.get(0).get("target-branch"));
        log.info("Using the Dependabot ignore rules for "
                + (branchMatched ? "target-branch " + targetBranch : "the default branch") + " from " + file);

        List<Ignore> ignores = new ArrayList<>();
        for (Map<?, ?> entry : selected) {
            for (Object item : list(entry.get("ignore"))) {
                toIgnore(map(item), log).ifPresent(ignores::add);
            }
        }
        return ignores;
    }

    /**
     * Returns the branch checked out in the git working tree containing {@code directory}.
     *
     * @param directory a directory inside the working tree
     * @return the branch name, or empty for a detached HEAD or a directory outside a git working tree
     */
    public static Optional<String> currentBranch(Path directory) {
        for (Path dir = directory.toAbsolutePath(); dir != null; dir = dir.getParent()) {
            Path git = dir.resolve(".git");
            try {
                Path head;
                if (Files.isDirectory(git)) {
                    head = git.resolve("HEAD");
                } else if (Files.isRegularFile(git)) {
                    // a linked worktree or submodule: ".git" is a file holding "gitdir: <path>"
                    String pointer = new String(Files.readAllBytes(git), StandardCharsets.UTF_8).trim();
                    if (!pointer.startsWith("gitdir:")) {
                        return Optional.empty();
                    }
                    head = dir.resolve(pointer.substring("gitdir:".length()).trim())
                            .resolve("HEAD");
                } else {
                    continue;
                }
                Matcher m = GIT_HEAD_REF.matcher(new String(Files.readAllBytes(head), StandardCharsets.UTF_8).trim());
                return m.matches() ? Optional.of(m.group(1)) : Optional.empty();
            } catch (IOException e) {
                return Optional.empty();
            }
        }
        return Optional.empty();
    }

    private static List<Map<?, ?>> select(List<Map<?, ?>> entries, String targetBranch) {
        List<Map<?, ?>> forBranch = new ArrayList<>();
        List<Map<?, ?>> forDefault = new ArrayList<>();
        for (Map<?, ?> entry : entries) {
            Object branch = entry.get("target-branch");
            if (branch == null) {
                forDefault.add(entry);
            } else if (branch.toString().equals(targetBranch)) {
                forBranch.add(entry);
            }
        }
        return forBranch.isEmpty() ? forDefault : forBranch;
    }

    private static Optional<Ignore> toIgnore(Map<?, ?> item, Log log) {
        Object name = item.get("dependency-name");
        if (name == null) {
            log.warn("Skipping a Dependabot ignore item without dependency-name: " + item);
            return Optional.empty();
        }
        String dependencyName = name.toString();
        List<Object> versions = list(item.get("versions"));
        boolean hasUpdateTypes = !list(item.get("update-types")).isEmpty();
        if (hasUpdateTypes) {
            log.warn("Ignoring update-types of the Dependabot ignore item for " + dependencyName
                    + ": it depends on the version in use and has no equivalent version rule");
            if (versions.isEmpty()) {
                return Optional.empty();
            }
        }

        List<IgnoreVersion> ignoreVersions = new ArrayList<>();
        if (versions.isEmpty()) {
            ignoreVersions.add(ignoreVersion(IgnoreVersion.TYPE_REGEX, ".*"));
        }
        for (Object version : versions) {
            IgnoreVersion ignoreVersion =
                    toIgnoreVersion(Objects.toString(version).trim());
            if (ignoreVersion == null) {
                log.warn("Skipping the Dependabot ignore version '" + version + "' for " + dependencyName
                        + ": not a Maven version range, comparator, prefix or version");
            } else {
                ignoreVersions.add(ignoreVersion);
                preReleasesOfLowerBound(ignoreVersion).ifPresent(ignoreVersions::add);
            }
        }
        return ignoreVersions.isEmpty() ? Optional.empty() : Optional.of(new Ignore(dependencyName, ignoreVersions));
    }

    /**
     * Converts one Dependabot {@code versions} item to an {@link IgnoreVersion}.
     *
     * @param version the item, e.g. {@code "[1.3.0,)"}, {@code ">= 5.0.0"}, {@code "1.2.+"} or {@code "1.2.3"}
     * @return the ignore version, or {@code null} if the item has no valid Maven equivalent
     */
    static IgnoreVersion toIgnoreVersion(String version) {
        if (version.isEmpty()) {
            return null;
        }
        char first = version.charAt(0);
        if (first == '[' || first == '(') {
            return range(version.replace(" ", ""));
        }
        if (first == '>' || first == '<' || first == '=') {
            String range = comparatorsToRange(version);
            return range == null ? null : range(range);
        }
        Matcher prefix = PREFIX.matcher(version);
        if (prefix.matches()) {
            String head = prefix.group(1);
            long last = Long.parseLong(prefix.group(2));
            return range("[" + head + last + "," + head + (last + 1) + ")");
        }
        if (version.contains("*") || version.contains("+") || version.contains(" ")) {
            return null;
        }
        return ignoreVersion(IgnoreVersion.TYPE_EXACT, version);
    }

    /**
     * Returns a rule for the pre-releases of a range's inclusive lower bound. Maven orders
     * {@code 1.3.0-beta1} before {@code 1.3.0}, so {@code [1.3.0,)} alone lets it through, while
     * Dependabot, which never proposes a pre-release for a release dependency, treats the line as
     * ignored. Versions such as {@code 1.3.0-1} or {@code 1.3.0.1} also match, but they lie inside
     * the range anyway.
     *
     * @param ignoreVersion a converted {@code versions} item
     * @return a regex rule for {@code <lower>-*} and {@code <lower>.*}, or empty if the item has no
     *     inclusive lower bound made only of numbers
     */
    static Optional<IgnoreVersion> preReleasesOfLowerBound(IgnoreVersion ignoreVersion) {
        String range = ignoreVersion.getVersion();
        if (!IgnoreVersion.TYPE_RANGE.equals(ignoreVersion.getType()) || !range.startsWith("[")) {
            return Optional.empty();
        }
        int end = range.indexOf(',');
        String lower = end < 0 ? "" : range.substring(1, end);
        if (!RELEASE.matcher(lower).matches()) {
            return Optional.empty();
        }
        return Optional.of(ignoreVersion(IgnoreVersion.TYPE_REGEX, Pattern.quote(lower) + "[-.].*"));
    }

    /**
     * Converts a comma-separated list of comparators to a single Maven range.
     *
     * @param requirement e.g. {@code ">= 1.0, < 2.0"}
     * @return e.g. {@code "[1.0,2.0)"}, or {@code null} if the requirement has no single-range equivalent
     */
    static String comparatorsToRange(String requirement) {
        String lower = null;
        String upper = null;
        boolean lowerInclusive = false;
        boolean upperInclusive = false;
        for (String part : requirement.split(",")) {
            Matcher m = COMPARATOR.matcher(part);
            if (!m.matches()) {
                return null;
            }
            String op = m.group(1);
            String v = m.group(2);
            switch (op) {
                case "=":
                    return "[" + v + "]";
                case ">=":
                case ">":
                    if (lower != null) {
                        return null;
                    }
                    lower = v;
                    lowerInclusive = op.equals(">=");
                    break;
                default:
                    if (upper != null) {
                        return null;
                    }
                    upper = v;
                    upperInclusive = op.equals("<=");
                    break;
            }
        }
        return (lowerInclusive ? "[" : "(")
                + (lower == null ? "" : lower)
                + ","
                + (upper == null ? "" : upper)
                + (upperInclusive ? "]" : ")");
    }

    private static IgnoreVersion range(String spec) {
        try {
            VersionRange.createFromVersionSpec(spec);
        } catch (InvalidVersionSpecificationException e) {
            return null;
        }
        return ignoreVersion(IgnoreVersion.TYPE_RANGE, spec);
    }

    private static IgnoreVersion ignoreVersion(String type, String version) {
        IgnoreVersion ignoreVersion = new IgnoreVersion();
        ignoreVersion.setType(type);
        ignoreVersion.setVersion(version);
        return ignoreVersion;
    }

    private static Map<?, ?> map(Object value) {
        return value instanceof Map ? (Map<?, ?>) value : Collections.emptyMap();
    }

    @SuppressWarnings("unchecked")
    private static List<Object> list(Object value) {
        if (value instanceof List) {
            return (List<Object>) value;
        }
        return value == null ? Collections.emptyList() : Collections.singletonList(value);
    }
}
