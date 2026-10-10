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

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.eclipse.aether.RepositorySystem;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;

/**
 * A {@link RepositorySystem} offering the versions in {@code src/it-repo}, the repository the integration tests
 * resolve against, so tests moved out of {@code src/it} see the same versions.
 */
final class ItRepository {

    /** {@code artifactId-version.pom}, the version starting at the first dash followed by a digit. */
    private static final Pattern FILE = Pattern.compile("(.+?)-(\\d.*)\\.(?:pom|jar)");

    /** A timestamped snapshot, which the repository serves as {@code -SNAPSHOT}. */
    private static final Pattern TIMESTAMP = Pattern.compile("-\\d{8}\\.\\d{6}-\\d+$");

    private ItRepository() {}

    static RepositorySystem repositorySystem() {
        try (Stream<Path> files = Files.list(Paths.get("src/it-repo"))) {
            Map<String, TreeSet<String>> versions = files.map(
                            f -> FILE.matcher(f.getFileName().toString()))
                    .filter(Matcher::matches)
                    .collect(Collectors.groupingBy(
                            m -> m.group(1),
                            Collectors.mapping(
                                    m -> TIMESTAMP.matcher(m.group(2)).replaceFirst("-SNAPSHOT"),
                                    Collectors.toCollection(TreeSet::new))));
            return mockAetherRepositorySystem(versions.entrySet().stream()
                    .collect(Collectors.toMap(
                            Map.Entry::getKey, e -> e.getValue().toArray(new String[0]))));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
