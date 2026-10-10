package org.codehaus.mojo.versions.filtering;

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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertSameElements;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DependencyFilterTest {

    @Nested
    class RemoveFromTest {
        private final Set<Dependency> input = new HashSet<>(asList(
                DependencyBuilder.newBuilder()
                        .withGroupId("foo")
                        .withArtifactId("bar")
                        .withVersion("1")
                        .build(),
                DependencyBuilder.newBuilder()
                        .withGroupId("localhost")
                        .withArtifactId("my-api")
                        .withVersion("2")
                        .build(),
                DependencyBuilder.newBuilder()
                        .withGroupId("localhost")
                        .withArtifactId("my-impl")
                        .withVersion("3")
                        .build()));

        @Test
        void removesExcludedDepsWithExactMatch() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:my-impl:3"));

            Set<Dependency> actual = exclusions.removingFrom(input);

            assertSameElements(Arrays.asList("foo:bar:1", "localhost:my-api:2"), gavs(actual));
        }

        @Test
        void removesExcludedDepsWithWildcardInVersion() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:my-impl:*"));

            Set<Dependency> actual = exclusions.removingFrom(input);

            assertSameElements(Arrays.asList("foo:bar:1", "localhost:my-api:2"), gavs(actual));
        }

        @Test
        void removesExcludedDepsWithWildcardInGroupId() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:*:*"));

            Set<Dependency> actual = exclusions.removingFrom(input);

            assertSameElements(Arrays.asList("foo:bar:1"), gavs(actual));
        }

        @Test
        void removesExcludedDepsWithAllWildcards() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("*:*:*"));

            Set<Dependency> actual = exclusions.removingFrom(input);

            assertTrue(actual.isEmpty(), () -> String.valueOf(actual));
        }

        @Test
        void removesMultiplePatterns() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(asList("*:my-api", "*:my-impl"));

            Set<Dependency> actual = exclusions.removingFrom(input);

            assertSameElements(Arrays.asList("foo:bar:1"), gavs(actual));
        }
    }

    @Nested
    class RetainingInTest {
        private final Set<Dependency> input = new HashSet<>(asList(
                DependencyBuilder.newBuilder()
                        .withGroupId("foo")
                        .withArtifactId("bar")
                        .withVersion("1")
                        .build(),
                DependencyBuilder.newBuilder()
                        .withGroupId("localhost")
                        .withArtifactId("my-api")
                        .withVersion("2")
                        .build(),
                DependencyBuilder.newBuilder()
                        .withGroupId("localhost")
                        .withArtifactId("my-impl")
                        .withVersion("3")
                        .build()));

        @Test
        void retainsOnlyDepsWithExactMatch() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:my-impl:3"));

            Set<Dependency> actual = exclusions.retainingIn(input);

            assertSameElements(Arrays.asList("localhost:my-impl:3"), gavs(actual));
        }

        @Test
        void retainsOnlyDepsMatchingWildcardInVersion() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:my-api:*"));

            Set<Dependency> actual = exclusions.retainingIn(input);

            assertSameElements(Arrays.asList("localhost:my-api:2"), gavs(actual));
        }

        @Test
        void retainsOnlyDepsWithMultipleWildcards() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("localhost:my-*:*"));

            Set<Dependency> actual = exclusions.retainingIn(input);

            assertSameElements(Arrays.asList("localhost:my-api:2", "localhost:my-impl:3"), gavs(actual));
        }

        @Test
        void retainsAllOnAllWildcards() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(Collections.singletonList("*:*:*"));

            Set<Dependency> actual = exclusions.retainingIn(input);

            assertSameElements(Arrays.asList("foo:bar:1", "localhost:my-api:2", "localhost:my-impl:3"), gavs(actual));
        }

        @Test
        void retainsMultiplePatterns() {
            DependencyFilter exclusions = DependencyFilter.parseFrom(asList("*:my-api", "*:my-impl"));

            Set<Dependency> actual = exclusions.retainingIn(input);

            assertSameElements(Arrays.asList("localhost:my-api:2", "localhost:my-impl:3"), gavs(actual));
        }
    }

    private static List<String> gavs(Collection<Dependency> dependencies) {
        return dependencies.stream()
                .map(d -> d.getGroupId() + ":" + d.getArtifactId() + ":" + d.getVersion())
                .collect(Collectors.toList());
    }
}
