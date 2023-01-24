package org.codehaus.mojo.versions.filtering;

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

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.utils.DependencyComparator;

public class DependencyFilter {

    private final String pattern;
    private final List<TokenizedMatcher> matchers;

    DependencyFilter(String pattern, List<TokenizedMatcher> matchers) {
        this.pattern = pattern;
        this.matchers = matchers;
    }

    public static DependencyFilter parseFrom(List<String> dependencies) {
        List<TokenizedMatcher> matchers =
                dependencies.stream().map(TokenizedMatcher::parse).collect(Collectors.toList());

        String debugPattern = String.join(",", dependencies);

        return new DependencyFilter(debugPattern, matchers);
    }

    private static <T> Predicate<T> not(Predicate<T> predicate) {
        return x -> !predicate.test(x);
    }

    @Override
    public String toString() {
        return String.format("%s{%s}", getClass().getSimpleName(), pattern);
    }

    public Set<Dependency> retainingIn(Collection<Dependency> dependencies) {
        return filterBy(dependencies, this::matchersMatch);
    }

    public Set<Dependency> removingFrom(Collection<Dependency> dependencies) {
        return filterBy(dependencies, not(this::matchersMatch));
    }

    public boolean matchersMatch(Dependency dependency) {
        return matchers.stream().anyMatch(m -> m.test(dependency));
    }

    public boolean matchersDontMatch(Dependency dependency) {
        return !matchersMatch(dependency);
    }

    private TreeSet<Dependency> filterBy(Collection<Dependency> dependencies, Predicate<Dependency> predicate) {
        return dependencies.stream()
                .filter(predicate)
                .collect(Collectors.toCollection(() -> new TreeSet<>(DependencyComparator.INSTANCE)));
    }

    /**
     * Returns a set of dependencies filtered by the given include- and exclude filters.
     * @param dependencies collection of dependencies to filter
     * @param includes a list of dependency includes
     * @param excludes a list of dependency excludes
     * @param section if log is not null, dependency section name for the debug log
     * @param log null or log to which debug information will be logged
     * @return filtered set of dependencies
     */
    public static Set<Dependency> filterDependencies(
            Collection<Dependency> dependencies,
            List<String> includes,
            List<String> excludes,
            String section,
            Log log) {
        DependencyFilter includeDeps = DependencyFilter.parseFrom(includes);
        DependencyFilter excludeDeps = DependencyFilter.parseFrom(excludes);

        Set<Dependency> filtered = includeDeps.retainingIn(dependencies);
        filtered = excludeDeps.removingFrom(filtered);

        if (log != null && log.isDebugEnabled()) {
            log.debug(String.format("parsed includes in %s: %s -> %s", section, includes, includeDeps));
            log.debug(String.format("parsed excludes in %s: %s -> %s", section, excludes, excludeDeps));
            log.debug(String.format("Unfiltered %s: ", section) + output(dependencies));
            log.debug(String.format("Filtered %s: ", section) + output(filtered));
        }

        return filtered;
    }

    private static String output(Collection<Dependency> dependencies) {
        return dependencies.stream()
                .map(d -> String.format("%s:%s:%s", d.getGroupId(), d.getArtifactId(), d.getVersion()))
                .collect(Collectors.joining(", "));
    }
}
