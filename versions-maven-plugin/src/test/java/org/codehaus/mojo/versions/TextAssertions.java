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

import java.util.List;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Assertions on text that JUnit does not have, each reporting the text it checked when it fails.
 */
final class TextAssertions {

    private TextAssertions() {}

    static void assertContains(String actual, String... expected) {
        for (String part : expected) {
            if (actual == null || !actual.contains(part)) {
                fail("expected to contain \"" + part + "\" but was:\n" + actual);
            }
        }
    }

    static void assertNotContains(String actual, String... unexpected) {
        for (String part : unexpected) {
            if (actual != null && actual.contains(part)) {
                fail("expected not to contain \"" + part + "\" but was:\n" + actual);
            }
        }
    }

    static void assertContainsInOrder(String actual, String... parts) {
        int from = 0;
        for (String part : parts) {
            int at = actual == null ? -1 : actual.indexOf(part, from);
            if (at < 0) {
                fail("expected to contain \"" + part + "\" after position " + from + " but was:\n" + actual);
            }
            from = at + part.length();
        }
    }

    /**
     * Matches the whole text against the expression, like {@link java.util.regex.Matcher#matches()}.
     */
    static void assertMatches(String actual, String regex) {
        if (actual == null || !Pattern.compile(regex).matcher(actual).matches()) {
            fail("expected to match " + regex + " but was:\n" + actual);
        }
    }

    static void assertAnyLineContains(List<String> lines, String expected) {
        if (lines.stream().noneMatch(line -> line.contains(expected))) {
            fail("expected a line containing \"" + expected + "\" but was:\n" + String.join("\n", lines));
        }
    }
}
