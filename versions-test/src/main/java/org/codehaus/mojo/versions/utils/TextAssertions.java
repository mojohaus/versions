package org.codehaus.mojo.versions.utils;

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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Assertions on text and collections that JUnit does not have, each reporting what it checked when it fails.
 */
public final class TextAssertions {

    private TextAssertions() {}

    /**
     * Asserts that the text contains each of the parts.
     *
     * @param actual the text
     * @param expected the parts it must contain
     */
    public static void assertTextContains(String actual, String... expected) {
        for (String part : expected) {
            if (actual == null || !actual.contains(part)) {
                fail("expected to contain \"" + part + "\" but was:\n" + actual);
            }
        }
    }

    /**
     * Asserts that the text contains none of the parts.
     *
     * @param actual the text
     * @param unexpected the parts it must not contain
     */
    public static void assertTextNotContains(String actual, String... unexpected) {
        for (String part : unexpected) {
            if (actual != null && actual.contains(part)) {
                fail("expected not to contain \"" + part + "\" but was:\n" + actual);
            }
        }
    }

    /**
     * Asserts that the text contains the parts one after another.
     *
     * @param actual the text
     * @param parts the parts, in the order they must appear
     */
    public static void assertTextContainsInOrder(String actual, String... parts) {
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
     * Asserts that the whole text matches the expression, like {@link java.util.regex.Matcher#matches()}.
     *
     * @param actual the text
     * @param regex the regular expression
     */
    public static void assertTextMatches(String actual, String regex) {
        if (actual == null || !Pattern.compile(regex).matcher(actual).matches()) {
            fail("expected to match " + regex + " but was:\n" + actual);
        }
    }

    /**
     * Asserts that the whole text does not match the expression.
     *
     * @param actual the text
     * @param regex the regular expression
     */
    public static void assertTextNotMatches(String actual, String regex) {
        if (actual != null && Pattern.compile(regex).matcher(actual).matches()) {
            fail("expected not to match " + regex + " but was:\n" + actual);
        }
    }

    /**
     * Asserts that the expression is found somewhere in the text, like {@link java.util.regex.Matcher#find()}.
     *
     * @param actual the text
     * @param regex the regular expression
     */
    public static void assertTextContainsPattern(String actual, String regex) {
        if (actual == null || !Pattern.compile(regex).matcher(actual).find()) {
            fail("expected to find " + regex + " in:\n" + actual);
        }
    }

    /**
     * Asserts that the expression is found nowhere in the text.
     *
     * @param actual the text
     * @param regex the regular expression
     */
    public static void assertTextNotContainsPattern(String actual, String regex) {
        if (actual != null && Pattern.compile(regex).matcher(actual).find()) {
            fail("expected not to find " + regex + " in:\n" + actual);
        }
    }

    /**
     * Asserts that at least one line contains the text.
     *
     * @param lines the lines
     * @param expected the text one of them must contain
     */
    public static void assertAnyLineContains(List<String> lines, String expected) {
        if (lines.stream().noneMatch(line -> line.contains(expected))) {
            fail("expected a line containing \"" + expected + "\" but was:\n" + String.join("\n", lines));
        }
    }

    /**
     * Asserts that no line contains the text.
     *
     * @param lines the lines
     * @param unexpected the text none of them may contain
     */
    public static void assertNoLineContains(List<String> lines, String unexpected) {
        if (lines.stream().anyMatch(line -> line.contains(unexpected))) {
            fail("expected no line containing \"" + unexpected + "\" but was:\n" + String.join("\n", lines));
        }
    }

    /**
     * Asserts that both collections have the same elements, each as often, in any order.
     *
     * @param expected the expected elements
     * @param actual the actual elements
     */
    public static void assertSameElements(Collection<?> expected, Collection<?> actual) {
        assertEquals(
                counts(expected), counts(actual), () -> "expected the elements " + expected + " but was " + actual);
    }

    private static Map<Object, Integer> counts(Collection<?> elements) {
        Map<Object, Integer> counts = new HashMap<>();
        elements.forEach(e -> counts.merge(e, 1, Integer::sum));
        return counts;
    }
}
