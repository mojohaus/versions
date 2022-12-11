package org.codehaus.mojo.versions.filtering;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WildcardMatcherTest {
    @Nested
    class ExactValueTest {
        private final WildcardMatcher matcher = WildcardMatcher.parse("asdf");

        @Test
        void matchesTheExactInput() {
            boolean actual = matcher.test("asdf");

            assertTrue(actual);
        }

        @Test
        void rejectsNull() {
            boolean actual = matcher.test(null);

            assertFalse(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "", "    ", "a", "as", "asf", "asXf", "asdfx",
                })
        void rejectsDifferentInput(String input) {
            boolean actual = matcher.test(input);

            assertFalse(actual);
        }
    }

    @Nested
    class WildcardOnlyPattern {
        private final WildcardMatcher matcher = WildcardMatcher.parse("*");

        @Test
        void acceptsNull() {
            boolean actual = matcher.test(null);

            assertTrue(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "", "    ", "a", "asdfx",
                })
        void accepts(String input) {
            boolean actual = matcher.test(input);

            assertTrue(actual);
        }
    }

    @Nested
    class PatternWithWildcardAtStartAndEnd {
        private final WildcardMatcher matcher = WildcardMatcher.parse("*asdf*");

        @Test
        void rejectsNull() {
            boolean actual = matcher.test(null);

            assertFalse(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "asdf",
                    "fooasdf",
                    "asdfbar",
                    "fooasdfbar",
                    " asdf",
                    "asdf ",
                    " asdf ",
                })
        void accepts(String input) {
            boolean actual = matcher.test(input);

            assertTrue(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "", "    ", "foo",
                })
        void rejects(String input) {
            boolean actual = matcher.test(input);

            assertFalse(actual);
        }
    }

    @Nested
    class PatternStartingWithWildcard {
        private final WildcardMatcher matcher = WildcardMatcher.parse("*asdf");

        @Test
        void rejectsNull() {
            boolean actual = matcher.test(null);

            assertFalse(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "asdf",
                    "asdfasdf",
                    "    asdf",
                    "Xasdf",
                    "99999999asdf",
                })
        void accepts(String input) {
            boolean actual = matcher.test(input);

            assertTrue(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "",
                    "    ",
                    "asdf ",
                    "asdfx",
                    "asdfbanana",
                })
        void rejects(String input) {
            boolean actual = matcher.test(input);

            assertFalse(actual);
        }
    }

    @Nested
    class PatternEndingWithWildcard {
        private final WildcardMatcher matcher = WildcardMatcher.parse("asdf*");

        @Test
        void rejectsNull() {
            boolean actual = matcher.test(null);

            assertFalse(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "asdf",
                    "asdfasdf",
                    "asdf ",
                    "asdfx",
                    "asdfbanana",
                })
        void accepts(String input) {
            boolean actual = matcher.test(input);

            assertTrue(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "",
                    "    ",
                    "    asdf",
                    "Xasdf",
                    "99999999asdf",
                })
        void rejects(String input) {
            boolean actual = matcher.test(input);

            assertFalse(actual);
        }
    }

    @Nested
    class PatternWithVersionRange {
        private final WildcardMatcher matcher = WildcardMatcher.parse("[2.0,3.0]");

        @Test
        void rejectsNull() {
            boolean actual = matcher.test(null);

            assertFalse(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "2.0", "2.1", "3.0",
                })
        void accepts(String input) {
            boolean actual = matcher.test(input);

            assertTrue(actual);
        }

        @ParameterizedTest
        @ValueSource(
                strings = {
                    "",
                    "    ",
                    "1.0",
                    "2.0-SNAPSHOT",
                    "4.0",
                })
        void rejects(String input) {
            boolean actual = matcher.test(input);

            assertFalse(actual);
        }
    }
}
