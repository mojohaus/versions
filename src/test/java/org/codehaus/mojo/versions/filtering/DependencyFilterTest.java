package org.codehaus.mojo.versions.filtering;

import java.util.HashSet;
import java.util.Set;

import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.codehaus.mojo.versions.HasGAVMatcher.hasGAV;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;

class DependencyFilterTest
{

    @Nested
    class RemoveFromTest
    {
        private final Set<Dependency> input = new HashSet<>( asList(
            DependencyBuilder.dependencyWith( "foo", "bar", "1" ),
            DependencyBuilder.dependencyWith( "localhost", "my-api", "2" ),
            DependencyBuilder.dependencyWith( "localhost", "my-impl", "3" )
        ) );

        @Test
        void removesExcludedDepsWithExactMatch()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:my-impl:3" ) );

            Set<Dependency> actual = exclusions.removingFrom( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "foo", "bar", "1" ),
                    hasGAV( "localhost", "my-api", "2" )
                )
            );
        }

        @Test
        void removesExcludedDepsWithWildcardInVersion()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:my-impl:*" ) );

            Set<Dependency> actual = exclusions.removingFrom( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "foo", "bar", "1" ),
                    hasGAV( "localhost", "my-api", "2" )
                )
            );
        }

        @Test
        void removesExcludedDepsWithWildcardInGroupId()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:*:*" ) );

            Set<Dependency> actual = exclusions.removingFrom( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "foo", "bar", "1" )
                )
            );
        }

        @Test
        void removesExcludedDepsWithAllWildcards()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "*:*:*" ) );

            Set<Dependency> actual = exclusions.removingFrom( input );

            assertThat(
                actual,
                empty()
            );
        }

        @Test
        void removesMultiplePatterns()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList(
                "*:my-api",
                "*:my-impl"
            ) );

            Set<Dependency> actual = exclusions.removingFrom( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "foo", "bar", "1" )
                )
            );
        }

    }

    @Nested
    class RetainingInTest
    {
        private final Set<Dependency> input = new HashSet<>( asList(
            DependencyBuilder.dependencyWith( "foo", "bar", "1" ),
            DependencyBuilder.dependencyWith( "localhost", "my-api", "2" ),
            DependencyBuilder.dependencyWith( "localhost", "my-impl", "3" )
        ) );

        @Test
        void retainsOnlyDepsWithExactMatch()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:my-impl:3" ) );

            Set<Dependency> actual = exclusions.retainingIn( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "localhost", "my-impl", "3" )
                )
            );
        }

        @Test
        void retainsOnlyDepsMatchingWildcardInVersion()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:my-api:*" ) );

            Set<Dependency> actual = exclusions.retainingIn( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "localhost", "my-api", "2" )
                )
            );
        }

        @Test
        void retainsOnlyDepsWithMultipleWildcards()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "localhost:my-*:*" ) );

            Set<Dependency> actual = exclusions.retainingIn( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "localhost", "my-api", "2" ),
                    hasGAV( "localhost", "my-impl", "3" )
                )
            );
        }

        @Test
        void retainsAllOnAllWildcards()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList( "*:*:*" ) );

            Set<Dependency> actual = exclusions.retainingIn( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "foo", "bar", "1" ),
                    hasGAV( "localhost", "my-api", "2" ),
                    hasGAV( "localhost", "my-impl", "3" )
                )
            );
        }


        @Test
        void retainsMultiplePatterns()
        {
            DependencyFilter exclusions = DependencyFilter.parseFrom( asList(
                "*:my-api",
                "*:my-impl"
            ) );

            Set<Dependency> actual = exclusions.retainingIn( input );

            assertThat(
                actual,
                containsInAnyOrder(
                    hasGAV( "localhost", "my-api", "2" ),
                    hasGAV( "localhost", "my-impl", "3" )
                )
            );
        }
    }

}
