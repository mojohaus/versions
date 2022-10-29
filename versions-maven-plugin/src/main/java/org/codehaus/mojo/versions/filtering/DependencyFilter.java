package org.codehaus.mojo.versions.filtering;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.utils.DependencyComparator;

public class DependencyFilter
{

    private final String pattern;
    private final List<TokenizedMatcher> matchers;

    DependencyFilter( String pattern, List<TokenizedMatcher> matchers )
    {
        this.pattern = pattern;
        this.matchers = matchers;
    }

    public static DependencyFilter parseFrom( List<String> dependencies )
    {
        List<TokenizedMatcher> matchers = dependencies.stream()
                .map( TokenizedMatcher::parse )
                .collect( Collectors.toList() );

        String debugPattern = String.join( ",", dependencies );

        return new DependencyFilter( debugPattern, matchers );
    }

    private static <T> Predicate<T> not( Predicate<T> predicate )
    {
        return x -> !predicate.test( x );
    }

    @Override
    public String toString()
    {
        return String.format( "%s{%s}", getClass().getSimpleName(), pattern );
    }

    public Set<Dependency> retainingIn( Set<Dependency> dependencies )
    {
        return filterBy( dependencies, this::matchersMatch );
    }

    public Set<Dependency> removingFrom( Set<Dependency> dependencies )
    {
        return filterBy( dependencies, not( this::matchersMatch ) );
    }

    private boolean matchersMatch( Dependency dependency )
    {
        return matchers.stream().anyMatch( m -> m.test( dependency ) );
    }

    private TreeSet<Dependency> filterBy( Set<Dependency> dependencies, Predicate<Dependency> predicate )
    {
        return dependencies.stream()
                .filter( predicate )
                .collect( Collectors.toCollection( () -> new TreeSet<>( DependencyComparator.INSTANCE ) ) );
    }
}
