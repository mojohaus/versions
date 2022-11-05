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
