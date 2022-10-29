package org.codehaus.mojo.versions.filtering;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import org.apache.maven.model.Dependency;

public class TokenizedMatcher implements Predicate<Dependency>
{
    public enum Tokens
    {
        GROUP_ID( Dependency::getGroupId ),
        ARTIFACT_ID( Dependency::getArtifactId ),
        VERSION( Dependency::getVersion ),
        TYPE( Dependency::getType ),
        CLASSIFIER( Dependency::getClassifier ),
        SCOPE( Dependency::getScope );

        private final Function<Dependency, String> tokenExtractor;

        Tokens( Function<Dependency, String> tokenExtractor )
        {
            this.tokenExtractor = tokenExtractor;
        }

        public Function<Dependency, String> getTokenExtractor()
        {
            return tokenExtractor;
        }
    }

    private final Map<Tokens, Predicate<String>> matchers;

    private TokenizedMatcher( Map<Tokens, Predicate<String>> matchers )
    {
        this.matchers = matchers;
    }

    @Override
    public boolean test( Dependency dependency )
    {
        for ( Tokens token : Tokens.values() )
        {
            String tokenValue = token.getTokenExtractor().apply( dependency );

            Predicate<String> matcher = matchers.get( token );
            boolean matches = matcher.test( tokenValue );

            if ( !matches )
            {
                return false;
            }
        }

        return true;
    }

    public static TokenizedMatcher parse( String pattern )
    {
        EnumMap<Tokens, Predicate<String>> matchers = new EnumMap<>( Tokens.class );

        String[] split = pattern == null
                ? new String[0]
                : pattern.split( ":" );

        String groupIdPattern = split.length >= 1
                ? split[0]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.GROUP_ID, WildcardMatcher.parse( groupIdPattern ) );

        String artifactIdPattern = split.length >= 2
                ? split[1]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.ARTIFACT_ID, WildcardMatcher.parse( artifactIdPattern ) );

        String versionPattern = split.length >= 3
                ? split[2]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.VERSION, WildcardMatcher.parse( versionPattern ) );

        String typePattern = split.length >= 4
                ? split[3]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.TYPE, WildcardMatcher.parse( typePattern ) );

        String classifierPattern = split.length >= 5
                ? split[4]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.CLASSIFIER, new NullAwareWildcardMatcher( classifierPattern ) );

        String scopePattern = split.length >= 6
                ? split[5]
                : WildcardMatcher.WILDCARD;
        matchers.put( Tokens.SCOPE, new NullAwareWildcardMatcher( scopePattern ) );

        return new TokenizedMatcher( Collections.unmodifiableMap( matchers ) );
    }

    @Override
    public String toString()
    {
        return String.format( "%s{%s}", getClass().getSimpleName(), matchers );
    }
}
