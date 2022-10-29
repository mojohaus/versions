package org.codehaus.mojo.versions.filtering;

public class NullAwareWildcardMatcher extends WildcardMatcher
{
    public static final String NULL_KEYWORD = "null";

    public NullAwareWildcardMatcher( String pattern )
    {
        super( pattern );
    }

    @Override
    public boolean test( String token )
    {
        if ( NULL_KEYWORD.equals( getPattern() ) )
        {
            return token == null;
        }

        return super.test( token );
    }
}
