package org.codehaus.mojo.versions.set;

import java.util.regex.Pattern;

import org.codehaus.mojo.versions.utils.RegexUtils;

/**
 * A facility to define which properties in which {@link PropertyScope}s should be changed.
 * <p>
 * Conceptually, the property patterns consist of two parts:
 * <ol>
 * <li>A {@link PropertyScopePattern} and
 * <li>A property name pattern.
 * </ol>
 * <p>
 * {@link PropertyScopePattern} depicts a the scope in which the given property should be changed. There are just two
 * places in the hierarchy of pom.xml elements, where {@code <properties>} can be located: (i) directly under
 * {@code <project>} or (ii) under a {@code <profile>}. This gives three possible kinds of
 * {@link PropertyScopePattern}s:
 * <ol>
 * <li>In-profile to match only properties under a {@code <profile>}
 * <li>Profile-less to match only properties directly under {@code <project>}
 * <li>Match-all to match both above kinds.
 * </ol>
 * <p>
 * The property name pattern is there for selecting which specific property or properties should be changed.
 * <p>
 * Property patterns are defined using a string notation. Such strings can be passed to
 * {@link PropertyPattern#of(String)} to parse them to {@link PropertyPattern} objects.
 * <h2>Property patterns string notation examples:</h2>
 * <ul>
 * <li>{@code myProperty} matches property called {@code myProperty} located directly under {@code <project>} or under
 * any arbitrary {@code <profile>}
 * <li>{@code /myProfileLessProperty} matches property called {@code myProfileLessProperty} located only directly under
 * {@code <project>}. Does not match any property occurrence under any {@code <profile>}.
 * <li>{@code myProfile/myProperty} matches property called {@code myProperty} only if it is located under a
 * {@code <profile>} having {@code <id>myProfile</id>}. Does not match any property occurrence directly under
 * {@code <project>}.
 * <li><code>myProfilePrefix&ast;/myPropertyPrefix*</code> matches all properties starting with {@code myPropertyPrefix}
 * as long as they are located under a {@code <profile>} whose {@code id} starts with {@code myProfilePrefix}.
 * <li><code>&ast;/myPropertyPrefix*</code> matches all properties starting with {@code myPropertyPrefix} as long as
 * they are located under any {@code <profile>}. Does not match any property occurrence directly under
 * {@code <project>}.
 * </ul>
 *
 * @author <a href="https://github.com/ppalaga">Peter Palaga</a>
 */
public class PropertyPattern
{
    /**
     * A selector of property scopes.
     */
    private static abstract class PropertyScopePattern
    {
        private static PropertyScopePattern ANY = new PropertyScopePattern()
        {
            @Override
            public boolean matches( PropertyScope propertyScope )
            {
                return true;
            }
        };

        private static PropertyScopePattern PROFILE_LESS = new PropertyScopePattern()
        {
            @Override
            public boolean matches( PropertyScope propertyScope )
            {
                return propertyScope.isProfileLess();
            }
        };

        /**
         * @return the {@link #ANY} singleton that matches all possible {@link PropertyScope}s
         */
        public static PropertyScopePattern any()
        {
            return ANY;
        }

        public static PropertyScopePattern ofProfile( final Pattern profileNamePattern )
        {
            return new PropertyScopePattern()
            {

                @Override
                public boolean matches( PropertyScope propertyScope )
                {
                    return !propertyScope.isProfileLess()
                        && profileNamePattern.matcher( propertyScope.getProfileId() ).matches();
                }
            };
        }

        /**
         * @return the {@link #PROFILE_LESS} singleton that matches only those {@link PropertyScope}s whose
         *         {@link PropertyScope#isProfileLess()} returns {@code true}
         */
        public static PropertyScopePattern profileLess()
        {
            return PROFILE_LESS;
        }

        /**
         * @param propertyScope a {@link PropertyScope} to matchg against this {@link PropertyScopePattern}
         * @return {@code true} if the given {@code propertyScope} matches this {@link PropertyScopePattern} or
         *         {@code false} otherwise
         */
        public abstract boolean matches( PropertyScope propertyScope );
    }

    /** {@value DELIMITER} constant used to separate the scope from the property name */
    public static final char DELIMITER = '/';

    /**
     * Parses the given {@code rawPropertyPattern} into a {@link PropertyPattern}.
     *
     * @param rawPropertyPattern a string such as {@code "myProperty"}, {@code "/myProfileLessProperty"},
     *            {@code "myProfile/myProperty"}, <code>"myProfilePrefix&ast;/myPropertyPrefix*"</code>, etc.
     * @return a new {@link PropertyPattern}
     */
    public static PropertyPattern of( String rawPropertyPattern )
    {
        if ( rawPropertyPattern == null )
        {
            throw new IllegalArgumentException( "Cannot parse a null rawPropertyPattern" );
        }
        int colonPosition = rawPropertyPattern.indexOf( DELIMITER );
        if ( colonPosition == rawPropertyPattern.length() - 1 )
        {
            throw new IllegalArgumentException( PropertyPattern.class.getName() + " must not end with " + DELIMITER );
        }
        else if ( colonPosition == 0 )
        {
            /* "/myProperty" - the special profile-less property context */
            final Pattern namePattern =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( rawPropertyPattern.substring( colonPosition + 1 ),
                                                                     true ) );
            return new PropertyPattern( rawPropertyPattern, PropertyScopePattern.profileLess(), namePattern );
        }
        else if ( colonPosition > 0 )
        {
            /* "myProfile/myProperty" - non-empty profile name - this is going to be a in-profile matcher */
            final Pattern profilePattern =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( rawPropertyPattern.substring( 0, colonPosition ),
                                                                     true ) );
            final Pattern namePattern =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( rawPropertyPattern.substring( colonPosition + 1 ),
                                                                     true ) );
            return new PropertyPattern( rawPropertyPattern, PropertyScopePattern.ofProfile( profilePattern ),
                                        namePattern );
        }
        else
        {
            /* "myProperty" - there is no collon - we will match all contexts - i.e. both profile-less and in-profile */
            final Pattern namePattern =
                Pattern.compile( RegexUtils.convertWildcardsToRegex( rawPropertyPattern, true ) );
            return new PropertyPattern( rawPropertyPattern, PropertyScopePattern.any(), namePattern );
        }
    }

    /** A regular expression for selecting property names */
    private final Pattern namePattern;

    /** A selector of {@link PropertyScope}s */
    private final PropertyScopePattern propertyScopePattern;

    /** The string representation we parsed from; cannot be {@code null} */
    private final String rawPattern;

    private PropertyPattern( String rawPattern, PropertyScopePattern propertyScopePattern, Pattern namePattern )
    {
        super();
        this.rawPattern = rawPattern;
        this.propertyScopePattern = propertyScopePattern;
        this.namePattern = namePattern;
    }

    @Override
    public boolean equals( Object obj )
    {
        if ( this == obj )
            return true;
        if ( obj == null )
            return false;
        if ( getClass() != obj.getClass() )
            return false;
        return rawPattern.equals( ((PropertyPattern) obj).rawPattern );
    }

    @Override
    public int hashCode()
    {
        return rawPattern.hashCode();
    }

    /**
     * @param propertyScope a property scope in which the property of the given name is located
     * @param propertyName the property name
     * @return {@code true} if the given {@link PropertyScope} matches {@link #propertyScopePattern} and given
     *         {@code propertyName} matches {@link #namePattern}; or {@code false} otherwise
     */
    public boolean matches( PropertyScope propertyScope, String propertyName )
    {
        return propertyScopePattern.matches( propertyScope ) && namePattern.matcher( propertyName ).matches();
    }

    /**
     * @return {@link #rawPattern}
     */
    @Override
    public String toString()
    {
        return rawPattern;
    }

}
