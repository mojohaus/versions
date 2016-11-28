package org.codehaus.mojo.versions.set;

/**
 * A location of a property in the hierarchy of pom.xml elements. There are two kinds of property scopes:
 * <ol>
 * <li>Profile-less - the one at the project level, out of any profile - see {@link #profileLess()}.
 * <li>In-profile - under a profile - see {@link #ofProfile(String)}
 * </ol>
 *
 * @author <a href="https://github.com/ppalaga">Peter Palaga</a>
 */
public class PropertyScope
{
    /** A singleton that has a {@code null} {@link #profileId} set */
    private static final PropertyScope PROFILE_LESS = new PropertyScope( null );

    /**
     * @param profileId the id of the profile the returned scope is defined for
     * @return a new {@link PropertyScope} ranging over a profile with the given {@code profileId}
     */
    public static PropertyScope ofProfile( String profileId )
    {
        return new PropertyScope( profileId );
    }

    /**
     * @return the {@link #PROFILE_LESS} singleton that has a {@code null} {@link #profileId} set
     */
    public static PropertyScope profileLess()
    {
        return PROFILE_LESS;
    }

    /** The profile id this {@link PropertyScope} ranges over, possibly {@code null} */
    private final String profileId;

    private PropertyScope( String profileId )
    {
        super();
        this.profileId = profileId;
    }

    /**
     * {@link #isProfileLess()} should always be called before {@link #getProfileId()} to avoid
     * {@link IllegalStateException}.
     *
     * @return {@link #profileId} - the profile id this {@link PropertyScope} ranges over
     * @throws IllegalStateException if {@link #profileId} is {@code null}
     */
    public String getProfileId()
    {
        if ( profileId == null )
        {
            throw new IllegalStateException( "You should call " + PropertyScope.class.getName()
                + ".getProfileId() only if isProfileLess() returns true." );
        }
        return profileId;
    }

    /**
     * @return {@code true} if this {@link PropertyScope}'s {@link #profileId} is {@code null} - i.e. if this
     *         {@link PropertyScope} does not range over any profile
     */
    public boolean isProfileLess()
    {
        return profileId == null;
    }

}