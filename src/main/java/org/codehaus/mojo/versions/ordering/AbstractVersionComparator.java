package org.codehaus.mojo.versions.ordering;

import org.apache.maven.artifact.versioning.ArtifactVersion;

/**
 * Base class for version comparators.
 *
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionComparator
    implements VersionComparator
{
    /**
     * {@inheritDoc}
     */
    public abstract int compare( Object o1, Object o2 );

    /**
     * {@inheritDoc}
     */
    public final int getSegmentCount( ArtifactVersion v )
    {
        if ( v == null )
        {
            return 0;
        }
        if ( VersionComparators.isSnapshot( v ) )
        {
            return innerGetSegmentCount( VersionComparators.stripSnapshot( v ) );
        }
        return innerGetSegmentCount( v );

    }

    protected abstract int innerGetSegmentCount( ArtifactVersion v );

    /**
     * {@inheritDoc}
     */
    public final ArtifactVersion incrementSegment( ArtifactVersion v, int segment )
    {
        if ( VersionComparators.isSnapshot( v ) )
        {
            return VersionComparators.copySnapshot( v, innerIncrementSegment( VersionComparators.stripSnapshot( v ),
                                                                              segment ) );
        }
        return innerIncrementSegment( v, segment );
    }

    protected abstract ArtifactVersion innerIncrementSegment( ArtifactVersion v, int segment );

    /**
     * Returns a hash code value for the comparator class.
     *
     * @return the hash code.
     */
    public int hashCode()
    {
        return getClass().hashCode();
    }

    /**
     * Returns true if this object is the same type of comparator as the parameter.
     *
     * @param obj the reference object with which to compare.
     * @return <code>true</code> if this object is the same as the obj
     *         argument; <code>false</code> otherwise.
     * @see #hashCode()
     * @see java.util.Hashtable
     */
    public boolean equals( Object obj )
    {
        return obj == this || ( obj != null && getClass().equals( obj.getClass() ) );
    }

}
