package org.codehaus.mojo.versions.ordering;

import java.util.Iterator;
import java.util.List;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.codehaus.mojo.versions.api.Segment;

import static org.codehaus.mojo.versions.ordering.ComparableVersion.IntegerItem.ZERO;

/**
 * <p>Represents an artifact version with all segments more major or equal to a given segment
 * held in place. It can be thought of as an artifact having +&infin; as its upper bound
 * on all segments less major than the held segment.</p>
 * <p>When compared with another artifact versions, this results with the other object
 * with the segment versions up to the held segment being equal,
 * always comparing lower than this object.</p>
 * <p>This is particularly helpful for -SNAPSHOT and other versions with qualifiers, which
 * are lower than version 0 in the Maven versioning system.</p>
 */
public class BoundArtifactVersion extends DefaultArtifactVersion
{
    /**
     * Most major segment that can change, i.e. not held in place.
     * All segments that are more major than this one are held in place.
     */
    private final Segment segment;

    private final BoundComparableVersion comparator;

    /**
     * Constructs the instance
     * @param artifactVersion artifact version containing the segment version values
     * @param segment most major segment that can change, i.e. <em>not</em> held in place
     */
    public BoundArtifactVersion( ArtifactVersion artifactVersion, Segment segment )
    {
        super( artifactVersion.toString() );
        this.segment = segment;
        this.comparator = new BoundComparableVersion( this );
    }

    /**
     * Returns the most major segment that can change.
     * All segments that are more major than this one are held in place.
     * @return segment that can change
     */
    public Segment getSegment()
    {
        return segment;
    }

    @Override
    public int compareTo( ArtifactVersion other )
    {
        if ( other == null )
        {
            return -1;
        }

        return comparator.compareTo( new ComparableVersion( other.toString() ) );
    }

    protected static class BoundComparableVersion extends ComparableVersion
    {
        private BoundArtifactVersion artifactVersion;
        protected BoundComparableVersion( BoundArtifactVersion artifactVersion )
        {
            super( artifactVersion.toString() );
            this.artifactVersion = artifactVersion;
        }

        @Override
        public int compareTo( ComparableVersion o )
        {
            return compareTo( ( (List<Item>) items ).iterator(),
                    ( (Iterable<Item>) o.items ).iterator(), artifactVersion.segment.value() );

        }

        @SuppressWarnings( "checkstyle:InnerAssignment" )
        private int compareTo( Iterator<Item> left, Iterator<Item> right, int comparisonLimit )
        {
            int r;
            return !( comparisonLimit > 0 && right.hasNext() )
                    ? 1
                    : ( r = -( right.next().compareTo( left.hasNext() ? left.next() : ZERO ) ) ) != 0
                            ? r
                            : compareTo( left, right, comparisonLimit - 1 );
        }
    }
}
