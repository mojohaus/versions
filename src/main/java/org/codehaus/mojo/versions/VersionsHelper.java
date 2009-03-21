package org.codehaus.mojo.versions;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.ordering.ComparableVersion;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Helper class that provides common functionality required by both the mojos and the reports.
 */
public class VersionsHelper
{
    /**
     * Finds the latest version of the specified artifact that matches the version range.
     *
     * @param artifact               The artifact.
     * @param versionRange           The version range.
     * @param artifactMetadataSource
     * @param localRepository
     * @param remoteRepositories
     * @param comparisonMethod
     * @return The latest version of the specified artifact that matches the specified version range or
     *         <code>null</code> if no matching version could be found.
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          If the artifact metadata could not be found.
     * @since 1.0-alpha-1
     */
    public static ArtifactVersion findLatestVersion( Artifact artifact, VersionRange versionRange,
                                                     boolean snapshotsExcluded, Log log,
                                                     ArtifactMetadataSource artifactMetadataSource,
                                                     ArtifactRepository localRepository, List remoteRepositories,
                                                     String comparisonMethod )
        throws MojoExecutionException
    {
        final List versions;
        try
        {
            versions =
                artifactMetadataSource.retrieveAvailableVersions( artifact, localRepository, remoteRepositories );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( "Could not retrieve metadata for " + artifact, e );
        }

        log.debug( artifact.toString() + " has versions " + versions.toString() );

        final Comparator versionComparator = getVersionComparator( comparisonMethod );
        ArtifactVersion artifactVersion = null;
        for ( Iterator j = versions.iterator(); j.hasNext(); )
        {
            ArtifactVersion ver = (ArtifactVersion) j.next();
            if ( snapshotsExcluded && ArtifactUtils.isSnapshot( ver.toString() ) )
            {
                // not this version as it's a snapshot and we've been told no snapshots.
                continue;
            }
            if ( versionRange.containsVersion( ver ) )
            {
                // valid - check if it is greater than the currently matched version
                if ( artifactVersion == null || versionComparator.compare( ver, artifactVersion ) > 0 )
                {
                    artifactVersion = ver;
                }
            }
        }
        if ( artifactVersion == null )
        {
            log.warn( "Could not find any version of " + artifact + " matching " + versionRange );
        }
        return artifactVersion;
    }

    /**
     * Returns the version comparator to use.
     *
     * @param comparisonMethod
     * @return the version comparator to use.
     * @since 1.0-alpha-1
     */
    public static Comparator getVersionComparator( String comparisonMethod )
    {
        if ( "numeric".equalsIgnoreCase( comparisonMethod ) )
        {
            return new NumericVersionComparator();
        }
        else if ( "mercury".equalsIgnoreCase( comparisonMethod ) )
        {
            return new MercuryVersionComparator();
        }
        return new MavenVersionComparator();
    }


    /**
     * A comparator which will compare all segments of a dot separated version string as numbers if possible,
     * i.e. 1.3.34 &gt; 1.3.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.34-SNAPSHOT
     *
     * @since 1.0-alpha-1
     */
    static class NumericVersionComparator
        implements Comparator
    {

        /**
         * {@inheritDoc}
         */
        public int compare( Object o1, Object o2 )
        {
            String v1 = o1.toString();
            String v2 = o2.toString();
            StringTokenizer tok1 = new StringTokenizer( v1, "." );
            StringTokenizer tok2 = new StringTokenizer( v2, "." );
            while ( tok1.hasMoreTokens() && tok2.hasMoreTokens() )
            {
                String p1 = tok1.nextToken();
                String p2 = tok2.nextToken();
                String q1 = null;
                String q2 = null;
                if ( p1.indexOf( '-' ) >= 0 )
                {
                    int index = p1.indexOf( '-' );
                    p1 = p1.substring( 0, index );
                    q1 = p1.substring( index );
                }
                if ( p2.indexOf( '-' ) >= 0 )
                {
                    int index = p2.indexOf( '-' );
                    p2 = p2.substring( 0, index );
                    q2 = p2.substring( index );
                }
                try
                {
                    Integer n1 = Integer.valueOf( p1 );
                    Integer n2 = Integer.valueOf( p2 );
                    int result = n1.compareTo( n2 );
                    if ( result != 0 )
                    {
                        return result;
                    }
                }
                catch ( NumberFormatException e )
                {
                    int result = p1.compareTo( p2 );
                    if ( result != 0 )
                    {
                        return result;
                    }
                }
                if ( q1 != null && q2 != null )
                {
                    return q1.compareTo( q2 );
                }
                if ( q1 != null )
                {
                    return -1;
                }
                if ( q2 != null )
                {
                    return +1;
                }
            }
            if ( tok1.hasMoreTokens() )
            {
                Integer n2 = new Integer( 0 );
                while ( tok1.hasMoreTokens() )
                {
                    try
                    {
                        Integer n1 = Integer.valueOf( tok1.nextToken() );
                        int result = n1.compareTo( n2 );
                        if ( result != 0 )
                        {
                            return result;
                        }
                    }
                    catch ( NumberFormatException e )
                    {
                        // ignore
                    }
                }
                return -1;
            }
            if ( tok2.hasMoreTokens() )
            {
                Integer n1 = new Integer( 0 );
                while ( tok2.hasMoreTokens() )
                {
                    try
                    {
                        Integer n2 = Integer.valueOf( tok2.nextToken() );
                        int result = n1.compareTo( n2 );
                        if ( result != 0 )
                        {
                            return result;
                        }
                    }
                    catch ( NumberFormatException e )
                    {
                        // ignore
                    }
                }
                return +1;
            }
            return 0;
        }

    }

    /**
     * A comparator which uses Maven's version rules, i.e. 1.3.34 &gt; 1.3.9 but 1.3.4.3.2.34 &lt; 1.3.4.3.2.9.
     *
     * @since 1.0-alpha-1
     */
    private static class MavenVersionComparator
        implements Comparator
    {

        /**
         * {@inheritDoc}
         */
        public int compare( Object o1, Object o2 )
        {
            return ( (ArtifactVersion) o1 ).compareTo( (ArtifactVersion) o2 );
        }

    }

    /**
     * A comparator which uses Mercury's version rules.
     *
     * @since 1.0-alpha-3
     */
    private static class MercuryVersionComparator
        implements Comparator
    {

        /**
         * {@inheritDoc}
         */
        public int compare( Object o1, Object o2 )
        {
            return new ComparableVersion( o1.toString() ).compareTo( new ComparableVersion( o2.toString() ) );
        }

    }

}
