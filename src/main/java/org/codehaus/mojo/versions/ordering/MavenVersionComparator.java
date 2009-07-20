package org.codehaus.mojo.versions.ordering;

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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 * A comparator which uses Maven's version rules, i.e. 1.3.34 &gt; 1.3.9 but 1.3.4.3.2.34 &lt; 1.3.4.3.2.9.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class MavenVersionComparator
    implements VersionComparator
{

    /**
     * {@inheritDoc}
     */
    public int compare( Object o1, Object o2 )
    {
        return ( (ArtifactVersion) o1 ).compareTo( (ArtifactVersion) o2 );
    }

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

    public int getSegmentCount( ArtifactVersion v )
    {
        if ( VersionComparators.isSnapshot( v ) )
        {
            return innerGetSegmentCount( VersionComparators.stripSnapshot( v ) );
        }
        return innerGetSegmentCount( v );

    }

    private int innerGetSegmentCount( ArtifactVersion v )
    {
        // if the version does not match the maven rules, then we have only one segment
        // i.e. the qualifier
        final String version = v.toString();
        return ( v.getMajorVersion() != 0 || v.getMinorVersion() != 0 || v.getIncrementalVersion() != 0
            || v.getBuildNumber() != 0
            || ( ( version.indexOf( '.' ) != -1 || version.indexOf( '-' ) != -1 ) && !version.equals(
            v.getQualifier() ) ) || version.length() == 0 ) ? 4 : 1;
    }

    public ArtifactVersion incrementSegment( ArtifactVersion v, int segment )
    {
        if ( VersionComparators.isSnapshot( v ) )
        {
            return VersionComparators.copySnapshot( v, innerIncrementSegment( VersionComparators.stripSnapshot( v ),
                                                                              segment ) );
        }
        return innerIncrementSegment( v, segment );
    }

    private ArtifactVersion innerIncrementSegment( ArtifactVersion v, int segment )
    {
        int segmentCount = innerGetSegmentCount( v );
        if ( segment < 0 || segment >= segmentCount )
        {
            throw new IllegalArgumentException( "Invalid segment" );
        }
        String version = v.toString();
        if ( segmentCount == 1 )
        {
            // only the qualifier
            version = VersionComparators.alphaNumIncrement( version );
            return new DefaultArtifactVersion( version );
        }
        else
        {
            int major = v.getMajorVersion();
            int minor = v.getMinorVersion();
            int incremental = v.getIncrementalVersion();
            int build = v.getBuildNumber();
            String qualifier = v.getQualifier();

            int minorIndex = version.indexOf( '.' );
            boolean haveMinor = minorIndex != -1;
            int incrementalIndex = haveMinor ? version.indexOf( '.', minorIndex + 1 ) : -1;
            boolean haveIncremental = incrementalIndex != -1;
            int buildIndex = version.indexOf( '-' );
            boolean haveBuild = buildIndex != -1 && qualifier == null;
            boolean haveQualifier = buildIndex != -1 && qualifier != null;

            switch ( segment )
            {
                case 0:
                    major++;
                    minor = 0;
                    incremental = 0;
                    build = 0;
                    if ( haveQualifier && qualifier.endsWith( "SNAPSHOT" ) )
                    {
                        qualifier = "SNAPSHOT";
                    }
                    break;
                case 1:
                    minor++;
                    incremental = 0;
                    build = 0;
                    if ( haveQualifier && qualifier.endsWith( "SNAPSHOT" ) )
                    {
                        qualifier = "SNAPSHOT";
                    }
                    break;
                case 2:
                    incremental++;
                    build = 0;
                    if ( haveQualifier && qualifier.endsWith( "SNAPSHOT" ) )
                    {
                        qualifier = "SNAPSHOT";
                    }
                    break;
                case 3:
                    if ( haveQualifier )
                    {
                        if ( "SNAPSHOT".equals( qualifier ) )
                        {
                            // next thing after a snapshot is a release
                            qualifier = null;
                            build = 1;
                        }
                        else if ( qualifier.endsWith( "-SNAPSHOT" ) )
                        {
                            // this is likely something like alpha-1-SNAPSHOT
                            qualifier =
                                qualifierIncrement( qualifier.substring( 0, qualifier.lastIndexOf( "-SNAPSHOT" ) ) )
                                    + "-SNAPSHOT";
                        }
                        else
                        {
                            qualifier = qualifierIncrement( qualifier );
                        }
                    }
                    else
                    {
                        build++;
                    }
                    break;
            }
            StringBuffer result = new StringBuffer();
            result.append( major );
            if ( haveMinor || minor > 0 || incremental > 0 )
            {
                result.append( '.' );
                result.append( minor );
            }
            if ( haveIncremental || incremental > 0 )
            {
                result.append( '.' );
                result.append( incremental );
            }
            if ( haveQualifier && qualifier != null )
            {
                result.append( '-' );
                result.append( qualifier );
            }
            else if ( haveBuild || build > 0 )
            {
                result.append( '-' );
                result.append( build );
            }
            return new DefaultArtifactVersion( result.toString() );
        }
    }

    private String qualifierIncrement( String qualifier )
    {
        if ( qualifier.toLowerCase().startsWith( "alpha" ) )
        {
            return qualifier.substring( 0, 5 ) + VersionComparators.alphaNumIncrement( qualifier.substring( 5 ) );
        }
        if ( qualifier.toLowerCase().startsWith( "beta" ) )
        {
            return qualifier.substring( 0, 4 ) + VersionComparators.alphaNumIncrement( qualifier.substring( 4 ) );
        }
        if ( qualifier.toLowerCase().startsWith( "milestone" ) )
        {
            return qualifier.substring( 0, 8 ) + VersionComparators.alphaNumIncrement( qualifier.substring( 8 ) );
        }
        if ( qualifier.toLowerCase().startsWith( "cr" ) || qualifier.toLowerCase().startsWith( "rc" )
            || qualifier.toLowerCase().startsWith( "sp" ) )
        {
            return qualifier.substring( 0, 2 ) + VersionComparators.alphaNumIncrement( qualifier.substring( 2 ) );
        }
        return VersionComparators.alphaNumIncrement( qualifier );
    }
}
