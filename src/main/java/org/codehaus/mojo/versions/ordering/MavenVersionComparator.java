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
import org.codehaus.plexus.util.StringUtils;

/**
 * A comparator which uses Maven's version rules, i.e. 1.3.34 &gt; 1.3.9 but 1.3.4.3.2.34 &lt; 1.3.4.3.2.9.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class MavenVersionComparator
    extends AbstractVersionComparator
{

    /**
     * {@inheritDoc}
     */
    public int compare( ArtifactVersion o1, ArtifactVersion o2 )
    {
        return o1.compareTo( o2 );
    }

    /**
     * {@inheritDoc}
     */
    protected int innerGetSegmentCount( ArtifactVersion v )
    {
        // if the version does not match the maven rules, then we have only one segment
        // i.e. the qualifier
        if ( v.getBuildNumber() != 0 )
        {
            // the version was successfully parsed, and we have a build number
            // have to have four segments
            return 4;
        }
        if ( ( v.getMajorVersion() != 0 || v.getMinorVersion() != 0 || v.getIncrementalVersion() != 0 )
            && v.getQualifier() != null )
        {
            // the version was successfully parsed, and we have a qualifier
            // have to have four segments
            return 4;
        }
        final String version = v.toString();
        if ( version.indexOf( '-' ) != -1 )
        {
            // the version has parts and was not parsed successfully
            // have to have one segment
            return version.equals( v.getQualifier() ) ? 1 : 4;
        }
        if ( version.indexOf( '.' ) != -1 )
        {
            // the version has parts and was not parsed successfully
            // have to have one segment
            return version.equals( v.getQualifier() ) ? 1 : 3;
        }
        if ( StringUtils.isEmpty( version ) )
        {
            return 3;
        }
        try
        {
            Integer.parseInt( version );
            return 3;
        }
        catch ( NumberFormatException e )
        {
            return 1;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected ArtifactVersion innerIncrementSegment( ArtifactVersion v, int segment )
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
                    qualifier = null;
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
                    qualifier = null;
                    break;
                case 3:
                    if ( haveQualifier )
                    {
                        qualifier = qualifierIncrement( qualifier );
                    }
                    else
                    {
                        build++;
                    }
                    break;
            }
            StringBuilder result = new StringBuilder();
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
        if ( qualifier.toLowerCase().startsWith( "cr" ) || qualifier.toLowerCase().startsWith( "rc" ) ||
            qualifier.toLowerCase().startsWith( "sp" ) )
        {
            return qualifier.substring( 0, 2 ) + VersionComparators.alphaNumIncrement( qualifier.substring( 2 ) );
        }
        return VersionComparators.alphaNumIncrement( qualifier );
    }
}
