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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 * @deprecated
 * Note: This comparator should be replaced by using the one of Maven core instead.
 */
public final class VersionComparators
{
    private static final Pattern SNAPSHOT_PATTERN = Pattern.compile( "(-((\\d{8}\\.\\d{6})-(\\d+))|(SNAPSHOT))$" );

    private VersionComparators()
    {
        throw new IllegalAccessError( "Utility classes should never be instantiated" );
    }

    /**
     * Returns the version comparator to use.
     *
     * @param comparisonMethod the comparison method.
     * @return the version comparator to use.
     * @since 1.0-alpha-1
     */
    public static VersionComparator getVersionComparator( String comparisonMethod )
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

    public static String alphaNumIncrement( String token )
    {
        String newToken;
        int i = token.length();
        boolean done = false;
        newToken = token;
        while ( !done && i > 0 )
        {
            i--;
            char c = token.charAt( i );
            if ( '0' <= c && c < '9' )
            {
                c++;
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
                done = true;
            }
            else if ( c == '9' )
            {
                c = '0';
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
            }
            else if ( 'A' <= c && c < 'Z' )
            {
                c++;
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
                done = true;
            }
            else if ( c == 'Z' )
            {
                c = 'A';
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
            }
            else if ( 'a' <= c && c < 'z' )
            {
                c++;
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
                done = true;
            }
            else if ( c == 'z' )
            {
                c = 'a';
                newToken =
                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring( i + 1 ) : "" );
            }
        }
        if ( done )
        {
            return newToken;
        }
        else
        {
            // ok this is roll-over time
            boolean lastNumeric = false;
            boolean lastAlpha = false;
            boolean lastUpper = false;
            i = token.length();
            while ( !lastAlpha && !lastNumeric && i > 0 )
            {
                i--;
                char c = token.charAt( i );
                lastAlpha = Character.isLetter( c );
                lastUpper = c == Character.toUpperCase( c );
                lastNumeric = Character.isDigit( c );
            }
            if ( lastAlpha )
            {
                if ( lastUpper )
                {
                    return token + 'A';
                }
                return token + 'a';
            }
            return token + '0';

        }
    }

    static boolean isSnapshot( ArtifactVersion v )
    {
        return v != null && SNAPSHOT_PATTERN.matcher( v.toString() ).find();
    }

    static ArtifactVersion stripSnapshot( ArtifactVersion v )
    {
        final String version = v.toString();
        final Matcher matcher = SNAPSHOT_PATTERN.matcher( version );
        if ( matcher.find() )
        {
            return new DefaultArtifactVersion( version.substring( 0, matcher.start( 1 ) - 1 ) );
        }
        return v;
    }

    static ArtifactVersion copySnapshot( ArtifactVersion source, ArtifactVersion destination )
    {
        if ( isSnapshot( destination ) )
        {
            destination = stripSnapshot( destination );
        }
        Pattern matchSnapshotRegex = SNAPSHOT_PATTERN;
        final Matcher matcher = matchSnapshotRegex.matcher( source.toString() );
        if ( matcher.find() )
        {
            return new DefaultArtifactVersion( destination.toString() + "-" + matcher.group( 0 ) );
        }
        else
        {
            return new DefaultArtifactVersion( destination.toString() + "-SNAPSHOT" );
        }
    }
}
