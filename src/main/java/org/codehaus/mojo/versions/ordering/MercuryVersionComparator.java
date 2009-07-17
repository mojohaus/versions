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

import java.math.BigInteger;
import java.util.StringTokenizer;

/**
 * A comparator which uses Mercury's version rules.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class MercuryVersionComparator
    implements VersionComparator
{
    private static final BigInteger BIG_INTEGER_ONE = new BigInteger( "1" );

    /**
     * {@inheritDoc}
     */
    public int compare( Object o1, Object o2 )
    {
        return new ComparableVersion( o1.toString() ).compareTo( new ComparableVersion( o2.toString() ) );
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
        final String version = v.toString();
        StringTokenizer tok = new StringTokenizer( version, ".-" );
        return tok.countTokens();
    }

    public ArtifactVersion incrementSegment( ArtifactVersion v, int segment )
    {
        if ( segment < 0 || segment > getSegmentCount( v ) )
        {
            throw new IllegalArgumentException( "Invalid segment" );
        }
        final String version = v.toString();
        StringBuffer result = new StringBuffer( version.length() + 10 );
        StringTokenizer tok = new StringTokenizer( version, ".-" );
        int index = 0;
        while ( tok.hasMoreTokens() && segment > 0 )
        {
            String token = tok.nextToken();
            result.append( token );
            index += token.length();
            if ( tok.hasMoreTokens() )
            {
                // grab the token separator
                result.append( version.substring( index, index + 1 ) );
                index++;
            }
            segment--;
        }
        if ( segment == 0 )
        {
            if ( tok.hasMoreTokens() )
            {
                String token = tok.nextToken();
                String newToken;
                try
                {
                    BigInteger n = new BigInteger( token );
                    newToken = n.add( BIG_INTEGER_ONE ).toString();
                }
                catch ( NumberFormatException e )
                {
                    // ok, let's try some common tricks
                    if ( "alpha".equalsIgnoreCase( token ) )
                    {
                        newToken = "beta";
                    }
                    else if ( "beta".equalsIgnoreCase( token ) )
                    {
                        newToken = "milestone";
                    }
                    else if ( "milestone".equalsIgnoreCase( token ) )
                    {
                        newToken = "rc";
                    }
                    else if ( "rc".equalsIgnoreCase( token ) || "cr".equalsIgnoreCase( token ) )
                    {
                        newToken = "ga";
                    }
                    else if ( "final".equalsIgnoreCase( token ) || "ga".equalsIgnoreCase( token )
                        || "".equalsIgnoreCase( token ) )
                    {
                        newToken = "sp";
                    }
                    else
                    {
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
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                                done = true;
                            }
                            else if ( c == '9' )
                            {
                                c++;
                                newToken =
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                            }
                            else if ( 'A' <= c && c < 'Z' )
                            {
                                c++;
                                newToken =
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                                done = true;
                            }
                            else if ( c == 'Z' )
                            {
                                c++;
                                newToken =
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                            }
                            else if ( 'a' <= c && c < 'z' )
                            {
                                c++;
                                newToken =
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                                done = true;
                            }
                            else if ( c == 'z' )
                            {
                                c++;
                                newToken =
                                    newToken.substring( 0, i ) + c + ( i + 1 < newToken.length() ? newToken.substring(
                                        i + 1 ) : "" );
                            }
                        }
                    }
                }

                result.append( newToken );
                index += token.length();
                if ( tok.hasMoreTokens() )
                {
                    // grab the token separator
                    result.append( version.substring( index, index + 1 ) );
                    index++;
                }

            }
            else
            {
                // an empty part is equivalent to 0 for mercury version comparator
                result.append( "1" );
            }
        }
        while ( tok.hasMoreTokens() )
        {
            String token = tok.nextToken();
            result.append( "0" );
            index += token.length();
            if ( tok.hasMoreTokens() )
            {
                // grab the token separator
                result.append( version.substring( index, index + 1 ) );
                index++;
            }
        }
        return new DefaultArtifactVersion( result.toString() );
    }
}
