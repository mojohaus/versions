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
 * A comparator which will compare all segments of a dot separated version string as numbers if possible,
 * i.e. 1.3.34 &gt; 1.3.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.34-SNAPSHOT
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class NumericVersionComparator
    implements VersionComparator
{
    private static final BigInteger BIG_INTEGER_ZERO = new BigInteger( "0" );

    private static final BigInteger BIG_INTEGER_ONE = new BigInteger( "1" );

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
                q1 = p1.substring( index );
                p1 = p1.substring( 0, index );
            }
            if ( p2.indexOf( '-' ) >= 0 )
            {
                int index = p2.indexOf( '-' );
                q2 = p2.substring( index );
                p2 = p2.substring( 0, index );
            }
            try
            {
                BigInteger n1 = new BigInteger( p1 );
                BigInteger n2 = new BigInteger( p2 );
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
                final int result = q1.compareTo( q2 );
                if ( result != 0 )
                {
                    return result;
                }
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
            BigInteger n2 = BIG_INTEGER_ZERO;
            while ( tok1.hasMoreTokens() )
            {
                try
                {
                    BigInteger n1 = new BigInteger( tok1.nextToken() );
                    int result = n1.compareTo( n2 );
                    if ( result != 0 )
                    {
                        return result;
                    }
                }
                catch ( NumberFormatException e )
                {
                    // any token is better than zero
                    return +1;
                }
            }
            return -1;
        }
        if ( tok2.hasMoreTokens() )
        {
            BigInteger n1 = BIG_INTEGER_ZERO;
            while ( tok2.hasMoreTokens() )
            {
                try
                {
                    BigInteger n2 = new BigInteger( tok2.nextToken() );
                    int result = n1.compareTo( n2 );
                    if ( result != 0 )
                    {
                        return result;
                    }
                }
                catch ( NumberFormatException e )
                {
                    // any token is better than zero
                    return -1;
                }
            }
            return +1;
        }
        return 0;
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

    /**
     * {@inheritDoc}
     */
    public int getSegmentCount( ArtifactVersion v )
    {
        final String version = v.toString();
        StringTokenizer tok = new StringTokenizer( version, "." );
        return tok.countTokens();
    }

    /**
     * {@inheritDoc}
     */
    public ArtifactVersion incrementSegment( ArtifactVersion v, int segment )
    {
        if ( segment < 0 || segment > getSegmentCount( v ) )
        {
            throw new IllegalArgumentException( "Invalid segment" );
        }
        final String version = v.toString();
        StringBuffer buf = new StringBuffer();
        StringTokenizer tok = new StringTokenizer( version, "." );
        boolean first = true;
        while ( segment >= 0 && tok.hasMoreTokens() )
        {
            if ( first )
            {
                first = false;
            }
            else
            {
                buf.append( '.' );
            }
            String p = tok.nextToken();
            String q = null;
            if ( p.indexOf( '-' ) >= 0 )
            {
                int index = p.indexOf( '-' );
                q = p.substring( index + 1 );
                p = p.substring( 0, index );
            }

            if ( segment == 0 )
            {
                try
                {
                    BigInteger n = new BigInteger( p );
                    p = n.add( BIG_INTEGER_ONE ).toString();
                    q = null;
                }
                catch ( NumberFormatException e )
                {
                    // ok, let's try some common tricks
                    if ( "alpha".equalsIgnoreCase( p ) )
                    {
                        if ( q == null )
                        {
                            p = "beta";
                        }
                        else
                        {
                            try
                            {
                                BigInteger n = new BigInteger( q );
                                q = n.add( BIG_INTEGER_ONE ).toString();
                            }
                            catch ( NumberFormatException e1 )
                            {
                                p = "beta";
                                q = null;
                            }
                        }
                    }
                    else if ( "beta".equalsIgnoreCase( p ) )
                    {
                        if ( q == null )
                        {
                            p = "milestone";
                        }
                        else
                        {
                            try
                            {
                                BigInteger n = new BigInteger( q );
                                q = n.add( BIG_INTEGER_ONE ).toString();
                            }
                            catch ( NumberFormatException e1 )
                            {
                                p = "milestone";
                                q = null;
                            }
                        }
                    }
                    else if ( "milestone".equalsIgnoreCase( p ) )
                    {
                        if ( q == null )
                        {
                            p = "rc";
                        }
                        else
                        {
                            try
                            {
                                BigInteger n = new BigInteger( q );
                                q = n.add( BIG_INTEGER_ONE ).toString();
                            }
                            catch ( NumberFormatException e1 )
                            {
                                p = "rc";
                                q = null;
                            }
                        }
                    }
                    else if ( "cr".equalsIgnoreCase( p ) || "rc".equalsIgnoreCase( p ) )
                    {
                        if ( q == null )
                        {
                            p = "ga";
                        }
                        else
                        {
                            try
                            {
                                BigInteger n = new BigInteger( q );
                                q = n.add( BIG_INTEGER_ONE ).toString();
                            }
                            catch ( NumberFormatException e1 )
                            {
                                p = "ga";
                                q = null;
                            }
                        }
                    }
                    else if ( "ga".equalsIgnoreCase( p ) || "final".equalsIgnoreCase( p ) )
                    {
                        if ( q == null )
                        {
                            p = "sp";
                            q = "1";
                        }
                        else
                        {
                            try
                            {
                                BigInteger n = new BigInteger( q );
                                q = n.add( BIG_INTEGER_ONE ).toString();
                            }
                            catch ( NumberFormatException e1 )
                            {
                                p = "sp";
                                q = "1";
                            }
                        }
                    }
                    else
                    {
                        p = VersionComparators.alphaNumIncrement( p );
                    }
                }
            }
            buf.append( p );
            if ( q != null )
            {
                buf.append( '-' );
                buf.append( q );
            }
            segment--;
        }
        while ( tok.hasMoreTokens() )
        {
            if ( first )
            {
                first = false;
            }
            else
            {
                buf.append( '.' );
            }
            tok.nextToken();
            buf.append( "0" );
        }
        return new DefaultArtifactVersion( buf.toString() );
    }
}
