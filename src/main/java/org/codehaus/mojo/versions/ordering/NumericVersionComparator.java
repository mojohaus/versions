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

import java.util.Comparator;
import java.util.StringTokenizer;

/**
 * A comparator which will compare all segments of a dot separated version string as numbers if possible,
 * i.e. 1.3.34 &gt; 1.3.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.9 and 1.3.4.3.2.34 &gt; 1.3.4.3.2.34-SNAPSHOT
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class NumericVersionComparator
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

    /**
     * Returns a hash code value for the comparator class.
     * @return the hash code.
     */
    public int hashCode()
    {
        return getClass().hashCode();    
    }

    /**
     * Returns true if this object is the same type of comparator as the parameter.
     * @param obj the reference object with which to compare.
     * @return <code>true</code> if this object is the same as the obj
     *         argument; <code>false</code> otherwise.
     * @see #hashCode()
     * @see java.util.Hashtable
     */
    public boolean equals( Object obj )
    {
        return obj == this || (obj != null && getClass().equals( obj.getClass() ));
    }
}
