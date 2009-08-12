package org.codehaus.mojo.versions.utils;

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

import org.codehaus.mojo.versions.Property;

import java.util.Comparator;

/**
 * A comparator used to sort {@link Property}  instances.
 *
 * @since 1.0-beta-1
 */
public class PropertyComparator
    implements Comparator
{
    /**
     * Compares to {@link Property} instances.
     *
     * @param o1 the first object
     * @param o2 the second object.
     * @return the comparison result
     * @see java.util.Comparator#compare(Object, Object)
     * @since 1.0-beta-1
     */
    public int compare( Object o1, Object o2 )
    {
        if ( !( o1 instanceof Property ) )
        {
            throw new IllegalArgumentException( "This comparator can only be used to compare Property instances" );
        }
        if ( !( o2 instanceof Property ) )
        {
            throw new IllegalArgumentException( "This comparator can only be used to compare Property instances" );
        }
        Property p1 = (Property) o1;
        Property p2 = (Property) o2;
        return p1.getName().compareTo( p2.getName() );
    }
}
