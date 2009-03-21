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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import java.util.Comparator;

/**
 * Utility.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public final class VersionComparators
{

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
}
