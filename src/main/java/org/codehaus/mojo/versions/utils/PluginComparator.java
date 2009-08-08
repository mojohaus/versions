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

import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;

import java.util.Comparator;

/**
 * A comparator used to sort plugins and report plugins by group id, artifact id and finally version.
 *
 * @since 1.0-beta-1
 */
public class PluginComparator
    implements Comparator
{

    /**
     * Compares to {@link Plugin} or {@link ReportPlugin} instances.
     *
     * @param o1 the first object
     * @param o2 the second object.
     * @return the comparison result
     * @see java.util.Comparator#compare(Object, Object)
     * @since 1.0-beta-1
     */
    public int compare( Object o1, Object o2 )
    {
        if ( !(o1 instanceof Plugin || o1 instanceof ReportPlugin) ) 
        {
            throw new IllegalArgumentException( "This comparator can only be used to compare Plugin and ReportPlugin instances" );
        }
        if ( !(o2 instanceof Plugin || o2 instanceof ReportPlugin) )
        {
            throw new IllegalArgumentException( "This comparator can only be used to compare Plugin and ReportPlugin instances" );
        }
        String g1 = o1 instanceof Plugin ? ((Plugin) o1).getGroupId() : ((ReportPlugin) o1).getGroupId();
        String g2 = o2 instanceof Plugin ? ((Plugin) o2).getGroupId() : ((ReportPlugin) o2).getGroupId();

        int r = g1.compareTo( g2 );
        if ( r == 0 )
        {
            String a1 = o1 instanceof Plugin ? ((Plugin) o1).getArtifactId() : ((ReportPlugin) o1).getArtifactId();
            String a2 = o2 instanceof Plugin ? ((Plugin) o2).getArtifactId() : ((ReportPlugin) o2).getArtifactId();
            r = a1.compareTo( a2 );
        }
        if ( r == 0 )
        {
            String v1 = o1 instanceof Plugin ? ((Plugin) o1).getVersion() : ((ReportPlugin) o1).getVersion();
            String v2 = o2 instanceof Plugin ? ((Plugin) o2).getVersion() : ((ReportPlugin) o2).getVersion();
            if ( v1 == null )
            {
                // hope I got the +1/-1 the right way around
                return v2 == null ? 0 : -1;
            }
            if ( v2 == null )
            {
                return 1;
            }
            r = v1.compareTo( v2 );
        }
        return r;
    }

}