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
        int segmentCount = getSegmentCount( v );
        // TODO make more preserving of number of segments
        if ( segment < 0 || segment >= segmentCount )
        {
            throw new IllegalArgumentException( "Invalid segment" );
        }
        if ( segmentCount == 1 )
        {
            // only the qualifier
            return new DefaultArtifactVersion( VersionComparators.alphaNumIncrement( v.toString() ) );
        }
        else
        {
            switch ( segment )
            {
                case 0:
                    return new DefaultArtifactVersion( "" + ( v.getMajorVersion() + 1 ) + ".0.0" );
                case 1:
                    return new DefaultArtifactVersion(
                        "" + v.getMajorVersion() + "." + ( v.getMinorVersion() + 1 ) + ".0" );
                case 2:
                    return new DefaultArtifactVersion(
                        "" + v.getMajorVersion() + "." + v.getMinorVersion() + "." + ( v.getIncrementalVersion()
                            + 1 ) );
                case 3:
                default:
                    if ( v.getQualifier() != null )
                    {
                        return new DefaultArtifactVersion(
                            "" + v.getMajorVersion() + "." + v.getMinorVersion() + "." + v.getIncrementalVersion() + "-"
                                + ( VersionComparators.alphaNumIncrement( v.getQualifier() ) ) );
                    }
                    return new DefaultArtifactVersion(
                        "" + v.getMajorVersion() + "." + v.getMinorVersion() + "." + v.getIncrementalVersion() + "-" + (
                            v.getBuildNumber() + 1 ) );
            }
        }
    }
}
