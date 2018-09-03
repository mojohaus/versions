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

import java.util.LinkedList;
import java.util.List;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;

/**
 * This class will handle the edge cases where a version update would have happened based on the usage of version ranges
 * to limit the replacement of the versions. We have currently the following scenario: The user defined
 * <code>allowMajorUpdates=false</code> and <code>allowMinorUpdates=true</code>. An given artifact
 * <code>groupId:artifactId:2.0.8</code> and a repository which contains the following versions of this artifact:
 * <ul>
 * <li>2.0.11</li>
 * <li>2.1.0-M1</li>
 * <li>2.2.1</li>
 * <li>3.0-beta-3</li>
 * <li>3.0</li>
 * <li>3.1.0</li>
 * <li>3.3.0</li>
 * </ul>
 * The {@link AbstractVersionDetails#getNewerVersions(String, int, boolean)} will use an upper version of
 * <code>2.1.0</code> to limit the versions to use. The result of this would be using <code>2.1.0-M1</code> which
 * contradicts the wish of the user of not updating the minor version. The root cause of this is the comparison of Maven
 * versions which will defined <code>2.1.0-M1</code> as less than <code>2.1.0</code>. The method
 * {@link #filter(ArtifactVersion, ArtifactVersion[])} will filter out those versions which violate the configuration
 * {@link #allowMajorUpdates}, {@link #allowMinorUpdates} {@link #allowIncrementalUpdates}.
 * 
 * @author Karl Heinz Marbaise
 */
public class MajorMinorIncrementalFilter
{

    private boolean allowMajorUpdates;

    private boolean allowMinorUpdates;

    private boolean allowIncrementalUpdates;

    public MajorMinorIncrementalFilter( boolean allowMajorUpdates, boolean allowMinorUpdates,
                                        boolean allowIncrementalUpdates )
    {
        this.allowMajorUpdates = allowMajorUpdates;
        this.allowMinorUpdates = allowMinorUpdates;
        this.allowIncrementalUpdates = allowIncrementalUpdates;
    }

    /**
     * @param selectedVersion The version which will be checked.
     * @param newerVersions The list of identified versions which are greater or equal than the selectedVersion.
     * @return The cleaned up list which obeys usage of {@link #allowMajorUpdates}, {@link #allowMinorUpdates},
     *         {@link #allowIncrementalUpdates}.
     */
    public ArtifactVersion[] filter( ArtifactVersion selectedVersion, ArtifactVersion[] newerVersions )
    {
        List<ArtifactVersion> versionsToUse = new LinkedList<ArtifactVersion>();
        for ( ArtifactVersion artifactVersion : newerVersions )
        {
            if ( artifactVersion.getMajorVersion() != selectedVersion.getMajorVersion() )
            {
                if ( allowMajorUpdates )
                {
                    if ( !versionsToUse.contains( artifactVersion ) )
                    {
                        versionsToUse.add( artifactVersion );
                    }
                }
            }
            else if ( artifactVersion.getMinorVersion() != selectedVersion.getMinorVersion() )
            {
                if ( allowMinorUpdates )
                {
                    if ( !versionsToUse.contains( artifactVersion ) )
                    {
                        versionsToUse.add( artifactVersion );
                    }
                }
            }
            else if ( artifactVersion.getIncrementalVersion() != selectedVersion.getIncrementalVersion() )
            {
                if ( allowIncrementalUpdates )
                {
                    if ( !versionsToUse.contains( artifactVersion ) )
                    {
                        versionsToUse.add( artifactVersion );
                    }
                }
            }
            else {
                // build number or qualifier.  Will already be sorted and higher
                if ( !versionsToUse.contains( artifactVersion ) )
                {
                        versionsToUse.add( artifactVersion );
                }
            }
        }
        return versionsToUse.toArray( new ArtifactVersion[versionsToUse.size()] );

    }
}
