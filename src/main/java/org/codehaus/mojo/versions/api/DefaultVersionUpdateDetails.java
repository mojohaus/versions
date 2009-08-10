package org.codehaus.mojo.versions.api;

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

/**
 * A default implementation of {@link org.codehaus.mojo.versions.api.VersionUpdateDetails}.
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public class DefaultVersionUpdateDetails
    implements VersionUpdateDetails
{

    private final VersionDetails versionDetails;

    private final boolean includeSnapshots;

    public DefaultVersionUpdateDetails(VersionDetails versionDetails, boolean includeSnapshots)
    {
        this.includeSnapshots = includeSnapshots;
        this.versionDetails = versionDetails;
    }

    public final ArtifactVersion getCurrentVersion()
    {
        return versionDetails.getCurrentVersion();
    }

    public final ArtifactVersion getOldestUpdate( UpdateScope updateScope )
    {
        return versionDetails.getOldestUpdate( updateScope, includeSnapshots );
    }

    public final ArtifactVersion getNewestUpdate( UpdateScope updateScope )
    {
        return versionDetails.getNewestUpdate( updateScope, includeSnapshots );
    }

    public final ArtifactVersion[] getAllUpdates( UpdateScope updateScope )
    {
        return versionDetails.getAllUpdates(updateScope, includeSnapshots);
    }
    
    protected final VersionDetails getVersionDetails() {
        return versionDetails;
    }
}
