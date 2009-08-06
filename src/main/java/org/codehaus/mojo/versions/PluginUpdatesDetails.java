package org.codehaus.mojo.versions;

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

import org.codehaus.mojo.versions.api.UpdateScope;

import java.util.Iterator;
import java.util.Map;

/**
 * Details of a plugin's updates.
 */
public class PluginUpdatesDetails
{
    private final ArtifactUpdatesDetails artifactDetails;

    private final Map dependencyDetails;

    public PluginUpdatesDetails( ArtifactUpdatesDetails artifactDetails, Map dependencyDetails )
    {
        artifactDetails.getClass(); // throw NPE if null
        dependencyDetails.getClass(); // throw NPE if null
        this.artifactDetails = artifactDetails;
        this.dependencyDetails = dependencyDetails;
    }

    public ArtifactUpdatesDetails getArtifactDetails()
    {
        return artifactDetails;
    }

    public Map getDependencyDetails()
    {
        return dependencyDetails;
    }

    public boolean isArtifactUpdateAvailable()
    {
        return artifactDetails.getAllUpdates( UpdateScope.ANY ).length > 0;
    }

    public boolean isDependencyUpdateAvailable()
    {
        for ( Iterator i = dependencyDetails.values().iterator(); i.hasNext(); )
        {
            ArtifactUpdatesDetails details = (ArtifactUpdatesDetails) i.next();
            if ( details.getAllUpdates( UpdateScope.ANY ).length > 0 )
            {
                return true;
            }
        }
        return false;
    }

    public boolean isUpdateAvailable()
    {
        return isArtifactUpdateAvailable() || isDependencyUpdateAvailable();
    }
}
