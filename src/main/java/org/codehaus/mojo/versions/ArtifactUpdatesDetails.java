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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.UpdateScope;
import org.codehaus.mojo.versions.api.DefaultVersionUpdateDetails;
import org.codehaus.mojo.versions.api.VersionDetails;

import java.util.Iterator;
import java.util.Arrays;

/**
 * Contains details of the available updates for an artifact.
 *
 * @since 1.0-beta-1
 */
public class ArtifactUpdatesDetails
    extends DefaultVersionUpdateDetails
{
    private final Artifact artifact;

    public ArtifactUpdatesDetails( ArtifactVersions v, ArtifactVersion current, boolean includeSnapshots)
        throws ArtifactMetadataRetrievalException
    {
        super( v, current, includeSnapshots );
        this.artifact = v.getArtifact();
    }
    
    public Artifact getArtifact()
    {
        return artifact;
    }

}
