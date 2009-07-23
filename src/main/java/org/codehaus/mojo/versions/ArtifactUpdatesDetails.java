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
import org.apache.maven.artifact.versioning.ArtifactVersion;

/**
 * Contains details of the available updates for an artifact.
 *
 * @since 1.0-beta-1
 */
public class ArtifactUpdatesDetails
{
    private final Artifact artifact;

    private final ArtifactVersion nextVersion;

    private final ArtifactVersion nextIncremental;

    private final ArtifactVersion latestIncremental;

    private final ArtifactVersion nextMinor;

    private final ArtifactVersion latestMinor;

    private final ArtifactVersion nextMajor;

    private final ArtifactVersion latestMajor;

    private final ArtifactVersion[] all;

    public ArtifactUpdatesDetails( Artifact artifact, ArtifactVersion nextVersion, ArtifactVersion nextIncremental,
                                   ArtifactVersion latestIncremental, ArtifactVersion nextMinor,
                                   ArtifactVersion latestMinor, ArtifactVersion nextMajor, ArtifactVersion latestMajor,
                                   ArtifactVersion[] all )
    {
        this.artifact = artifact;
        this.nextVersion = nextVersion;
        this.nextIncremental = nextIncremental;
        this.latestIncremental = latestIncremental;
        this.nextMinor = nextMinor;
        this.latestMinor = latestMinor;
        this.nextMajor = nextMajor;
        this.latestMajor = latestMajor;
        this.all = all;
    }

    public Artifact getArtifact()
    {
        return artifact;
    }

    public ArtifactVersion getNextIncremental()
    {
        return nextIncremental;
    }

    public ArtifactVersion getLatestIncremental()
    {
        return latestIncremental;
    }

    public ArtifactVersion getNextMinor()
    {
        return nextMinor;
    }

    public ArtifactVersion getLatestMinor()
    {
        return latestMinor;
    }

    public ArtifactVersion getNextMajor()
    {
        return nextMajor;
    }

    public ArtifactVersion getLatestMajor()
    {
        return latestMajor;
    }

    public ArtifactVersion getNextVersion()
    {
        return nextVersion;
    }

    public ArtifactVersion[] getAll()
    {
        return all;
    }
}
