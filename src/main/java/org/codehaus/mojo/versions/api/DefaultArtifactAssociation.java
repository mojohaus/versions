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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;

/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since Aug 6, 2009 9:23:13 AM
 */
final class DefaultArtifactAssociation
    implements Comparable, ArtifactAssociation
{
    private final Artifact artifact;

    private final boolean usePluginRepositories;

    DefaultArtifactAssociation( Artifact artifact, boolean usePluginRepositories )
    {
        artifact.getClass(); // throw NPE if null;
        this.artifact = artifact;
        this.usePluginRepositories = usePluginRepositories;
    }

    public String getGroupId()
    {
        return artifact.getGroupId();
    }

    public String getArtifactId()
    {
        return artifact.getArtifactId();
    }

    public Artifact getArtifact()
    {
        return artifact;
    }

    public boolean isUsePluginRepositories()
    {
        return usePluginRepositories;
    }

    public int compareTo( Object o )
    {
        if ( this == o )
        {
            return 0;
        }
        if ( o == null || getClass() != o.getClass() )
        {
            return 1;
        }
        DefaultArtifactAssociation that = (DefaultArtifactAssociation) o;

        int rv = getGroupId().compareTo( that.getGroupId() );
        if ( rv != 0 )
        {
            return rv;
        }
        rv = getArtifactId().compareTo( that.getArtifactId() );
        if ( rv != 0 )
        {
            return rv;
        }
        if ( usePluginRepositories != that.usePluginRepositories )
        {
            return usePluginRepositories ? 1 : -1;
        }
        return 0;
    }

    public boolean equals( Object o )
    {
        if ( this == o )
        {
            return true;
        }
        if ( o == null || getClass() != o.getClass() )
        {
            return false;
        }

        DefaultArtifactAssociation that = (DefaultArtifactAssociation) o;

        if ( usePluginRepositories != that.usePluginRepositories )
        {
            return false;
        }
        if ( !getArtifactId().equals( that.getArtifactId() ) )
        {
            return false;
        }
        if ( !getGroupId().equals( that.getGroupId() ) )
        {
            return false;
        }

        return true;
    }

    public int hashCode()
    {
        int result = getGroupId().hashCode();
        result = 31 * result + getArtifactId().hashCode();
        result = 31 * result + ( usePluginRepositories ? 1 : 0 );
        return result;
    }

    public String toString()
    {
        return ( usePluginRepositories ? "plugin:" : "artifact:" ) + ArtifactUtils.versionlessKey( artifact );
    }
}
