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
*  http://www.apache.org/licenses/LICENSE-2.0
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
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.plugin.MojoExecutionException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Manages a property that is associated with one or more artifacts.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @since 1.0-alpha-3
 */
public class PropertyVersions
{
    private final String name;

    private final String profileId;

    private final Set/*<String*/ associations;

    private final VersionsHelper helper;

    /**
     * Constructs a new {@link org.codehaus.mojo.versions.api.PropertyVersions}.
     *
     * @param profileId
     * @param name      The property name.
     * @param helper    The {@link org.codehaus.mojo.versions.api.DefaultVersionsHelper}.
     */
    public PropertyVersions( String profileId, String name, VersionsHelper helper )
    {
        this.profileId = profileId;
        this.name = name;
        this.associations = new TreeSet();
        this.helper = helper;
    }

    public void addAssociation( Artifact artifact, boolean usePluginRepositories )
    {
        associations.add( new DefaultArtifactAssociation( artifact, usePluginRepositories ) );
    }

    public void removeAssociation( Artifact artifact, boolean usePluginRepositories )
    {
        associations.remove( new DefaultArtifactAssociation( artifact, usePluginRepositories ) );
    }

    public ArtifactAssocation[] getAssociations()
    {
        return (ArtifactAssocation[]) associations.toArray( new ArtifactAssocation[associations.size()] );
    }

    private Comparator[] lookupComparators( VersionsHelper helper )
    {
        Set result = new HashSet();
        Iterator i = associations.iterator();
        while ( i.hasNext() )
        {
            ArtifactAssocation association = (ArtifactAssocation) i.next();
            result.add( helper.getVersionComparator( association.getArtifact() ) );
        }
        return (Comparator[]) result.toArray( new Comparator[result.size()] );
    }

    /**
     * Uses the {@link DefaultVersionsHelper} to find all available versions that match all
     * the associations with this property.
     *
     * @param includeSnapshots Whether to include snapshot versions in our search.
     * @return The (possibly empty) array of versions.
     * @throws MojoExecutionException
     */
    public ArtifactVersion[] getVersions( boolean includeSnapshots )
        throws MojoExecutionException
    {
        List result = null;
        Iterator i = associations.iterator();
        while ( i.hasNext() )
        {
            ArtifactAssocation association = (ArtifactAssocation) i.next();
            final ArtifactVersions versions =
                helper.lookupArtifactVersions( association.getArtifact(), association.isUsePluginRepositories() );
            if ( result != null )
            {
                final ArtifactVersion[] artifactVersions = versions.getVersions( includeSnapshots );
                // since ArtifactVersion does not override equals, we have to do this the hard way
                // result.retainAll( Arrays.asList( artifactVersions ) );
                Iterator j = result.iterator();
                while ( j.hasNext() )
                {
                    boolean contains = false;
                    ArtifactVersion version = (ArtifactVersion) j.next();
                    for ( int k = 0; k < artifactVersions.length; k++ )
                    {
                        if ( version.compareTo( artifactVersions[k] ) == 0 )
                        {
                            contains = true;
                            break;
                        }
                    }
                    if ( !contains )
                    {
                        j.remove();
                    }
                }
            }
            else
            {
                result = new ArrayList( Arrays.asList( versions.getVersions( includeSnapshots ) ) );
            }
        }
        if ( result == null || result.isEmpty() )
        {
            return new ArtifactVersion[0];
        }
        else
        {
            final ArtifactVersion[] answer = (ArtifactVersion[]) result.toArray( new ArtifactVersion[result.size()] );
            Comparator[] rules = lookupComparators( helper );
            assert rules.length > 0;
            Arrays.sort( answer, rules[0] );
            if ( rules.length == 1 || answer.length == 1 )
            {
                // only one rule...
                return answer;
            }
            ArtifactVersion[] alt = (ArtifactVersion[]) answer.clone();
            for ( int j = 1; j < rules.length; j++ )
            {
                Arrays.sort( alt, rules[j] );
                if ( !Arrays.equals( alt, answer ) )
                {
                    throw new MojoExecutionException( "Property " + name + " is associated with multiple artifacts"
                        + " and these artifacts use different version sorting rules and these rules are effectively"
                        + " incompatible for the set of versions available to this property.\nFirst rule says: "
                        + Arrays.asList( answer ) + "\nSecond rule says: " + Arrays.asList( alt ) );
                }
            }
            return answer;
        }
    }

    public String getName()
    {
        return name;
    }

    public String getProfileId()
    {
        return profileId;
    }

    public boolean isAssociated()
    {
        return !associations.isEmpty();
    }

    public String toString()
    {
        return "PropertyVersions{" + ( profileId == null ? "" : "profileId='" + profileId + "', " ) + "name='" + name
            + '\'' + ", associations=" + associations + '}';
    }

    public void clearAssociations()
    {
        associations.clear();
    }

    private static final class DefaultArtifactAssociation
        implements Comparable, ArtifactAssocation
    {
        private final Artifact artifact;

        private final boolean usePluginRepositories;

        private DefaultArtifactAssociation( Artifact artifact, boolean usePluginRepositories )
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

}
