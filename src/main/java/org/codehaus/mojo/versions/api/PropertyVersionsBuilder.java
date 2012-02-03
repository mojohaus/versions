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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Builds {@link org.codehaus.mojo.versions.api.PropertyVersions} instances.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
class PropertyVersionsBuilder
{
    private final String name;

    private final String profileId;

    private final Set<ArtifactAssociation> associations;

    private final VersionsHelper helper;

    private final Map<String, Boolean> upperBounds = new LinkedHashMap<String, Boolean>();

    private final Map<String, Boolean> lowerBounds = new LinkedHashMap<String, Boolean>();

    /**
     * Constructs a new {@link org.codehaus.mojo.versions.api.PropertyVersions}.
     *
     * @param profileId The profileId.
     * @param name      The property name.
     * @param helper    The {@link org.codehaus.mojo.versions.api.DefaultVersionsHelper}.
     */
    public PropertyVersionsBuilder( String profileId, String name, VersionsHelper helper )
    {
        this.profileId = profileId;
        this.name = name;
        this.associations = new TreeSet<ArtifactAssociation>();
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

    public void clearAssociations()
    {
        associations.clear();
    }

    public boolean isAssociated()
    {
        return !associations.isEmpty();
    }

    public ArtifactAssociation[] getAssociations()
    {
        return associations.toArray( new ArtifactAssociation[associations.size()] );
    }

    public PropertyVersions newPropertyVersions()
        throws ArtifactMetadataRetrievalException
    {
        return new PropertyVersions( profileId, name, helper, associations );
    }

    public String getName()
    {
        return name;
    }

    public String getVersionRange()
    {
        Comparator<ArtifactVersion> comparator = new PropertyVersionComparator();
        if ( lowerBounds.isEmpty() && upperBounds.isEmpty() )
        {
            return null;
        }
        ArtifactVersion lowerBound = null;
        boolean includeLower = true;
        for ( Map.Entry<String, Boolean> entry : lowerBounds.entrySet() )
        {
            ArtifactVersion candidate = helper.createArtifactVersion( entry.getKey() );
            if ( lowerBound == null )
            {
                lowerBound = candidate;
                includeLower = entry.getValue();
            }
            else
            {
                final int result = comparator.compare( lowerBound, candidate );
                if ( result > 0 )
                {
                    lowerBound = candidate;
                    includeLower = entry.getValue();
                }
                else if ( result == 0 )
                {
                    includeLower = includeLower && entry.getValue();
                }
            }
        }
        ArtifactVersion upperBound = null;
        boolean includeUpper = true;
        for ( Map.Entry<String, Boolean> entry : upperBounds.entrySet() )
        {
            ArtifactVersion candidate = helper.createArtifactVersion( entry.getKey() );
            if ( upperBound == null )
            {
                upperBound = candidate;
                includeUpper = entry.getValue();
            }
            else
            {
                final int result = comparator.compare( upperBound, candidate );
                if ( result < 0 )
                {
                    upperBound = candidate;
                    includeUpper = entry.getValue();
                }
                else if ( result == 0 )
                {
                    includeUpper = includeUpper && entry.getValue();
                }
            }
        }
        StringBuilder buf = new StringBuilder();
        if ( includeLower )
        {
            buf.append( '[' );
        }
        else
        {
            buf.append( '(' );
        }
        if ( lowerBound != null )
        {
            buf.append( lowerBound );
        }
        buf.append( ',' );
        if ( upperBound != null )
        {
            buf.append( upperBound );
        }
        if ( includeUpper )
        {
            buf.append( ']' );
        }
        else
        {
            buf.append( ')' );
        }
        return buf.toString();
    }

    public void addLowerBound( String lowerBound, boolean includeLower )
    {
        Boolean value = lowerBounds.get( lowerBound );
        if ( value == null )
        {
            value = includeLower;
        }
        else
        {
            value = includeLower && value;
        }
        lowerBounds.put( lowerBound, value );
    }

    public void addUpperBound( String upperBound, boolean includeUpper )
    {
        Boolean value = upperBounds.get( upperBound );
        if ( value == null )
        {
            value = includeUpper;
        }
        else
        {
            value = includeUpper && value;
        }
        upperBounds.put( upperBound, value );
    }

    private VersionComparator[] lookupComparators()
    {
        Set<VersionComparator> result = new HashSet<VersionComparator>();
        for ( ArtifactAssociation association : associations )
        {
            result.add( helper.getVersionComparator( association.getArtifact() ) );
        }
        return result.toArray( new VersionComparator[result.size()] );
    }

    private final class PropertyVersionComparator
        implements Comparator<ArtifactVersion>
    {
        public int compare( ArtifactVersion v1, ArtifactVersion v2 )
        {
            return innerCompare( v1, v2 );
        }

        private int innerCompare( ArtifactVersion v1, ArtifactVersion v2 )
        {
            if ( !isAssociated() )
            {
                throw new IllegalStateException( "Cannot compare versions for a property with no associations" );
            }
            VersionComparator[] comparators = lookupComparators();
            assert comparators.length >= 1 : "we have at least one association => at least one comparator";
            int result = comparators[0].compare( v1, v2 );
            for ( int i = 1; i < comparators.length; i++ )
            {
                int alt = comparators[i].compare( v1, v2 );
                if ( result != alt && ( result >= 0 && alt < 0 ) || ( result <= 0 && alt > 0 ) )
                {
                    throw new IllegalStateException( "Property " + name + " is associated with multiple artifacts" +
                                                         " and these artifacts use different version sorting rules and these rules are effectively"
                                                         +
                                                         " incompatible for the two of versions being compared.\nFirst rule says compare(\""
                                                         + v1 +
                                                         "\", \"" + v2 + "\") = " + result
                                                         + "\nSecond rule says compare(\"" + v1 + "\", \"" + v2 +
                                                         "\") = " + alt );
                }
            }
            return result;
        }

    }

}
