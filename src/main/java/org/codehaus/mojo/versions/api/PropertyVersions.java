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
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.OverConstrainedVersionException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.ordering.VersionComparator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Manages a property that is associated with one or more artifacts.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class PropertyVersions
    extends AbstractVersionDetails
{
    private final String name;

    private final String profileId;

    private final Set<ArtifactAssociation> associations;

    private final VersionsHelper helper;

    /**
     * The available versions.
     *
     * @since 1.0-beta-1
     */
    private final SortedSet<ArtifactVersion> versions;

    private final PropertyVersions.PropertyVersionComparator comparator;

    PropertyVersions( String profileId, String name, VersionsHelper helper, Set<ArtifactAssociation> associations )
        throws ArtifactMetadataRetrievalException
    {
        this.profileId = profileId;
        this.name = name;
        this.helper = helper;
        this.associations = new TreeSet<ArtifactAssociation>( associations );
        this.comparator = new PropertyVersionComparator();
        this.versions = resolveAssociatedVersions( helper, associations, comparator );

    }

    private static SortedSet<ArtifactVersion> resolveAssociatedVersions( VersionsHelper helper,
                                                                         Set<ArtifactAssociation> associations,
                                                                         VersionComparator versionComparator )
        throws ArtifactMetadataRetrievalException
    {
        SortedSet<ArtifactVersion> versions = null;
        for ( ArtifactAssociation association : associations )
        {
            final ArtifactVersions associatedVersions =
                helper.lookupArtifactVersions( association.getArtifact(), association.isUsePluginRepositories() );
            if ( versions != null )
            {
                final ArtifactVersion[] artifactVersions = associatedVersions.getVersions( true );
                // since ArtifactVersion does not override equals, we have to do this the hard way
                // result.retainAll( Arrays.asList( artifactVersions ) );
                Iterator j = versions.iterator();
                while ( j.hasNext() )
                {
                    boolean contains = false;
                    ArtifactVersion version = (ArtifactVersion) j.next();
                    for ( ArtifactVersion artifactVersion : artifactVersions )
                    {
                        if ( version.compareTo( artifactVersion ) == 0 )
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
                versions = new TreeSet<ArtifactVersion>( versionComparator );
                versions.addAll( Arrays.asList( associatedVersions.getVersions( true ) ) );
            }
        }
        if ( versions == null )
        {
            versions = new TreeSet<ArtifactVersion>( versionComparator );
        }
        return Collections.unmodifiableSortedSet( versions );
    }

    /**
     * Gets the rule for version comparison of this artifact.
     *
     * @return the rule for version comparison of this artifact.
     * @since 1.0-beta-1
     */
    public VersionComparator getVersionComparator()
    {
        return comparator;
    }

    public ArtifactAssociation[] getAssociations()
    {
        return associations.toArray( new ArtifactAssociation[associations.size()] );
    }

    private VersionComparator[] lookupComparators()
    {
        Set result = new HashSet();
        Iterator i = associations.iterator();
        while ( i.hasNext() )
        {
            ArtifactAssociation association = (ArtifactAssociation) i.next();
            result.add( helper.getVersionComparator( association.getArtifact() ) );
        }
        return (VersionComparator[]) result.toArray( new VersionComparator[result.size()] );
    }

    /**
     * Uses the supplied {@link Collection} of {@link Artifact} instances to see if an ArtifactVersion can be provided.
     *
     * @param artifacts The {@link Collection} of {@link Artifact} instances .
     * @return The versions that can be resolved from the supplied Artifact instances or an empty array if no version
     *         can be resolved (i.e. the property is not associated with any of the supplied artifacts or the property is
     *         also associated to an artifact that has not been provided).
     * @throws org.apache.maven.plugin.MojoExecutionException
     *          When things go wrong.
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions( Collection<Artifact> artifacts )
        throws MojoExecutionException
    {
        List<ArtifactVersion> result = new ArrayList<ArtifactVersion>();
        // go through all the associations
        // see if they are met from the collection
        // add the version if they are
        // go through all the versions
        // see if the version is available for all associations
        for ( ArtifactAssociation association : associations )
        {
            for ( Artifact artifact : artifacts )
            {
                if ( association.getGroupId().equals( artifact.getGroupId() ) && association.getArtifactId().equals(
                    artifact.getArtifactId() ) )
                {
                    try
                    {
                        result.add( artifact.getSelectedVersion() );
                    }
                    catch ( OverConstrainedVersionException e )
                    {
                        // ignore this one as we cannot resolve a valid version
                    }
                }
            }
        }
        // we now have a list of all the versions that partially satisfy the association requirements
        Iterator<ArtifactVersion> k = result.iterator();
        versions:
        while ( k.hasNext() )
        {
            ArtifactVersion candidate = k.next();
            associations:
            for ( ArtifactAssociation association : associations )
            {
                for ( Artifact artifact : artifacts )
                {
                    if ( association.getGroupId().equals( artifact.getGroupId() ) && association.getArtifactId().equals(
                        artifact.getArtifactId() ) )
                    {
                        try
                        {
                            if ( candidate.toString().equals( artifact.getSelectedVersion().toString() ) )
                            {
                                // this association can be met, try the next
                                continue associations;
                            }
                        }
                        catch ( OverConstrainedVersionException e )
                        {
                            // ignore this one again
                        }
                    }
                }
                // candidate is not valid as at least one association cannot be met
                k.remove();
                continue versions;
            }
        }
        return asArtifactVersionArray( result );
    }

    /**
     * Uses the {@link DefaultVersionsHelper} to find all available versions that match all
     * the associations with this property.
     *
     * @param includeSnapshots Whether to include snapshot versions in our search.
     * @return The (possibly empty) array of versions.
     */
    public synchronized ArtifactVersion[] getVersions( boolean includeSnapshots )
    {
        Set<ArtifactVersion> result;
        if ( includeSnapshots )
        {
            result = versions;
        }
        else
        {
            result = new TreeSet<ArtifactVersion>( getVersionComparator() );
            for ( ArtifactVersion candidate : versions )
            {
                if ( ArtifactUtils.isSnapshot( candidate.toString() ) )
                {
                    continue;
                }
                result.add( candidate );
            }
        }
        return asArtifactVersionArray( result );
    }

    private ArtifactVersion[] asArtifactVersionArray( Collection<ArtifactVersion> result )
    {
        if ( result == null || result.isEmpty() )
        {
            return new ArtifactVersion[0];
        }
        else
        {
            final ArtifactVersion[] answer = result.toArray( new ArtifactVersion[result.size()] );
            VersionComparator[] rules = lookupComparators();
            assert rules.length > 0;
            Arrays.sort( answer, rules[0] );
            if ( rules.length == 1 || answer.length == 1 )
            {
                // only one rule...
                return answer;
            }
            ArtifactVersion[] alt = answer.clone();
            for ( int j = 1; j < rules.length; j++ )
            {
                Arrays.sort( alt, rules[j] );
                if ( !Arrays.equals( alt, answer ) )
                {
                    throw new IllegalStateException( "Property " + name + " is associated with multiple artifacts" +
                                                         " and these artifacts use different version sorting rules and these rules are effectively"
                                                         +
                                                         " incompatible for the set of versions available to this property.\nFirst rule says: "
                                                         +
                                                         Arrays.asList( answer ) + "\nSecond rule says: "
                                                         + Arrays.asList( alt ) );
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
        return "PropertyVersions{" + ( profileId == null ? "" : "profileId='" + profileId + "', " ) + "name='" + name +
            '\'' + ", associations=" + associations + '}';
    }

    public ArtifactVersion getNewestVersion( String currentVersion, Property property, Boolean allowSnapshots,
                                             List reactorProjects, VersionsHelper helper )
        throws MojoExecutionException
    {
        final boolean includeSnapshots = !property.isBanSnapshots() && Boolean.TRUE.equals( allowSnapshots );
        helper.getLog().debug( "Property ${" + property.getName() + "}: Set of valid available versions is " +
                                   Arrays.asList( getVersions( includeSnapshots ) ) );
        VersionRange range;
        try
        {
            if ( property.getVersion() != null )
            {
                range = VersionRange.createFromVersionSpec( property.getVersion() );
                helper.getLog().debug( "Property ${" + property.getName() + "}: Restricting results to " + range );
            }
            else
            {
                range = null;
                helper.getLog().debug( "Property ${" + property.getName() + "}: Restricting results to " + range );
            }
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        ArtifactVersion result =
            getNewestVersion( range, helper.createArtifactVersion( currentVersion ), null, includeSnapshots, false,
                              true );
        helper.getLog().debug( "Property ${" + property.getName() + "}: Current winner is: " + result );

        if ( property.isSearchReactor() )
        {
            helper.getLog().debug( "Property ${" + property.getName() + "}: Searching reactor for a valid version..." );
            Collection reactorArtifacts = helper.extractArtifacts( reactorProjects );
            ArtifactVersion[] reactorVersions = getVersions( reactorArtifacts );
            helper.getLog().debug(
                "Property ${" + property.getName() + "}: Set of valid available versions from the reactor is " +
                    Arrays.asList( reactorVersions ) );
            ArtifactVersion fromReactor = null;
            if ( reactorVersions.length > 0 )
            {
                for ( int j = reactorVersions.length - 1; j >= 0; j-- )
                {
                    if ( range == null || ArtifactVersions.isVersionInRange( reactorVersions[j], range ) )
                    {
                        fromReactor = reactorVersions[j];
                        helper.getLog().debug(
                            "Property ${" + property.getName() + "}: Reactor has version " + fromReactor );
                        break;
                    }
                }
            }
            if ( fromReactor != null && ( result != null || !currentVersion.equals( fromReactor.toString() ) ) )
            {
                if ( property.isPreferReactor() )
                {
                    helper.getLog().debug(
                        "Property ${" + property.getName() + "}: Reactor has a version and we prefer the reactor" );
                    result = fromReactor;
                }
                else
                {
                    if ( result == null )
                    {
                        helper.getLog().debug( "Property ${" + property.getName() + "}: Reactor has the only version" );
                        result = fromReactor;
                    }
                    else if ( getVersionComparator().compare( result, fromReactor ) < 0 )
                    {
                        helper.getLog().debug( "Property ${" + property.getName() + "}: Reactor has a newer version" );
                        result = fromReactor;
                    }
                    else
                    {
                        helper.getLog().debug(
                            "Property ${" + property.getName() + "}: Reactor has the same or older version" );
                    }
                }
            }
        }
        return result;
    }

    private final class PropertyVersionComparator
        implements VersionComparator
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

        public int getSegmentCount( ArtifactVersion v )
        {
            if ( !isAssociated() )
            {
                throw new IllegalStateException( "Cannot compare versions for a property with no associations" );
            }
            VersionComparator[] comparators = lookupComparators();
            assert comparators.length >= 1 : "we have at least one association => at least one comparator";
            int result = comparators[0].getSegmentCount( v );
            for ( int i = 1; i < comparators.length; i++ )
            {
                int alt = comparators[i].getSegmentCount( v );
                if ( result != alt )
                {
                    throw new IllegalStateException( "Property " + name + " is associated with multiple artifacts" +
                                                         " and these artifacts use different version sorting rules and these rules are effectively"
                                                         +
                                                         " incompatible for the two of versions being compared.\nFirst rule says getSegmentCount(\""
                                                         +
                                                         v + "\") = " + result + "\nSecond rule says getSegmentCount(\""
                                                         + v + "\") = " + alt );
                }
            }
            return result;
        }

        public ArtifactVersion incrementSegment( ArtifactVersion v, int segment )
        {
            if ( !isAssociated() )
            {
                throw new IllegalStateException( "Cannot compare versions for a property with no associations" );
            }
            VersionComparator[] comparators = lookupComparators();
            assert comparators.length >= 1 : "we have at least one association => at least one comparator";
            ArtifactVersion result = comparators[0].incrementSegment( v, segment );
            for ( int i = 1; i < comparators.length; i++ )
            {
                ArtifactVersion alt = comparators[i].incrementSegment( v, segment );
                if ( !result.toString().equals( alt.toString() ) )
                {
                    throw new IllegalStateException( "Property " + name + " is associated with multiple artifacts" +
                                                         " and these artifacts use different version sorting rules and these rules are effectively"
                                                         +
                                                         " incompatible for the two of versions being compared.\nFirst rule says incrementSegment(\""
                                                         +
                                                         v + "\", " + segment + ") = " + result
                                                         + "\nSecond rule says incrementSegment(\"" + v + "\", " +
                                                         segment + ") = " + alt );
                }
            }
            return result;
        }


    }

}
