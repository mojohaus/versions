package org.codehaus.mojo.versions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLStreamException;

import org.apache.commons.lang.StringUtils;

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
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Replaces any -SNAPSHOT versions with the corresponding release version (if it has been released).
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo( name = "use-releases", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UseReleasesMojo
    extends AbstractVersionsDependencyUpdaterMojo
{

    /**
     * Whether to check for releases within the range.
     *
     * @since 2.3
     */
    @Parameter( property = "allowRangeMatching", defaultValue = "false" )
    private boolean allowRangeMatching;

    /**
     * Whether to fail if a SNAPSHOT could not be replaced
     *
     * @since 2.3
     */
    @Parameter( property = "failIfNotReplaced", defaultValue = "false" )
    private boolean failIfNotReplaced;

    /**
     * Update versions in properties.
     *
     * @since 2.8
     */
    @Parameter( property = "updateProperties", defaultValue = "false" )
    private boolean updateProperties;

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a snapshot version.
     */
    public final Pattern matchSnapshotRegex = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException when things go wrong with XML streaming
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        try
        {
            if ( getProject().getParent() != null && isProcessingParent() )
            {
                useReleases( pom, getProject().getParent() );
            }

            if ( getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
            {
                useReleases( pom, PomHelper.readImportedPOMsFromDependencyManagementSection( pom ) );
                useReleases( pom, getProject().getDependencyManagement().getDependencies() );
            }
            if ( getProject().getDependencies() != null && isProcessingDependencies() )
            {
                useReleases( pom, getProject().getDependencies() );
            }
            if ( updateProperties )
            {
                useReleaseProperties( pom );
            }

        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
    }

    /**
     * Check for a release version in the range of properties which define dependency versions.
     *
     * @param pom
     * @throws MojoExecutionException
     * @throws ArtifactMetadataRetrievalException
     * @throws XMLStreamException
     */
    private void useReleaseProperties( ModifiedPomXMLEventReader pom ) throws MojoExecutionException, ArtifactMetadataRetrievalException, XMLStreamException
    {
        // Return properties defined for dependencies AND dependencyManagement
        Map<Property, PropertyVersions> propertyVersionsByProperty =
            this.getHelper().getVersionPropertiesMap( getProject(), null, "", "", updateProperties );

        for ( Map.Entry<Property, PropertyVersions> entry : propertyVersionsByProperty.entrySet() )
        {
            Property property = entry.getKey();
            PropertyVersions propertyVersions = entry.getValue();

            // Check if a dependency of the associations of the property is excluded.
            // If so at least one dependency is excluded, the property is not processed.
            if ( keepProperty( propertyVersions ) )
            {

                final String currentVersion = getProject().getProperties().getProperty( property.getName() );
                if ( currentVersion == null )
                {
                    continue;
                }
                Matcher versionMatcher = matchSnapshotRegex.matcher( currentVersion );
                if ( versionMatcher.matches() )
                {
                    String releaseVersion = versionMatcher.group( 1 );

                    if ( !allowRangeMatching )
                    {
                        propertyNoRangeMatching( pom, propertyVersions, currentVersion, releaseVersion );
                    }
                    else
                    {
                        propertyRangeMatching( pom, propertyVersions, currentVersion, releaseVersion );
                    }
                }
            }
        }
    }

    /**
     * Replace the property with the release version which is common to all dependencies refering to the property. If no common release version is available, do nothing.
     *
     * @param pom
     * @param propertyVersions
     * @param currentVersion
     * @param releaseVersion
     * @throws MojoExecutionException
     * @throws ArtifactMetadataRetrievalException
     * @throws XMLStreamException
     */
    private void propertyNoRangeMatching( ModifiedPomXMLEventReader pom, PropertyVersions propertyVersions, String currentVersion, String releaseVersion )
        throws MojoExecutionException, ArtifactMetadataRetrievalException, XMLStreamException
    {
        List<String> noReleaseForArtifacts = new ArrayList<>();
        for ( ArtifactAssociation artifactAssociation : propertyVersions.getAssociations() )
        {
            if ( artifactAssociation.getArtifact().getVersion() != null )
            {
                artifactAssociation.getArtifact().setVersion( releaseVersion );
                ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifactAssociation.getArtifact(), false );
                if ( !isExcludeReactor() )
                {
                    artifactVersions = mergeReactorArtifactsVersions( artifactVersions );
                }
                if ( !artifactVersions.containsVersion( releaseVersion ) )
                {
                    noReleaseForArtifacts.add( artifactAssociation.getArtifact().toString() );
                }
            }
        }

        if ( noReleaseForArtifacts.isEmpty() )
        {
            if ( PomHelper.setPropertyVersion( pom, propertyVersions.getProfileId(), propertyVersions.getName(), releaseVersion ) )
            {
                getLog().info( "Updated ${" + propertyVersions.getName() + "} from " + currentVersion + " to " + releaseVersion );
            }
        }
        else
        {
            getLog().info( "${" + propertyVersions.getName() + "} not updated since the version " + releaseVersion
                + " is not available for the following dependencies: " + noReleaseForArtifacts );
            if ( failIfNotReplaced )
            {
                throw new NoSuchElementException( "No matching release of " + currentVersion + " found for dependencies " + noReleaseForArtifacts );
            }
        }
    }

    /**
     * Replace the property with the release version which match the given range and which is common to all dependencies refering to the property. If no common release version is available, do nothing.
     *
     * @param pom
     * @param propertyVersions
     * @param currentVersion
     * @param releaseVersion
     * @throws MojoExecutionException
     * @throws ArtifactMetadataRetrievalException
     * @throws XMLStreamException
     */
    private void propertyRangeMatching( ModifiedPomXMLEventReader pom, PropertyVersions propertyVersions, String currentVersion, String releaseVersion )
        throws ArtifactMetadataRetrievalException, MojoExecutionException, XMLStreamException
    {
        ArtifactVersion[] commonVersions = null;
        for ( ArtifactAssociation artifactAssociation : propertyVersions.getAssociations() )
        {
            if ( artifactAssociation.getArtifact().getVersion() != null )
            {
                artifactAssociation.getArtifact().setVersion( releaseVersion );
                ArtifactVersions artifactVersions = getHelper().lookupArtifactVersions( artifactAssociation.getArtifact(), false );
                if ( !isExcludeReactor() )
                {
                    artifactVersions = mergeReactorArtifactsVersions( artifactVersions );
                }
                if ( commonVersions == null )
                {
                    commonVersions = artifactVersions.getVersions();
                }
                else
                {
                    commonVersions = intersection( commonVersions, artifactVersions.getVersions() );
                }
                Arrays.sort( commonVersions, Collections.reverseOrder( artifactVersions.getVersionComparator() ) );
            }
        }

        if ( commonVersions != null && commonVersions.length != 0 )
        {
            ArtifactVersion finalVersion = null;
            for ( ArtifactVersion proposedVersion : commonVersions )
            {
                if ( proposedVersion.toString().startsWith( releaseVersion ) )
                {
                    getLog().debug( "Found matching release version " + proposedVersion + " for property " + propertyVersions.getName() + " with range version " + releaseVersion );
                    finalVersion = proposedVersion;
                    break;
                }
            }

            if ( PomHelper.setPropertyVersion( pom, propertyVersions.getProfileId(), propertyVersions.getName(), finalVersion.toString() ) )
            {
                getLog().info( "Updated ${" + propertyVersions.getName() + "} from " + currentVersion + " to " + finalVersion );
            }
        }
        else
        {
            getLog().info( "${" + propertyVersions.getName() + "} not updated since no release version is not available for all the property linked dependencies." );
            if ( failIfNotReplaced )
            {
                throw new NoSuchElementException( "No matching release found for dependencies linked to property " + propertyVersions.getName() );
            }
        }
    }

    /**
     * Check if the property has to be processed. A property is not processed if at least one of the dependency which is linked
     * to the property is excluded.
     *
     * For exemple, if it is produced by Reactor and we do not want to process this kind of dependencies, or if the property is linked to a dependency
     * management and the dependency management is not enabled (or conversely)
     *
     * @param propertyVersions
     * @return
     * @throws MojoExecutionException
     */
    private boolean keepProperty( PropertyVersions propertyVersions ) throws MojoExecutionException
    {
        boolean keepProperty = true;
        for ( ArtifactAssociation association : propertyVersions.getAssociations() )
        {
            Artifact artifactAssociation = association.getArtifact();
            Dependency dependency = getArtifactDependency( artifactAssociation );
            if ( dependency != null )
            {
                if ( isExcludeReactor() && isProducedByReactor( dependency ) )
                {
                    getLog().info( "Ignoring reactor dependency: " + toString( dependency ) + ". Property " + propertyVersions.getName() + " will not be processed." );
                    keepProperty = false;
                    break;
                }
            }
            else
            {
                getLog().info( "Ignoring artifact " + artifactAssociation + " since the corresponding dependency (or dependency management) must not be processed. Property "
                    + propertyVersions.getName() + " will not be processed." );
                keepProperty = false;
                break;
            }

        }
        return keepProperty;
    }

    /**
     * Return the dependency of the projet corresponding to the given artifact.
     * Look for the dependency in the project dependencies if {@link #isProcessingDependencies()} returns true
     * then in the projet depdendencyManager if {@link #isProcessingDependencyManagement()} returns true if no dependency is found.
     *
     * Return null if the dependency is not found.
     *
     * @param artifact
     * @return
     */
    @SuppressWarnings( "unchecked" )
    private Dependency getArtifactDependency( Artifact artifact )
    {
        Dependency result = null;
        if ( getProject().getDependencies() != null && isProcessingDependencies() )
        {
            result = getArtifactDependency( artifact, getProject().getDependencies() );
        }
        if ( result == null && getProject().getDependencyManagement() != null && isProcessingDependencyManagement() )
        {
            result = getArtifactDependency( artifact, getProject().getDependencyManagement().getDependencies() );
        }
        return result;
    }

    /**
     * Return the dependency corresponding to the given artifact from the given dependencies list.
     * Return null if the dependency is not found.
     *
     * @param artifact
     * @param dependencies
     * @return
     */
    private Dependency getArtifactDependency( Artifact artifact, List<Dependency> dependencies )
    {
        Dependency result = null;
        if ( dependencies != null )
        {
            for ( Dependency dependency : dependencies )
            {
                if ( isSame( artifact, dependency ) )
                {
                    result = dependency;
                    break;
                }
            }
        }
        return result;
    }

    private void useReleases( ModifiedPomXMLEventReader pom, MavenProject project )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        String version = project.getVersion();
        Matcher versionMatcher = matchSnapshotRegex.matcher( version );
        if ( versionMatcher.matches() )
        {
            String releaseVersion = versionMatcher.group( 1 );

            VersionRange versionRange;
            try
            {
                versionRange = VersionRange.createFromVersionSpec( releaseVersion );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( "Invalid version range specification: " + version, e );
            }

            Artifact artifact = artifactFactory.createDependencyArtifact( getProject().getParent().getGroupId(),
                                                                          getProject().getParent().getArtifactId(),
                                                                          versionRange, "pom", null, null );
            if ( !isIncluded( artifact ) )
            {
                return;
            }

            getLog().debug( "Looking for a release of " + toString( project ) );
            // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
            // retrieve release version if provided snapshot version.
            artifact.setVersion( releaseVersion );
            ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
            if ( !allowRangeMatching ) // standard behaviour
            {
                if ( versions.containsVersion( releaseVersion ) )
                {
                    if ( PomHelper.setProjectParentVersion( pom, releaseVersion ) )
                    {
                        getLog().info( "Updated " + toString( project ) + " to version " + releaseVersion );
                    }
                }
                else if ( failIfNotReplaced )
                {
                    throw new NoSuchElementException( "No matching release of " + toString( project )
                        + " found for update." );
                }
            }
            else
            {
                ArtifactVersion finalVersion = null;
                for ( ArtifactVersion proposedVersion : versions.getVersions( false ) )
                {
                    if ( proposedVersion.toString().startsWith( releaseVersion ) )
                    {
                        getLog().debug( "Found matching version " + proposedVersion + " for " + toString( project ) + " to version "
                            + releaseVersion );
                        finalVersion = proposedVersion;
                    }
                }

                if ( finalVersion != null )
                {
                    if ( PomHelper.setProjectParentVersion( pom, finalVersion.toString() ) )
                    {
                        getLog().info( "Updated " + toString( project ) + " to version " + finalVersion.toString() );
                    }
                }
                else
                {
                    getLog().info( "No matching release of " + toString( project ) + " to update via rangeMatching." );
                    if ( failIfNotReplaced )
                    {
                        throw new NoSuchElementException( "No matching release of " + toString( project )
                            + " found for update via rangeMatching." );
                    }
                }

            }
        }
    }

    private void useReleases( ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies )
        throws XMLStreamException, MojoExecutionException, ArtifactMetadataRetrievalException
    {
        for ( Dependency dep : dependencies )
        {
            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                getLog().info( "Ignoring reactor dependency: " + toString( dep ) );
                continue;
            }

            String version = dep.getVersion();
            Matcher versionMatcher = matchSnapshotRegex.matcher( version );
            if ( versionMatcher.matches() )
            {
                String releaseVersion = versionMatcher.group( 1 );
                Artifact artifact = this.toArtifact( dep );
                if ( !isIncluded( artifact ) )
                {
                    continue;
                }

                getLog().debug( "Looking for a release of " + toString( dep ) );
                // Force releaseVersion version because org.apache.maven.artifact.metadata.MavenMetadataSource does not
                // retrieve release version if provided snapshot version.
                artifact.setVersion( releaseVersion );
                ArtifactVersions versions = getHelper().lookupArtifactVersions( artifact, false );
                if ( !allowRangeMatching ) // standard behaviour
                {
                    noRangeMatching( pom, dep, version, releaseVersion, versions );
                }
                else
                {
                    rangeMatching( pom, dep, version, releaseVersion, versions );
                }
            }
        }
    }

    private void rangeMatching( ModifiedPomXMLEventReader pom, Dependency dep, String version, String releaseVersion,
                                ArtifactVersions versions )
        throws XMLStreamException
    {
        ArtifactVersion finalVersion = null;
        for ( ArtifactVersion proposedVersion : versions.getVersions( false ) )
        {
            if ( proposedVersion.toString().startsWith( releaseVersion ) )
            {
                getLog().debug( "Found matching version " + proposedVersion + " for " + toString( dep ) + " to version " + releaseVersion );
                finalVersion = proposedVersion;
            }
        }

        if ( finalVersion != null )
        {
            if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version,
                                                 finalVersion.toString(), getProject().getModel() ) )
            {
                getLog().info( "Updated " + toString( dep ) + " to version " + finalVersion.toString() );
            }
        }
        else
        {
            getLog().info( "No matching release of " + toString( dep ) + " to update via rangeMatching." );
            if ( failIfNotReplaced )
            {
                throw new NoSuchElementException( "No matching release of " + toString( dep )
                    + " found for update via rangeMatching." );
            }
        }
    }

    private void noRangeMatching( ModifiedPomXMLEventReader pom, Dependency dep, String version, String releaseVersion,
                                  ArtifactVersions versions )
        throws XMLStreamException
    {
        if ( versions.containsVersion( releaseVersion ) )
        {
            if ( PomHelper.setDependencyVersion( pom, dep.getGroupId(), dep.getArtifactId(), version, releaseVersion,
                                                 getProject().getModel() ) )
            {
                getLog().info( "Updated " + toString( dep ) + " to version " + releaseVersion );
            }
        }
        else if ( failIfNotReplaced )
        {
            throw new NoSuchElementException( "No matching release of " + toString( dep ) + " found for update." );
        }
    }

    /**
     * Return an array which contains the intersection of the 2 given arrays, i.e. an array which contains the {@link ArtifactVersions} present in the 2 given arrays.
     *
     * @param versions1
     * @param versions2
     * @return
     */
    private ArtifactVersion[] intersection( ArtifactVersion[] versions1, ArtifactVersion[] versions2 )
    {
        Set<ArtifactVersion> s1 = new HashSet<>( Arrays.asList( versions1 ) );
        Set<ArtifactVersion> s2 = new HashSet<>( Arrays.asList( versions2 ) );
        s1.retainAll( s2 );

        return s1.toArray( new ArtifactVersion[s1.size()] );
    }

    /**
     * Compare an artifact to a dependency. Returns true only if the groupId, artifactId, type, and classifier are all
     * equal.
     *
     * @param artifact
     * @param dep
     * @return true if artifact and dep refer to the same artifact
     */
    private boolean isSame( Artifact artifact, Dependency dep )
    {
        if ( !StringUtils.equals( artifact.getGroupId(), dep.getGroupId() ) )
        {
            return false;
        }
        if ( !StringUtils.equals( artifact.getArtifactId(), dep.getArtifactId() ) )
        {
            return false;
        }
        if ( !StringUtils.equals( artifact.getType(), dep.getType() ) )
        {
            return false;
        }
        if ( !StringUtils.equals( artifact.getClassifier(), dep.getClassifier() ) )
        {
            return false;
        }
        return true;
    }

    /**
     * Return an {@link ArtifactVersions} from the given one merged with the versions of the artifact produced by the reactor.
     *
     * @param artifactVersions
     * @return
     * @throws MojoExecutionException
     */
    private ArtifactVersions mergeReactorArtifactsVersions( ArtifactVersions artifactVersions ) throws MojoExecutionException
    {
        Set<ArtifactVersion> mergedArtifactVersions = new HashSet<>( Arrays.asList( artifactVersions.getVersions() ) );
        mergedArtifactVersions.addAll( getReactorArtifactsVersions() );
        return new ArtifactVersions( artifactVersions.getArtifact(), new ArrayList<>( mergedArtifactVersions ), artifactVersions.getVersionComparator() );
    }

    /**
     * Return a {@link Set} of {@link ArtifactVersion} that are produced by the reactor.
     *
     * @return
     * @throws MojoExecutionException
     */
    private Set<ArtifactVersion> getReactorArtifactsVersions() throws MojoExecutionException
    {
        Set<ArtifactVersion> versions = new HashSet<>();
        for ( Artifact artifact : getReactorArtifacts() )
        {
            versions.add( getHelper().createArtifactVersion( artifact.getVersion() ) );
        }
        return versions;
    }

    /**
     * Return a {@link Set} of {@link Artifact} that are produced by the reactor.
     *
     * @return
     * @throws MojoExecutionException
     */
    @SuppressWarnings( "unchecked" )
    private Set<Artifact> getReactorArtifacts() throws MojoExecutionException
    {
        return getHelper().extractArtifacts( reactorProjects );
    }

}
