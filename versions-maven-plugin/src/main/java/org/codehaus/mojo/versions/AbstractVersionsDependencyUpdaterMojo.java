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

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.utils.DependencyComparator;

/**
 * Base class for a mojo that updates dependency versions.
 *
 * @author Paul Gier
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public abstract class AbstractVersionsDependencyUpdaterMojo
    extends AbstractVersionsUpdaterMojo
{
    private static final String END_RANGE_CHARS = "])";

    private static final String START_RANGE_CHARS = "[(";

    /**
     * Pattern to match snapshot versions
     */
    protected static final Pattern SNAPSHOT_REGEX = Pattern.compile( "^(.+)-((SNAPSHOT)|(\\d{8}\\.\\d{6}-\\d+))$" );


    /**
     * A comma separated list of artifact patterns to include. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifying the set of includes from the command
     * line. When specifying includes from the pom, use the {@link #includes} configuration instead. If this property is
     * specified then the {@link # include} configuration is ignored.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "includes" )
    private String includesList = null;

    /**
     * A comma separated list of artifact patterns to exclude. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifying the set of excludes from the command
     * line. When specifying excludes from the pom, use the {@link #excludes} configuration instead. If this property is
     * specified then the {@link # exclude} configuration is ignored.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "excludes" )
    private String excludesList = null;

    /**
     * A list of artifact patterns to include. Follows the pattern "groupId:artifactId:type:classifier:version". This
     * configuration setting is ignored if {@link #includesList} is defined.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private String[] includes = null;

    /**
     * A list of artifact patterns to exclude. Follows the pattern "groupId:artifactId:type:classifier:version". This
     * configuration setting is ignored if {@link #excludesList} is defined.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private String[] excludes = null;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "processDependencies", defaultValue = "true" )
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter( property = "processParent", defaultValue = "false" )
    private boolean processParent = false;

    /**
     * Artifact filter to determine if artifact should be included
     *
     * @since 1.0-alpha-3
     */
    private PatternIncludesArtifactFilter includesFilter;

    /**
     * Artifact filter to determine if artifact should be excluded
     *
     * @since 1.0-alpha-3
     */
    private PatternExcludesArtifactFilter excludesFilter;

    /**
     * Whether to skip processing dependencies that are produced as part of the current reactor.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "excludeReactor", defaultValue = "true" )
    private boolean excludeReactor = true;

    @Inject
    protected AbstractVersionsDependencyUpdaterMojo( RepositorySystem repositorySystem,
                                           MavenProjectBuilder projectBuilder,
                                           ArtifactMetadataSource artifactMetadataSource,
                                           WagonManager wagonManager,
                                           ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

    /**
     * Should the project/dependencies section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/dependencies section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencies()
    {
        return processDependencies;
    }

    /**
     * Should the project/dependencyManagement section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/dependencyManagement section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencyManagement()
    {
        return processDependencyManagement;
    }

    /**
     * Should the project/parent section of the pom be processed.
     *
     * @return returns <code>true</code> if the project/parent section of the pom should be processed.
     * @since 2.3
     */
    public boolean isProcessingParent()
    {
        return processParent;
    }

    /**
     * Should the artifacts produced in the current reactor be excluded from processing.
     *
     * @return returns <code>true</code> if the artifacts produced in the current reactor
     * should be excluded from processing.
     * @since 1.0-alpha-3
     */
    public boolean isExcludeReactor()
    {
        return excludeReactor;
    }

    /**
     * Should the dependency be updated itself or is it handled by properties.
     *
     * @param dependency Dependency
     * @return true if the version starts with '${'
     * @since 2.8
     */
    protected boolean isHandledByProperty( Dependency dependency )
    {
        String version = dependency.getVersion();
        return version != null && version.startsWith( "${" );
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency Dependency
     * @return Artifact
     * @since 1.0-alpha-3
     */
    protected Artifact findArtifact( Dependency dependency )
    {
        if ( getProject().getDependencyArtifacts() == null )
        {
            return null;
        }
        for ( Artifact artifact : getProject().getDependencyArtifacts() )
        {
            if ( compare( artifact, dependency ) )
            {
                return artifact;
            }
        }
        return null;
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency Dependency
     * @return Artifact
     * @throws MojoExecutionException Mojo execution exception
     * @since 1.0-alpha-3
     */
    protected Artifact toArtifact( Dependency dependency )
        throws MojoExecutionException
    {
        Artifact artifact = findArtifact( dependency );
        if ( artifact == null )
        {
            return getHelper().createDependencyArtifact( dependency );
        }
        return artifact;
    }

    protected Artifact toArtifact( Parent model )
        throws MojoExecutionException
    {
        return this.toArtifact( DependencyBuilder.newBuilder()
                .withGroupId( model.getGroupId() )
                .withArtifactId( model.getArtifactId() )
                .withVersion( model.getVersion() )
                .withType( "pom" )
                .withScope( Artifact.SCOPE_COMPILE )
                .build() );
    }

    /**
     * Returns the {@link Dependency} instance for the parent project
     * @return {@link Dependency} object for the parent
     */
    protected Dependency getParentDependency()
    {
        return DependencyBuilder.newBuilder()
                .withGroupId( getProject().getParent().getGroupId() )
                .withArtifactId( getProject().getParent().getArtifactId() )
                .withVersion( getProject().getParent().getVersion() )
                .withType( "pom" )
                .build();
    }

    protected String toString( MavenProject project )
    {
        StringBuilder buf = new StringBuilder();

        buf.append( project.getGroupId() );
        buf.append( ':' );
        buf.append( project.getArtifactId() );

        if ( project.getVersion() != null && project.getVersion().length() > 0 )
        {
            buf.append( ":" );
            buf.append( project.getVersion() );
        }

        return buf.toString();
    }

    protected String toString( Dependency d )
    {
        StringBuilder buf = new StringBuilder();
        buf.append( d.getGroupId() );
        buf.append( ':' );
        buf.append( d.getArtifactId() );
        if ( d.getType() != null && d.getType().length() > 0 )
        {
            buf.append( ':' );
            buf.append( d.getType() );
        }
        else
        {
            buf.append( ":jar" );
        }
        if ( d.getClassifier() != null && d.getClassifier().length() > 0 )
        {
            buf.append( ':' );
            buf.append( d.getClassifier() );
        }
        if ( d.getVersion() != null && d.getVersion().length() > 0 )
        {
            buf.append( ":" );
            buf.append( d.getVersion() );
        }
        return buf.toString();
    }

    /**
     * Returns <code>true</code> if the dependency is produced by the current reactor.
     *
     * @param dependency the dependency to heck.
     * @return <code>true</code> if the dependency is produced by the current reactor.
     * @since 1.0-alpha-3
     */
    protected boolean isProducedByReactor( Dependency dependency )
    {
        return reactorProjects.stream()
                .anyMatch( reactorProject -> compare( reactorProject, dependency ) );
    }

    /**
     * Compare a project to a dependency. Returns true only if the groupId and artifactId are all equal.
     *
     * @param project the project
     * @param dep     the dependency
     * @return true if project and dep refer to the same artifact
     */
    private boolean compare( MavenProject project, Dependency dep )
    {
        if ( !StringUtils.equals( project.getGroupId(), dep.getGroupId() ) )
        {
            return false;
        }
        return project.getArtifactId().equals( dep.getArtifactId() );
    }

    /**
     * Compare and artifact to a dependency. Returns true only if the groupId, artifactId, type, and classifier are all
     * equal.
     *
     * @param artifact Artifact
     * @param dep      Dependency
     * @return true if artifact and dep refer to the same artifact
     */
    private boolean compare( Artifact artifact, Dependency dep )
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
        return StringUtils.equals( artifact.getClassifier(), dep.getClassifier() );
    }

    /**
     * Determine if the artifact is included in the list of artifacts to be processed.
     *
     * @param artifact The artifact we want to check.
     * @return true if the artifact should be processed, false otherwise.
     */
    protected boolean isIncluded( Artifact artifact )
    {
        boolean result = true;

        ArtifactFilter includesFilter = this.getIncludesArtifactFilter();

        if ( includesFilter != null )
        {
            result = includesFilter.include( artifact );
        }

        ArtifactFilter excludesFilter = this.getExcludesArtifactFilter();

        if ( excludesFilter != null )
        {
            result = result && excludesFilter.include( artifact );
        }

        return result;
    }

    /**
     * Indicates whether any includes were specified via the 'includes' or 'includesList' options.
     *
     * @return true if includes were specified, false otherwise.
     */
    protected boolean hasIncludes()
    {
        return includes != null || includesList != null;
    }

    private ArtifactFilter getIncludesArtifactFilter()
    {
        if ( includesFilter == null && ( includes != null || includesList != null ) )
        {
            List<String> patterns = new ArrayList<>();
            if ( this.includesList != null )
            {
                patterns.addAll( separatePatterns( includesList ) );
            }
            else if ( includes != null )
            {
                patterns.addAll( Arrays.asList( includes ) );
            }
            includesFilter = new PatternIncludesArtifactFilter( patterns );
        }
        return includesFilter;
    }

    private ArtifactFilter getExcludesArtifactFilter()
    {
        if ( excludesFilter == null && ( excludes != null || excludesList != null ) )
        {
            List<String> patterns = new ArrayList<>();
            if ( excludesList != null )
            {
                patterns.addAll( separatePatterns( excludesList ) );
            }
            else if ( excludes != null )
            {
                patterns.addAll( Arrays.asList( excludes ) );
            }
            excludesFilter = new PatternExcludesArtifactFilter( patterns );
        }
        return excludesFilter;
    }

    /**
     * To handle multiple includes with version range like "group:artifact:jar:[1.0.0,2.2)", we have to use a parsing a
     * little bit more complex than split().
     *
     * @param includeString the string to parse
     * @return list of patterns
     */
    protected List<String> separatePatterns( String includeString )
    {
        if ( includeString == null )
        {
            return Collections.emptyList();
        }

        List<String> patterns = new ArrayList<>();
        int indexOf = nextCommaIndex( includeString );
        while ( indexOf >= 0 )
        {
            patterns.add( includeString.substring( 0, indexOf ) );
            includeString = includeString.substring( indexOf + 1 );
            indexOf = nextCommaIndex( includeString );
        }
        patterns.add( includeString );

        return patterns;
    }

    private int nextCommaIndex( final String includeString )
    {

        int indexOfComma = includeString.indexOf( ',' );
        int nextRangeStartDelimiterIndex = findFirstChar( includeString, START_RANGE_CHARS );
        if ( nextRangeStartDelimiterIndex >= 0 )
        {
            if ( !( indexOfComma >= 0 && indexOfComma < nextRangeStartDelimiterIndex ) )
            {
                int nextStopDelimiterIndex = findFirstChar( includeString, END_RANGE_CHARS );

                // recursive call
                int tmp = nextCommaIndex( includeString.substring( nextStopDelimiterIndex + 1 ) );
                indexOfComma = ( tmp >= 0 ) ? nextStopDelimiterIndex + 1 + tmp : -1;
            }
        }
        return indexOfComma;

    }

    private int findFirstChar( final String includeString, final String chars )
    {
        int nextRangeStartDelimiterIndex = -1;

        char[] delimiters = chars.toCharArray();
        for ( char delimiter : delimiters )
        {
            int index = includeString.indexOf( delimiter );
            if ( index >= 0 && nextRangeStartDelimiterIndex >= 0 )
            {
                nextRangeStartDelimiterIndex = Math.min( index, nextRangeStartDelimiterIndex );
            }
            else
            {
                if ( index >= 0 )
                {
                    nextRangeStartDelimiterIndex = index;
                }
            }
        }
        return nextRangeStartDelimiterIndex;
    }

    /**
     * Attempts to update the dependency {@code dep} to the given {@code newVersion}. The dependency can either
     * be the parent project or any given dependency.
     *
     * @param pom {@link ModifiedPomXMLEventReader} instance to update the POM XML document
     * @param dep dependency to be updated (can also be a dependency made from the parent)
     * @param newVersion new version to update the dependency to
     * @param changeRecorderTitle title for the {@link ChangeRecorder} log
     * @return {@code true} if an update has been made, {@code false} otherwise
     * @throws XMLStreamException thrown if updating the XML doesn't succeed
     */
    protected boolean updateDependencyVersion( ModifiedPomXMLEventReader pom, Dependency dep,
                                               String newVersion, String changeRecorderTitle )
            throws XMLStreamException
    {
        boolean updated = false;
        if ( isProcessingParent()
                && getProject().getParent() != null
                && DependencyComparator.INSTANCE.compare( dep, DependencyBuilder.newBuilder()
                        .withGroupId( getProject().getParentArtifact().getGroupId() )
                        .withArtifactId( getProject().getParentArtifact().getArtifactId() )
                        .withVersion( getProject().getParentArtifact().getVersion() )
                        .build() ) == 0
                && PomHelper.setProjectParentVersion( pom, newVersion ) )
        {
            if ( getLog().isDebugEnabled() )
            {
                getLog().debug( "Made parent update from " + dep.getVersion() + " to " + newVersion );
            }
            getChangeRecorder().recordUpdate( changeRecorderTitle,
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion(),
                    newVersion );
            updated = true;
        }

        if ( PomHelper.setDependencyVersion( pom,
                dep.getGroupId(), dep.getArtifactId(), dep.getVersion(),
                newVersion, getProject().getModel() ) )
        {
            if ( getLog().isInfoEnabled() )
            {
                getLog().info( "Updated " + toString( dep ) + " to version " + newVersion );
            }
            getChangeRecorder().recordUpdate( changeRecorderTitle,
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion(),
                    newVersion );
            updated = true;
        }

        return updated;
    }

    // TODO: add an updatePropertyVersion as well??? (like in CompareDependenciesMojo)
}
