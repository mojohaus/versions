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

import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

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
     * A comma separated list of artifact patterns to include. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifing the set of includes from the command
     * line. When specifying includes from the pom, use the {@link #includes} configuration instead. If this property is
     * specified then the {@link # include} configuration is ignored.
     *
     * @parameter expression="${includes}"
     * @since 1.0-beta-1
     */
    private String includesList = null;

    /**
     * A comma separated list of artifact patterns to exclude. Follows the pattern
     * "groupId:artifactId:type:classifier:version". Designed to allow specifing the set of excludes from the command
     * line. When specifying excludes from the pom, use the {@link #excludes} configuration instead. If this property is
     * specified then the {@link # exclude} configuration is ignored.
     *
     * @parameter expression="${excludes}"
     * @since 1.0-beta-1
     */
    private String excludesList = null;

    /**
     * A list of artifact patterns to include. Follows the pattern
     * "groupId:artifactId:type:classifier:version". This configuration setting is ignored if {@link #includesList} is
     * defined.
     *
     * @parameter
     * @since 1.0-beta-1
     */
    private String[] includes = null;

    /**
     * A list of artifact patterns to exclude. Follows the pattern
     * "groupId:artifactId:type:classifier:version". This configuration setting is ignored if {@link #excludesList} is 
     * defined.
     *
     * @parameter
     * @since 1.0-beta-1
     */
    private String[] excludes = null;

    /**
     * Whether to process the dependencies section of the project. If not
     * set will default to true.
     *
     * @parameter expression="${processDependencies}" defaultValue="true"
     * @since 1.0-alpha-3
     */
    private Boolean processDependencies;

    /**
     * Whether to process the dependencyManagement section of the project. If not
     * set will default to true.
     *
     * @parameter expression="${processDependencyManagement}" defaultValue="true"
     * @since 1.0-alpha-3
     */
    private Boolean processDependencyManagement;

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
     * @parameter expression="${excludeReactor}" defaultValue="true"
     * @since 1.0-alpha-3
     */
    private Boolean excludeReactor;

    /**
     * Should the project/dependencies section of the pom be processed.
     *
     * @return returns <code>true if the project/dependencies section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencies()
    {
        // true if true or null
        return !Boolean.FALSE.equals( processDependencies );
    }

    /**
     * Should the project/dependencyManagement section of the pom be processed.
     *
     * @return returns <code>true if the project/dependencyManagement section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencyManagement()
    {
        // true if true or null
        return !Boolean.FALSE.equals( processDependencyManagement );
    }

    /**
     * Should the artifacts produced in the current reactor be excluded from processing.
     *
     * @return returns <code>true if the artifacts produced in the current reactor should be excluded from processing.
     * @since 1.0-alpha-3
     */
    public boolean isExcludeReactor()
    {
        // true if true or null
        return !Boolean.FALSE.equals( excludeReactor );
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency
     * @return
     * @since 1.0-alpha-3
     */
    protected Artifact findArtifact( Dependency dependency )
    {
        if ( getProject().getDependencyArtifacts() == null )
        {
            return null;
        }
        Iterator iter = getProject().getDependencyArtifacts().iterator();
        while ( iter.hasNext() )
        {
            Artifact artifact = (Artifact) iter.next();
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
     * @param dependency
     * @return
     * @since 1.0-alpha-3
     */
    protected Artifact toArtifact( Dependency dependency )
        throws MojoExecutionException
    {
        Artifact artifact = findArtifact( dependency );
        if ( artifact == null )
        {
            try
            {
                return getHelper().createDependencyArtifact( dependency );
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
        }
        return artifact;
    }

    protected String toString( Dependency d )
    {
        StringBuffer buf = new StringBuffer();
        buf.append( d.getGroupId() );
        buf.append( ':' );
        buf.append( d.getArtifactId() );
        if ( d.getType() != null && d.getType().length() > 0 )
        {
            buf.append( ':' );
            buf.append( d.getType() );
            if ( d.getType() != null && d.getType().length() > 0 )
            {
                buf.append( ':' );
                buf.append( d.getClassifier() );
            }
        }
        else
        {
            if ( d.getClassifier() != null && d.getClassifier().length() > 0 )
            {
                buf.append( ":jar:" );
                buf.append( d.getClassifier() );
            }
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
        Iterator iter = reactorProjects.iterator();
        while ( iter.hasNext() )
        {
            MavenProject project = (MavenProject) iter.next();
            if ( compare( project, dependency ) )
            {
                return true;
            }
        }
        return false;

    }

    /**
     * Compare a project to a dependency. Returns true only if the groupId and artifactId are all
     * equal.
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
        if ( !StringUtils.equals( project.getArtifactId(), dep.getArtifactId() ) )
        {
            return false;
        }
        return true;
    }

    /**
     * Compare and artifact to a dependency. Returns true only if the groupId, artifactId, type, and classifier are all
     * equal.
     *
     * @param artifact
     * @param dep
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
        if ( !StringUtils.equals( artifact.getClassifier(), dep.getClassifier() ) )
        {
            return false;
        }
        return true;
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

    private ArtifactFilter getIncludesArtifactFilter()
    {
        if ( includesFilter == null && ( includes != null || includesList != null ) )
        {
            List patterns = new ArrayList();
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
            List patterns = new ArrayList();
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
     * To handle multiple includes with version range like "group:artifact:jar:[1.0.0,2.2)",
     * we have to use a parsing a little bit more complex than split().
     *
     * @param includeString the string to parse
     * @return list of patterns
     */
    protected List separatePatterns( String includeString )
    {
        if ( includeString == null )
        {
            return Collections.EMPTY_LIST;
        }

        List patterns = new ArrayList();
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
        for ( int i = 0; i < delimiters.length; i++ )
        {
            int index = includeString.indexOf( delimiters[i] );
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

}
