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
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLStreamException;

/**
 * Attempts to resolve dependency version ranges to the specific version being used in the build. For example a version
 * range of "[1.0, 1.2)" would be resolved to the specific version currently in use "1.1".
 * 
 * @author Paul Gier
 * @goal resolve-ranges
 * @requiresProject true
 * @requiresDirectInvocation true
 * @requiresDependencyResolution test
 * @since 1.0-alpha-3
 */
public class ResolveRangesMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * A comma separated list of artifact patterns to include. Follows the pattern
     * "groupId:artifactId:type:classifier:version".
     * 
     * @parameter expression="${includes}"
     * @since 1.0-alpha-3
     */
    private String includes = null;

    /**
     * A comma separated list of artifact patterns to exclude. Follows the pattern
     * "groupId:artifactId:type:classifier:version".
     * 
     * @parameter expression="${excludes}"
     * @since 1.0-alpha-3
     */
    private String excludes = null;

    /**
     * Artifact filter to determine if artifact should be included
     */
    private PatternIncludesArtifactFilter includesFilter;

    /**
     * Artifact filter to determine if artifact should be excluded
     */
    private PatternExcludesArtifactFilter excludesFilter;

    /**
     * Pattern to match a version range. For example 1.0-20090128.202731-1
     */
    public final Pattern matchRangeRegex = Pattern.compile( "," );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException when things go wrong in a very bad way
     * @throws XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // Note we have to get the dependencies from the model because the dependencies in the 
        // project may have already had their range resolved [MNG-4138]
        resolveRanges( pom, getProject().getModel().getDependencies() );
    }

    private void resolveRanges( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException
    {

        Iterator iter = dependencies.iterator();

        while ( iter.hasNext() )
        {
            Dependency dep = (Dependency) iter.next();

            Matcher versionMatcher = matchRangeRegex.matcher( dep.getVersion() );
            
            if ( versionMatcher.find() )
            {
                Artifact artifact = this.findArtifact( dep );
                
                if ( isIncluded( artifact ) )
                {
                    if ( PomHelper.setDependencyVersion( pom, artifact.getGroupId(), artifact.getArtifactId(),
                                                         dep.getVersion(), artifact.getVersion() ) )
                    {
                        getLog().debug( "Version set to " + artifact.getVersion() + " for dependency: " + artifact );
                    }
                }
            }
        }
    }

    /**
     * Try to find the dependency artifact that matches the given dependency.
     * 
     * @param dependency
     * @return
     */
    private Artifact findArtifact( Dependency dependency )
    {
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
     * Determine if the artifact is included in the list of artifacts to be resolved.
     * 
     * @param artifact
     * @return true if the artifact version should be resolved, false otherwise
     */
    private boolean isIncluded( Artifact artifact )
    {
        getLog().debug( "Resolving version range for dependency: " + artifact );

        boolean result = true;
        
        ArtifactFilter includesFilter = this.getIncludesArtifactFilter();
        
        if ( includesFilter != null )
        {
            result = includesFilter.include( artifact );
        }
        
        ArtifactFilter excludesFilter = this.getExcludesArtifactFilter();
        
        if ( excludesFilter != null )
        {
            result = excludesFilter.include( artifact );
        }
        
        return result;

    }

    private ArtifactFilter getIncludesArtifactFilter()
    {
        if ( this.includesFilter == null &&  this.includes != null )
        {
            List patterns = Arrays.asList( includes.split( "," ) );
            this.includesFilter = new PatternIncludesArtifactFilter( patterns );
        }
        return this.includesFilter;
    }

    private ArtifactFilter getExcludesArtifactFilter()
    {
        if ( this.excludesFilter == null &&  this.excludes != null )
        {
            List patterns = Arrays.asList( excludes.split( "," ) );
            this.excludesFilter = new PatternExcludesArtifactFilter( patterns );
        }
        return this.excludesFilter;
    }
}
