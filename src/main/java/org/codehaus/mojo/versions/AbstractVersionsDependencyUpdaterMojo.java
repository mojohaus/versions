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
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.model.Dependency;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.commons.lang.StringUtils;

import java.util.Iterator;
import java.util.List;
import java.util.Arrays;

/**
 * Base class for a mojo that updates dependency versions.
 *
 * @author Paul Gier
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public abstract class AbstractVersionsDependencyUpdaterMojo extends AbstractVersionsUpdaterMojo {

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
     * Try to find the dependency artifact that matches the given dependency.
     *
     * @param dependency
     * @return
     */
    protected Artifact findArtifact( Dependency dependency )
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
            result = excludesFilter.include( artifact );
        }

        return result;

    }

    private ArtifactFilter getIncludesArtifactFilter()
    {
        if ( this.includesFilter == null && this.includes != null )
        {
            List patterns = Arrays.asList( includes.split( "," ) );
            this.includesFilter = new PatternIncludesArtifactFilter( patterns );
        }
        return this.includesFilter;
    }

    private ArtifactFilter getExcludesArtifactFilter()
    {
        if ( this.excludesFilter == null && this.excludes != null )
        {
            List patterns = Arrays.asList( excludes.split( "," ) );
            this.excludesFilter = new PatternExcludesArtifactFilter( patterns );
        }
        return this.excludesFilter;
    }
}
