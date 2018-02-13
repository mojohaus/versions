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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.artifact.filter.PatternExcludesArtifactFilter;
import org.apache.maven.shared.artifact.filter.PatternIncludesArtifactFilter;

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

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "processDependencies", defaultValue = "true" )
    private boolean processDependencies;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    private boolean processDependencyManagement;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter( property = "processParent", defaultValue = "false" )
    private boolean processParent = false;

    /**
     * Whether to skip processing dependencies that are produced as part of the current reactor.
     *
     * @since 1.0-alpha-3
     */
    @Parameter( property = "excludeReactor", defaultValue = "true" )
    private boolean excludeReactor;

    /**
     * Should the project/dependencies section of the pom be processed.
     *
     * @return returns <code>true if the project/dependencies section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencies()
    {
        return processDependencies;
    }

    /**
     * Should the project/dependencyManagement section of the pom be processed.
     *
     * @return returns <code>true if the project/dependencyManagement section of the pom should be processed.
     * @since 1.0-alpha-3
     */
    public boolean isProcessingDependencyManagement()
    {
        return processDependencyManagement;
    }

    /**
     * Should the project/parent section of the pom be processed.
     *
     * @return returns <code>true if the project/parent section of the pom should be processed.
     * @since 2.3
     */
    public boolean isProcessingParent()
    {
        return processParent;
    }

    /**
     * Should the artifacts produced in the current reactor be excluded from processing.
     *
     * @return returns <code>true if the artifacts produced in the current reactor should be excluded from processing.
     * @since 1.0-alpha-3
     */
    public boolean isExcludeReactor()
    {
        return excludeReactor;
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
        Iterator<?> iter = reactorProjects.iterator();
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
     * Compare a project to a dependency. Returns true only if the groupId and artifactId are all equal.
     *
     * @param project the project
     * @param dep the dependency
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
}
