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
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Collection;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    extends AbstractVersionsDependencyUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * Pattern to match a version range. For example 1.0-20090128.202731-1
     */
    public final Pattern matchRangeRegex = Pattern.compile( "," );

    // ------------------------------ METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        // Note we have to get the dependencies from the model because the dependencies in the 
        // project may have already had their range resolved [MNG-4138]
        if ( getProject().getModel().getDependencyManagement() != null
            && getProject().getModel().getDependencyManagement().getDependencies() != null
            && isProcessingDependencyManagement() )
        {
            resolveRanges( pom, getProject().getModel().getDependencyManagement().getDependencies() );
        }
        if ( isProcessingDependencies() )
        {
            resolveRanges( pom, getProject().getModel().getDependencies() );
        }
    }

    private void resolveRanges( ModifiedPomXMLEventReader pom, Collection dependencies )
        throws XMLStreamException, MojoExecutionException
    {

        Iterator iter = dependencies.iterator();

        while ( iter.hasNext() )
        {
            Dependency dep = (Dependency) iter.next();

            if ( isExcludeReactor() && isProducedByReactor( dep ) )
            {
                continue;
            }

            Matcher versionMatcher = matchRangeRegex.matcher( dep.getVersion() );

            if ( versionMatcher.find() )
            {
                Artifact artifact = this.findArtifact( dep );

                if ( artifact != null && isIncluded( artifact ) )
                {
                    getLog().debug( "Resolving version range for dependency: " + artifact );

                    if ( PomHelper.setDependencyVersion( pom, artifact.getGroupId(), artifact.getArtifactId(),
                                                         dep.getVersion(), artifact.getVersion() ) )
                    {
                        getLog().debug( "Version set to " + artifact.getVersion() + " for dependency: " + artifact );
                    }
                }
            }
        }
    }

}
