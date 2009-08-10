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

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

import java.util.*;

/**
 * Generates a report of available updates for the dependencies of a project.
 *
 * @author Stephen Connolly
 * @goal dependency-updates-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 1.0-beta-1
 */
public class DependencyUpdatesReport
    extends AbstractVersionsReport
{
    /**
     * {@inheritDoc}
     */
    public boolean isExternalReport()
    {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public boolean canGenerateReport()
    {
        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        Set dependencyManagement = new TreeSet( new DependencyComparator() );
        dependencyManagement.addAll( getProject().getDependencyManagement() == null
            ? Collections.EMPTY_LIST
            : getProject().getDependencyManagement().getDependencies() );

        Set dependencies = new TreeSet( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );
        dependencies = removeDependencyManagment( dependencies, dependencyManagement );

        try
        {
            Map/*<Dependency,DependencyUpdateDetails>*/ dependencyUpdates =
                getHelper().lookupDependenciesUpdates( dependencies, false );
            Map/*<Dependency,DependencyUpdateDetails>*/ dependencyManagementUpdates =
                getHelper().lookupDependenciesUpdates( dependencyManagement, false );
            DependencyUpdatesRenderer renderer =
                new DependencyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, dependencyUpdates,
                                               dependencyManagementUpdates );
            renderer.render();
        }
        catch ( InvalidVersionSpecificationException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param dependencies         The set of dependencies.
     * @param dependencyManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set removeDependencyManagment( Set dependencies, Set dependencyManagement )
    {
        Set result = new TreeSet( new DependencyComparator() );
        for ( Iterator i = dependencies.iterator(); i.hasNext(); )
        {
            Dependency c = (Dependency) i.next();
            boolean matched = false;
            Iterator j = dependencyManagement.iterator();
            while ( !matched && j.hasNext() )
            {
                Dependency t = (Dependency) j.next();
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() )
                    && ( t.getScope() == null || StringUtils.equals( t.getScope(), c.getScope() ) )
                    && ( t.getClassifier() == null || StringUtils.equals( t.getClassifier(), c.getClassifier() ) ) && (
                    c.getVersion() == null || t.getVersion() == null || StringUtils.equals( t.getVersion(),
                                                                                            c.getVersion() ) ) )
                {
                    matched = true;
                    break;
                }
            }
            if ( !matched )
            {
                result.add( c );
            }
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "dependency-updates-report";
    }

}