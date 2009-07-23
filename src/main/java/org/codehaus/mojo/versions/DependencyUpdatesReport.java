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

import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Created by IntelliJ IDEA.
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
    public boolean isExternalReport()
    {
        return false;
    }

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

        Map/*<Dependency,DependencyUpdateDetails>*/ dependencyUpdates = lookupDependenciesUpdates( dependencies );
        Map/*<Dependency,DependencyUpdateDetails>*/ dependencyManagementUpdates =
            lookupDependenciesUpdates( dependencyManagement );
        DependencyUpdatesRenderer renderer =
            new DependencyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, dependencyUpdates,
                                           dependencyManagementUpdates );
        renderer.render();
    }

    private Set removeDependencyManagment( Set dependencies, Set dependencyManagement )
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

    public String getOutputName()
    {
        return "dependency-updates-report";
    }

}