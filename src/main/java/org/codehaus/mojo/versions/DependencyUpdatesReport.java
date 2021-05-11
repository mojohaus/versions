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

import java.io.File;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.util.StringUtils;

/**
 * Generates a report of available updates for the dependencies of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "dependency-updates-report", requiresProject = true, requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true)
public class DependencyUpdatesReport
    extends AbstractVersionsReport
{

    /**
     * Whether to process the <code>dependencyManagement</code> in pom or not.
     * 
     * @since 2.5
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    private boolean processDependencyManagement;

    /**
     * Whether to process the depdendencyManagement part transitive or not.
     * In case of <code>&lt;type&gt;pom&lt;/type&gt;</code>and
     * <code>&lt;scope&gt;import&lt;/scope&gt;</code> this means
     * by default to report also the imported dependencies. 
     * If processTransitive is set to <code>false</code> the report will only show
     * updates of the imported pom it self.
     * 
     * @since 2.5 Note: Currently in experimental state.
     */
    @Parameter( property = "processDependencyManagementTransitive", defaultValue = "true" )
    private boolean processDependencyManagementTransitive;

    /**
     * Report formats (html and/or xml). HTML by default.
     * 
     */
    @Parameter( property = "dependencyUpdatesReportFormats", defaultValue = "html" )
    private String[] formats = new String[] { "html" };

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
     * @param sink the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        Set<Dependency> dependencies = new TreeSet<>( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );

        Set<Dependency> dependencyManagement = new TreeSet<>( new DependencyComparator() );

        if ( processDependencyManagementTransitive )
        {
            if ( getProject().getDependencyManagement() != null
                && getProject().getDependencyManagement().getDependencies() != null )
            {
                for ( Dependency dep : getProject().getDependencyManagement().getDependencies() )
                {
                    getLog().debug( "Dpmg: " + dep.getGroupId() + ":" + dep.getArtifactId() + ":" + dep.getVersion()
                        + ":" + dep.getType() + ":" + dep.getScope() );
                }
                dependencyManagement.addAll( getProject().getDependencyManagement().getDependencies() );
            }
        }
        else
        {
            if ( getProject().getOriginalModel().getDependencyManagement() != null
                && getProject().getOriginalModel().getDependencyManagement().getDependencies() != null )
            {
                // Using the original model to get the original dependencyManagement entries and
                // not the interpolated model.
                // TODO: I'm not 100% sure if this will work correctly in all cases.

                // Grab the project properties
                Properties props = getProject().getProperties();
                for ( Dependency dep : getProject().getOriginalModel().getDependencyManagement().getDependencies() )
                {
                    String version = dep.getVersion();
                    getLog().debug( "Original Dpmg: " + dep.getGroupId() + ":" + dep.getArtifactId() + ":"
                        + version + ":" + dep.getType() + ":" + dep.getScope() );
                    // if the version is referencing a property, dereference it
                    if ( version.startsWith( "${" ) )
                    {
                        String propVersion = props.getProperty( version.substring( 2, version.length() - 1 ) );
                        if ( propVersion != null )
                        {
                            dep.setVersion( propVersion );
                        }
                    }

                }
                dependencyManagement.addAll( getProject().getOriginalModel().getDependencyManagement().getDependencies() );
            }
        }

        if ( processDependencyManagement )
        {
            dependencies = removeDependencyManagment( dependencies, dependencyManagement );
        }

        try
        {
            Map<Dependency, ArtifactVersions> dependencyUpdates =
                getHelper().lookupDependenciesUpdates( dependencies, false );

            Map<Dependency, ArtifactVersions> dependencyManagementUpdates = Collections.emptyMap();
            if ( processDependencyManagement )
            {
                dependencyManagementUpdates = getHelper().lookupDependenciesUpdates( dependencyManagement, false );
            }
            for ( String format : formats )
            {
                if ( "html".equals( format ) )
                {
                    DependencyUpdatesRenderer renderer =
                        new DependencyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, dependencyUpdates,
                                                       dependencyManagementUpdates );
                    renderer.render();

                }
                else if ( "xml".equals( format ) )
                {
                   File outputDir = new File(getProject().getBuild().getDirectory());
                   if (!outputDir.exists())
                    {
                        outputDir.mkdirs();
                    }
                    String outputFile =
                        outputDir.getAbsolutePath() + File.separator + getOutputName() + ".xml";
                    DependencyUpdatesXmlRenderer xmlGenerator =
                        new DependencyUpdatesXmlRenderer( dependencyUpdates, dependencyManagementUpdates, outputFile );
                    xmlGenerator.render();
                }
            }
        }
        catch ( InvalidVersionSpecificationException| ArtifactMetadataRetrievalException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * Returns a set of dependencies where the dependencies which are defined in the dependency management section have
     * been filtered out.
     *
     * @param dependencies The set of dependencies.
     * @param dependencyManagement The set of dependencies from the dependency management section.
     * @return A new set of dependencies which are from the set of dependencies but not from the set of dependency
     *         management dependencies.
     * @since 1.0-beta-1
     */
    private static Set<Dependency> removeDependencyManagment( Set<Dependency> dependencies, Set<Dependency> dependencyManagement )
    {
        Set<Dependency> result = new TreeSet<>( new DependencyComparator() );
        for ( Dependency c : dependencies )
        {
            boolean matched = false;
            Iterator<Dependency> j = dependencyManagement.iterator();
            while ( !matched && j.hasNext() )
            {
                Dependency t = j.next();
                if ( StringUtils.equals( t.getGroupId(), c.getGroupId() )
                    && StringUtils.equals( t.getArtifactId(), c.getArtifactId() )
                    && ( t.getScope() == null || StringUtils.equals( t.getScope(), c.getScope() ) )
                    && ( t.getClassifier() == null || StringUtils.equals( t.getClassifier(), c.getClassifier() ) )
                    && ( c.getVersion() == null || t.getVersion() == null
                        || StringUtils.equals( t.getVersion(), c.getVersion() ) ) )
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