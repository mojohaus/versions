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

import java.io.File;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.utils.DependencyComparator;
import org.codehaus.plexus.i18n.I18N;

import static java.util.Collections.EMPTY_MAP;
import static org.codehaus.mojo.versions.utils.MiscUtils.filter;

/**
 * Generates a report of available updates for the dependencies of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "dependency-updates-report",
       requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true )
public class DependencyUpdatesReport extends AbstractVersionsReport
{

    /**
     * Whether to process the <code>dependencyManagement</code> in pom or not.
     *
     * @since 2.5
     */
    @Parameter( property = "processDependencyManagement", defaultValue = "true" )
    protected boolean processDependencyManagement;

    /**
     * Whether to process the depdendencyManagement part transitive or not.
     * In case of <code>&lt;type&gt;pom&lt;/type&gt;</code>and
     * <code>&lt;scope&gt;import&lt;/scope&gt;</code> this means
     * by default to report also the imported dependencies.
     * If processTransitive is set to <code>false</code> the report will only show
     * updates of the imported pom itself.
     *
     * @since 2.5 Note: Currently in experimental state.
     */
    @Parameter( property = "processDependencyManagementTransitive", defaultValue = "true" )
    protected boolean processDependencyManagementTransitive;

    /**
     * Report formats (html and/or xml). HTML by default.
     */
    @Parameter( property = "dependencyUpdatesReportFormats", defaultValue = "html" )
    protected String[] formats = new String[] {"html"};

    /**
     * If <code>true</code>, only shows the subsection of the <code>dependencyManagement</code> artifacts that
     * are actually used in the project's <code>dependency</code> graph. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter( property = "onlyProjectDependencies", defaultValue = "false" )
    protected boolean onlyProjectDependencies;

    /**
     * If <code>true</code>, only shows upgradable dependencies in the report. <code>false</code> by default.
     *
     * @since 2.12
     */
    @Parameter( property = "onlyUpgradable", defaultValue = "false" )
    protected boolean onlyUpgradable;

    @Inject
    protected DependencyUpdatesReport( I18N i18n, RepositorySystem repositorySystem, ArtifactResolver artifactResolver,
                                       ArtifactMetadataSource artifactMetadataSource, WagonManager wagonManager )
    {
        super( i18n, repositorySystem, artifactResolver, artifactMetadataSource, wagonManager );
    }

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
    @SuppressWarnings( "deprecation" )
    protected void doGenerateReport( Locale locale, Sink sink ) throws MavenReportException
    {
        Set<Dependency> dependencies = new TreeSet<>( new DependencyComparator() );
        dependencies.addAll( getProject().getDependencies() );

        Set<Dependency> dependencyManagement = new TreeSet<>( new DependencyComparator() );

        if ( processDependencyManagement )
        {
            if ( processDependencyManagementTransitive )
            {
                if ( getProject().getDependencyManagement() != null
                    && getProject().getDependencyManagement().getDependencies() != null )
                {
                    for ( Dependency dep : getProject().getDependencyManagement().getDependencies() )
                    {
                        getLog().debug(
                            "Dpmg: " + dep.getGroupId() + ":" + dep.getArtifactId() + ":" + dep.getVersion() + ":"
                                + dep.getType() + ":" + dep.getScope() );
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
                    for ( Dependency dep : getProject().getOriginalModel().getDependencyManagement().getDependencies() )
                    {
                        getLog().debug( "Original Dpmg: " + dep.getGroupId() + ":" + dep.getArtifactId() + ":"
                                            + dep.getVersion() + ":" + dep.getType() + ":" + dep.getScope() );
                    }
                    dependencyManagement.addAll(
                        getProject().getOriginalModel().getDependencyManagement().getDependencies() );
                }
            }

            if ( !onlyProjectDependencies )
            {
                // Retains only dependencies not present in dependencyManagement
                dependencies.removeIf( dep -> dependencyManagement.stream().anyMatch( dmDep -> match( dep, dmDep ) ) );
            }
            else
            {
                // Retain only dependencies in dependencyManagement that are also present in dependencies
                dependencyManagement.removeIf( dep -> dependencies.stream().noneMatch( dmDep -> match( dep, dmDep ) ) );
            }
        }

        try
        {
            Map<Dependency, ArtifactVersions> dependencyUpdates =
                getHelper().lookupDependenciesUpdates( dependencies, false );

            Map<Dependency, ArtifactVersions> dependencyManagementUpdates =
                processDependencyManagement ? getHelper().lookupDependenciesUpdates( dependencyManagement, false )
                    : EMPTY_MAP;

            if ( onlyUpgradable )
            {
                dependencyUpdates = filter( dependencyUpdates, e -> e.getVersions().length > 1 );
                dependencyManagementUpdates = filter( dependencyManagementUpdates, e -> e.getVersions().length > 1 );
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
                    File outputDir = new File( getProject().getBuild().getDirectory() );
                    if ( !outputDir.exists() )
                    {
                        if ( !outputDir.mkdirs() )
                        {
                            throw new MavenReportException( "Could not create output directory" );
                        }
                    }
                    String outputFile = outputDir.getAbsolutePath() + File.separator + getOutputName() + ".xml";
                    DependencyUpdatesXmlRenderer xmlGenerator =
                        new DependencyUpdatesXmlRenderer( dependencyUpdates, dependencyManagementUpdates,
                                                          outputFile );
                    xmlGenerator.render();
                }
            }
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
    }

    /**
     * Compares two dependencies with each other
     *
     * @return true if the two dependencies match
     */
    private boolean match( Dependency dep, Dependency dmDep )
    {
        return dmDep.getGroupId().equals( dep.getGroupId() )
            && dmDep.getArtifactId().equals( dep.getArtifactId() )
            && ( dmDep.getScope() == null || dmDep.getScope().equals( dep.getScope() ) )
            && ( dmDep.getClassifier() == null || dmDep.getClassifier().equals( dep.getClassifier() ) )
            && ( dep.getVersion() == null || dmDep.getVersion() == null || dmDep.getVersion()
            .equals( dep.getVersion() ) );
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "dependency-updates-report";
    }

}
