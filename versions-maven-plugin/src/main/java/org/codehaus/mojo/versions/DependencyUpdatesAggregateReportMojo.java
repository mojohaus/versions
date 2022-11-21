package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */


import java.util.Map;
import java.util.Set;
import javax.inject.Inject;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.util.AggregateReportUtils;
import org.codehaus.plexus.i18n.I18N;

/**
 * Generates an aggregate report of available updates for the dependencies of a project.
 *
 * @since 2.14.0
 */
@Mojo( name = "dependency-updates-aggregate-report",
        requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true, aggregator = true )
public class DependencyUpdatesAggregateReportMojo extends AbstractDependencyUpdatesReportMojo
{

    @Inject
    protected DependencyUpdatesAggregateReportMojo( I18N i18n, RepositorySystem repositorySystem,
                                                    org.eclipse.aether.RepositorySystem aetherRepositorySystem,
                                                    Map<String, Wagon> wagonMap,
                                                    ReportRendererFactory rendererFactory )
    {
        super( i18n, repositorySystem, aetherRepositorySystem, wagonMap, rendererFactory );
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populateDependencies( Set<Dependency> dependenciesCollector )
    {
        getLog().debug( String.format( "Collecting dependencies for project %s",
                                       project.getName() ) );
        for ( MavenProject project : AggregateReportUtils.getProjectsToProcess( getProject() ) )
        {
            dependenciesCollector.addAll( project.getDependencies() );
        }
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populateDependencyManagement( Set<Dependency> dependencyManagementCollector,
                                                 Set<Dependency> dependencies ) throws MavenReportException
    {
        for ( MavenProject project : AggregateReportUtils.getProjectsToProcess( getProject() ) )
        {
            getLog().debug( String.format( "Collecting managed dependencies for project %s",
                                           project.getName() ) );
            handleDependencyManagementTransitive( project, dependencyManagementCollector );
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOutputName()
    {
        return "dependency-updates-aggregate-report";
    }

}
