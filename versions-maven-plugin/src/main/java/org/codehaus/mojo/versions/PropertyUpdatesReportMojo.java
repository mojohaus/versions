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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.doxia.sink.Sink;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.reporting.model.PropertyUpdatesModel;
import org.codehaus.mojo.versions.utils.PropertyComparator;
import org.codehaus.mojo.versions.xml.PropertyUpdatesXmlReportRenderer;
import org.codehaus.plexus.i18n.I18N;

/**
 * Generates a report of available updates for properties of a project which are linked to the dependencies and/or
 * plugins of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "property-updates-report", requiresDependencyResolution = ResolutionScope.RUNTIME,
       threadSafe = true )
public class PropertyUpdatesReportMojo extends AbstractVersionsReport<PropertyUpdatesModel>
{
    /**
     * Any restrictions that apply to specific properties.
     *
     * @since 1.0-beta-1
     */
    @Parameter
    private Property[] properties;

    /**
     * A comma separated list of properties to include in the report.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "includeProperties" )
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not include in the report.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "excludeProperties" )
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @since 1.0-beta-1
     */
    @Parameter( property = "autoLinkItems", defaultValue = "true" )
    private boolean autoLinkItems;

    /**
     * <p>Whether to include property updates from parent.</p>
     *
     * @since 2.14.0
     */
    @Parameter( property = "includeParent", defaultValue = "true" )
    private boolean includeParent = true;

    /**
     * Report formats (html and/or xml). HTML by default.
     *
     * @since 2.14.0
     */
    @Parameter( property = "propertyUpdatesReportFormats", defaultValue = "html" )
    protected String[] formats = new String[] {"html"};

    @Inject
    protected PropertyUpdatesReportMojo( I18N i18n, RepositorySystem repositorySystem,
                                         ArtifactResolver artifactResolver,
                                         ArtifactMetadataSource artifactMetadataSource, WagonManager wagonManager,
                                         ReportRendererFactory rendererFactory )
    {
        super( i18n, repositorySystem, artifactResolver, artifactMetadataSource, wagonManager, rendererFactory );
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
        return haveBuildProperties();
    }

    private boolean haveBuildProperties()
    {
        return getProject().getProperties() != null && !getProject().getProperties().isEmpty();
    }

    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        final Map<Property, PropertyVersions> updateSet = new TreeMap<>( PropertyComparator.INSTANCE );
        try
        {
            updateSet.putAll( getHelper().getVersionPropertiesMap( VersionsHelper.VersionPropertiesMapRequest.builder()
                    .withMavenProject( getProject() )
                    .withPropertyDefinitions( properties )
                    .withIncludeProperties( includeProperties )
                    .withExcludeProperties( excludeProperties )
                    .withIncludeParent( includeParent )
                    .withAutoLinkItems( autoLinkItems )
                    .build() ) );
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
        PropertyUpdatesModel model = new PropertyUpdatesModel( updateSet );
        for ( String format : formats )
        {
            if ( "html".equals( format ) )
            {
                rendererFactory.createReportRenderer( getOutputName(), getSink(), locale, model ).render();
            }
            else if ( "xml".equals( format ) )
            {
                Path outputDir = Paths.get( getProject().getBuild().getDirectory() );
                if ( !Files.exists( outputDir ) )
                {
                    try
                    {
                        Files.createDirectories( outputDir );
                    }
                    catch ( IOException e )
                    {
                        throw new MavenReportException( "Could not create the output directory" );
                    }
                }
                Path outputFile = outputDir.resolve( getOutputName() + ".xml" );
                new PropertyUpdatesXmlReportRenderer( model, outputFile ).render();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "property-updates-report";
    }
}

