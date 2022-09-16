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
import org.codehaus.mojo.versions.utils.PropertyComparator;
import org.codehaus.plexus.i18n.I18N;

/**
 * Generates a report of available updates for properties of a project which are linked to the dependencies and/or
 * plugins of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo( name = "property-updates-report", requiresProject = true, requiresDependencyResolution = ResolutionScope.RUNTIME,
       threadSafe = true )
public class PropertyUpdatesReport
    extends AbstractVersionsReport
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

    @Inject
    protected PropertyUpdatesReport( I18N i18n, RepositorySystem repositorySystem, ArtifactResolver artifactResolver,
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
        return haveBuildProperties();
    }

    private boolean haveBuildProperties()
    {
        return getProject().getProperties() != null && !getProject().getProperties().isEmpty();
    }

    protected void doGenerateReport( Locale locale, Sink sink )
        throws MavenReportException
    {
        final Map<Property, PropertyVersions> updateSet = new TreeMap<>( new PropertyComparator() );
        try
        {
            updateSet.putAll( getHelper().getVersionPropertiesMap( getProject(), properties, includeProperties,
                                                                   excludeProperties, autoLinkItems ) );
        }
        catch ( MojoExecutionException e )
        {
            throw new MavenReportException( e.getMessage(), e );
        }
        PropertyUpdatesRenderer renderer =
            new PropertyUpdatesRenderer( sink, getI18n(), getOutputName(), locale, updateSet );
        renderer.render();
    }

    /**
     * {@inheritDoc}
     */
    public String getOutputName()
    {
        return "property-updates-report";
    }
}
