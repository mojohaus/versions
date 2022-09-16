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
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.mojo.versions.utils.PropertiesVersionsFileReader;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.split;

/**
 * Set a property to a given version without any sanity checks. Please be careful this can lead to changes which might
 * not build anymore. The sanity checks are done by other goals like <code>update-properties</code> or
 * <code>update-property</code> etc. they are not done here. So use this goal with care.
 *
 * @author Karl Heinz Marbaise
 * @since 2.5
 */
@Mojo( name = "set-property", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class SetPropertyMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * A property to update.
     * You can also specify multiple property names separated by "," which are all set to the same new version.
     */
    @Parameter( property = "property" )
    private String property = null;

    /**
     * The new version to set the property.
     */
    @Parameter( property = "newVersion" )
    private String newVersion = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     */
    @Parameter( property = "autoLinkItems", defaultValue = "true" )
    private boolean autoLinkItems;

    /**
     * A property file name containing: property=value, to update several properties at the same time.
     * If 'property' and 'newVersion' are also used, they will be ignored.
     *
     * @since 2.9
     */

    @Parameter( property = "propertiesVersionsFile" )
    private String propertiesVersionsFile;

    @Inject
    public SetPropertyMojo( RepositorySystem repositorySystem,
                                MavenProjectBuilder projectBuilder,
                                ArtifactMetadataSource artifactMetadataSource,
                                WagonManager wagonManager,
                                ArtifactResolver artifactResolver )
    {
        super( repositorySystem, projectBuilder, artifactMetadataSource, wagonManager, artifactResolver );
    }

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
        Property[] propertiesConfig;
        String properties;
        if ( !isEmpty( propertiesVersionsFile ) )
        {
            logWrongConfigWarning();
            getLog().debug( "Reading properties and versions to update from file: " + propertiesVersionsFile );
            PropertiesVersionsFileReader reader = new PropertiesVersionsFileReader( propertiesVersionsFile );
            try
            {
                reader.read();
            }
            catch ( IOException e )
            {
                getLog().error( "Unable to read property file  " + propertiesVersionsFile
                                    + ". re-run with -X option for more details." );
                getLog().debug( "Error while reading  property file " + propertiesVersionsFile, e );
                throw new MojoFailureException( "Unable to read property file " + propertiesVersionsFile );
            }
            propertiesConfig = reader.getPropertiesConfig();
            properties = reader.getProperties();
        }
        else if ( !isEmpty( property ) )
        {
            getLog().debug( "Reading properties and versions to update from property and newVersion " );
            propertiesConfig = Arrays.stream( split( property, "," ) ).map(
                    prp ->
                    {
                        Property propertyConfig = new Property( prp );
                        propertyConfig.setVersion( newVersion );
                        return propertyConfig;
                    } )
                .toArray( Property[]::new );
            properties = property;
        }
        else
        {
            throw new MojoExecutionException(
                "Please provide either 'property' or 'propertiesVersionsFile' parameter." );
        }
        update( pom, propertiesConfig, properties );
    }

    private void update( ModifiedPomXMLEventReader pom, Property[] propertiesConfig, String properties )
        throws MojoExecutionException, XMLStreamException
    {
        Map<Property, PropertyVersions> propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), propertiesConfig, properties, "",
                                                      autoLinkItems );
        for ( Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet() )
        {
            Property currentProperty = entry.getKey();
            PropertyVersions version = entry.getValue();
            String newVersionGiven = currentProperty.getVersion();

            final String currentVersion = getProject().getProperties().getProperty( currentProperty.getName() );
            if ( currentVersion == null )
            {
                continue;
            }
            PomHelper.setPropertyVersion( pom, version.getProfileId(), currentProperty.getName(), 
                    defaultString( newVersionGiven ) );
        }
    }

    private void logWrongConfigWarning()
    {
        if ( !isEmpty( property ) )
        {
            getLog().warn( "-Dproperty provided but will be ignored as -DpropertiesVersionsFile is used" );
        }
        if ( !isEmpty( newVersion ) )
        {
            getLog().warn( "-DnewVersion provided but will be ignored as -DpropertiesVersionsFile is used" );
        }
    }

}
