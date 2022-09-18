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

import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProjectBuilder;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Sets a property to the latest version in a given range of associated artifacts.
 *
 * @author Eric Pabst
 * @since 1.3
 */
@Mojo( name = "update-property", requiresProject = true, requiresDirectInvocation = true, threadSafe = true )
public class UpdatePropertyMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * A property to update.
     *
     * @since 1.3
     */
    @Parameter( property = "property" )
    private String property = null;

    /**
     * The new version to set the property to (can be a version range to find a version within).
     * <ul>
     * <li><code>1.0</code>x >= 1.0. The default Maven meaning for 1.0 is everything (,) but with 1.0 recommended.</li>
     * <li><code>[1.0,2.0)</code> Versions 1.0 (included) to 2.0 (not included)</li>
     * <li><code>[1.0,2.0]</code> Versions 1.0 to 2.0 (both included)</li>
     * <li><code>[1.5,)</code> Versions 1.5 and higher</li>
     * <li><code>(,1.0],[1.2,)</code> Versions up to 1.0 (included) and 1.2 or higher</li>
     * </ul>
     * If you like to define the version to be used exactly you have to use it like this:
     * <code>-DnewVersion=[19.0]</code> otherwise a newer existing version will be used. If you need to downgrade a
     * version you have to define <code>-DallowDowngrade=true</code> as well otherwise
     * the version will be kept.
     *
     * @since 1.3
     */
    @Parameter( property = "newVersion" )
    private String newVersion = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @since 1.0-alpha-2
     */
    @Parameter( property = "autoLinkItems", defaultValue = "true" )
    private boolean autoLinkItems;

    /**
     * If a property points to a version like <code>1.2.3</code> and your repository contains versions like
     * <code>1.2.3</code> and <code>1.1.0</code> without settings this to <code>true</code> the property will never
     * being changed back to <code>1.1.0</code> by using <code>-DnewVersion=[1.1.0]</code>.
     *
     * @since 3.0.0
     */
    @Parameter( property = "allowDowngrade", defaultValue = "false" )
    private boolean allowDowngrade;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.4
     */
    @Parameter( property = "allowMajorUpdates", defaultValue = "true" )
    protected boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * @since 2.4
     */
    @Parameter( property = "allowMinorUpdates", defaultValue = "true" )
    protected boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * @since 2.4
     */
    @Parameter( property = "allowIncrementalUpdates", defaultValue = "true" )
    protected boolean allowIncrementalUpdates;

    // -------------------------- STATIC METHODS --------------------------

    // -------------------------- OTHER METHODS --------------------------

    @Inject
    public UpdatePropertyMojo( RepositorySystem repositorySystem,
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
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        Property propertyConfig = new Property( property );
        propertyConfig.setVersion( newVersion );
        Map<Property, PropertyVersions> propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), new Property[] {propertyConfig}, property, "",
                                                      autoLinkItems );
        for ( Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet() )
        {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            final String currentVersion = getProject().getProperties().getProperty( property.getName() );
            if ( currentVersion == null )
            {
                continue;
            }

            Optional<Segment> unchangedSegment =
                    determineUnchangedSegment( allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates );
            try
            {
                ArtifactVersion targetVersion = updatePropertyToNewestVersion( pom, property, version, currentVersion,
                        allowDowngrade, unchangedSegment );

                if ( targetVersion != null )
                {
                    for ( final ArtifactAssociation association : version.getAssociations() )
                    {
                        this.getChangeRecorder().recordUpdate( "updateProperty", association.getGroupId(),
                                association.getArtifactId(), currentVersion,
                                targetVersion.toString() );
                    }
                }
            }
            catch ( InvalidSegmentException | InvalidVersionSpecificationException e )
            {
                getLog().warn( String.format( "Skipping the processing of %s:%s due to: %s", property.getName(),
                        property.getVersion(), e.getMessage() ) );
            }
        }
    }

}
