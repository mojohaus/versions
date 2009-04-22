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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 * @goal update-properties
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-alpha-1
 */
public class UpdatePropertiesMojo
    extends AbstractVersionsUpdaterMojo
{

// ------------------------------ FIELDS ------------------------------

    /**
     * Any restrictions that apply to specific properties.
     *
     * @parameter
     * @since 1.0-alpha-3
     */
    private Property[] properties;

    /**
     * A comma separated list of properties to update.
     *
     * @parameter expression="${includeProperties}"
     * @since 1.0-alpha-1
     */
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not update.
     *
     * @parameter expression="${excludeProperties}"
     * @since 1.0-alpha-1
     */
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @parameter expression="${autoLinkItems}" defaultValue="true"
     * @since 1.0-alpha-2
     */
    private Boolean autoLinkItems;

// -------------------------- STATIC METHODS --------------------------

    // -------------------------- OTHER METHODS --------------------------

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
        final PropertyVersions[] propertyVersions;
        try
        {
            propertyVersions = PomHelper.getPropertyVersions( getHelper(), getProject(), getExpressionEvaluator() );
        }
        catch ( ExpressionEvaluationException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }
        catch ( IOException e )
        {
            throw new MojoExecutionException( e.getMessage(), e );
        }

        Map properties = new HashMap();
        if ( this.properties != null )
        {
            for ( int i = 0; i < this.properties.length; i++ )
            {
                properties.put( this.properties[i].getName(), this.properties[i] );
            }
        }
        Map versions = new HashMap();
        if ( autoLinkItems == null || Boolean.TRUE.equals( autoLinkItems ) )
        {
            for ( int i = 0; i < propertyVersions.length; i++ )
            {
                versions.put( propertyVersions[i].getName(), propertyVersions[i] );
                if ( !properties.containsKey( propertyVersions[i].getName() ) )
                {
                    properties.put( propertyVersions[i].getName(), new Property( propertyVersions[i] ) );
                }
            }
        }
        getLog().info( "Searching for properties to update" );
        Iterator i = properties.values().iterator();
        while ( i.hasNext() )
        {
            Property property = (Property) i.next();
            if ( includeProperties != null )
            {
                if ( includeProperties.indexOf( property.getName() ) < 0 )
                {
                    getLog().debug( "Skipping update of property ${" + property.getName() + "}" );
                    continue;
                }
            }

            if ( excludeProperties != null )
            {
                if ( excludeProperties.indexOf( property.getName() ) >= 0 )
                {
                    getLog().debug( "Ignoring update of property ${" + property.getName() + "}" );
                    continue;
                }
            }

            getLog().debug( "Property ${" + property.getName() + "}" );
            PropertyVersions version = (PropertyVersions) versions.get( property.getName() );
            if ( version == null || !version.isAssociated() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Looks like this property is not "
                    + "associated with any dependency..." );
                version = new PropertyVersions( null, property.getName(), getHelper() );
            }
            if ( !property.isAutoLinkDependencies() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Removing any autoLinkDependencies" );
                version.clearAssociations();
            }
            Dependency[] dependencies = property.getDependencies();
            if ( dependencies != null )
            {
                for ( int j = 0; j < dependencies.length; j++ )
                {
                    try
                    {
                        getLog().debug(
                            "Property ${" + property.getName() + "}: Adding association to " + dependencies[j] );
                        version.addAssociation( getHelper().createDependencyArtifact( dependencies[j] ), false );
                    }
                    catch ( InvalidVersionSpecificationException e )
                    {
                        throw new MojoExecutionException( e.getMessage(), e );
                    }
                }
            }
            ArtifactVersion[] artifactVersions =
                version.getVersions( !property.isBanSnapshots() && Boolean.TRUE.equals( allowSnapshots ) );
            getLog().debug(
                "Property ${" + property.getName() + "}: Set of valid available versions is " + Arrays.asList(
                    artifactVersions ) );
            VersionRange range;
            try
            {
                if ( property.getVersion() != null )
                {
                    range = VersionRange.createFromVersionSpec( property.getVersion() );
                    getLog().debug( "Property ${" + property.getName() + "}: Restricting results to " + range );
                }
                else
                {
                    range = null;
                    getLog().debug( "Property ${" + property.getName() + "}: Restricting results to " + range );
                }
            }
            catch ( InvalidVersionSpecificationException e )
            {
                throw new MojoExecutionException( e.getMessage(), e );
            }
            final String currentVersion = getProject().getProperties().getProperty( property.getName() );
            ArtifactVersion winner = null;
            for ( int j = artifactVersions.length - 1; j >= 0; j-- )
            {
                if ( range == null || range.containsVersion( artifactVersions[j] ) )
                {
                    if ( currentVersion.equals( artifactVersions[j].toString() ) )
                    {
                        getLog().debug( "Property ${" + property.getName() + "}: No newer version" );
                        break;
                    }
                    winner = artifactVersions[j];
                    getLog().debug( "Property ${" + property.getName() + "}: Newest version is: " + winner );
                    break;
                }
            }
            getLog().debug( "Property ${" + property.getName() + "}: Current winner is: " + winner );
            if ( property.isSearchReactor() )
            {
                getLog().debug( "Property ${" + property.getName() + "}: Searching reactor for a valid version..." );
                Collection reactorArtifacts = getHelper().extractArtifacts( reactorProjects );
                ArtifactVersion[] reactorVersions = version.getVersions( reactorArtifacts );
                getLog().debug(
                    "Property ${" + property.getName() + "}: Set of valid available versions from the reactor is "
                        + Arrays.asList( reactorVersions ) );
                ArtifactVersion fromReactor = null;
                if ( reactorVersions.length > 0 )
                {
                    for ( int j = reactorVersions.length - 1; j >= 0; j-- )
                    {
                        if ( range == null || range.containsVersion( reactorVersions[j] ) )
                        {
                            fromReactor = reactorVersions[j];
                            getLog().debug(
                                "Property ${" + property.getName() + "}: Reactor has version " + fromReactor );
                            break;
                        }
                    }
                }
                if ( fromReactor != null && ( winner != null || !currentVersion.equals( fromReactor.toString() ) ) )
                {
                    if ( property.isPreferReactor() )
                    {
                        getLog().debug(
                            "Property ${" + property.getName() + "}: Reactor has a version and we prefer the reactor" );
                        winner = fromReactor;
                    }
                    else
                    {
                        if ( winner == null )
                        {
                            getLog().debug( "Property ${" + property.getName() + "}: Reactor has the only version" );
                            winner = fromReactor;
                        }
                        else if ( version.compare( winner, fromReactor ) < 0 )
                        {
                            getLog().debug( "Property ${" + property.getName() + "}: Reactor has a newer version" );
                            winner = fromReactor;
                        }
                        else
                        {
                            getLog().debug(
                                "Property ${" + property.getName() + "}: Reactor has the same or older version" );
                        }
                    }
                }
            }
            if ( winner == null || currentVersion.equals( winner.toString() ) )
            {
                getLog().info(
                    "Property ${" + property.getName() + "}: Leaving unchanged as " + currentVersion.toString() );
            }
            else if ( PomHelper.setPropertyVersion( pom, version.getProfileId(), property.getName(),
                                                    winner.toString() ) )
            {
                getLog().info( "Updated ${" + property.getName() + "} from " + currentVersion + " to " + winner );
            }

        }
    }

}