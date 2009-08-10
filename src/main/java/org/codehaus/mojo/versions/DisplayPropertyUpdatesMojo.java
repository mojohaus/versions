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
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.*;

/**
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author Stephen Connolly
 * @goal display-property-updates
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-beta-1
 */
public class DisplayPropertyUpdatesMojo
    extends AbstractVersionsUpdaterMojo
{

    /**
     * The width to pad info messages.
     *
     * @since 1.0-alpha-1
     */
    private static final int INFO_PAD_SIZE = 68;

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

    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        List current = new ArrayList();
        List updates = new ArrayList();

        Map propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), properties, includeProperties, excludeProperties,
                                                      !Boolean.FALSE.equals( autoLinkItems ) );
        Iterator i = propertyVersions.entrySet().iterator();
        while ( i.hasNext() )
        {
            Map.Entry/*<Property,PropertyVersions>*/ entry = (Map.Entry/*<Property,PropertyVersions>*/) i.next();
            Property property = (Property) entry.getKey();
            PropertyVersions version = (PropertyVersions) entry.getValue();
            VersionComparator comparator = version.getVersionComparator();

            final String currentVersion = getProject().getProperties().getProperty( property.getName() );
            if ( currentVersion == null )
            {
                continue;
            }

            final boolean includeSnapshots = !property.isBanSnapshots() && Boolean.TRUE.equals( allowSnapshots );
            ArtifactVersion[] artifactVersions = version.getVersions( includeSnapshots );
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
            ArtifactVersion winner =
                version.getNewestVersion( range, getHelper().createArtifactVersion( currentVersion ), null,
                                          includeSnapshots, false, true );
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
                        else if ( comparator.compare( winner, fromReactor ) < 0 )
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
            if ( winner != null && !currentVersion.equals( winner.toString() ) )
            {
                StringBuffer buf = new StringBuffer();
                buf.append( "${" );
                buf.append( property.getName() );
                buf.append( "} " );
                final String newVersion = winner.toString();
                int padding = INFO_PAD_SIZE - currentVersion.length() - newVersion.length() - 4;
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( currentVersion );
                buf.append( " -> " );
                buf.append( newVersion );
                updates.add( buf.toString() );
            }
            else
            {
                StringBuffer buf = new StringBuffer();
                buf.append( "${" );
                buf.append( property.getName() );
                buf.append( "} " );
                int padding = INFO_PAD_SIZE - currentVersion.length();
                while ( buf.length() < padding )
                {
                    buf.append( '.' );
                }
                buf.append( ' ' );
                buf.append( currentVersion );
                current.add( buf.toString() );
            }

        }
        getLog().info( "" );
        if ( !current.isEmpty() )
        {
            getLog().info( "The following version properties are referencing the newest available version:" );
            i = updates.iterator();
            while ( i.hasNext() )
            {
                getLog().info( "  " + i.next() );
            }
        }
        if ( updates.isEmpty() && current.isEmpty() )
        {
            getLog().info( "This project does not have any properties associated with versions." );
        }
        else if ( updates.isEmpty() )
        {
            getLog().info( "All version properties are referencing the newest version available." );
        }

        if ( !updates.isEmpty() )
        {
            getLog().info( "The following version property updates are available:" );
            i = updates.iterator();
            while ( i.hasNext() )
            {
                getLog().info( "  " + i.next() );
            }
        }
        getLog().info( "" );
    }

    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
    }
}