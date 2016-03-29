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
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Displays properties that are linked to artifact versions and have updates available.
 *
 * @author Stephen Connolly
 * @goal display-property-updates
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 1.0-beta-1
 */
public class DisplayPropertyUpdatesMojo
    extends AbstractVersionsDisplayMojo
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
     * @parameter property="includeProperties"
     * @since 1.0-alpha-1
     */
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not update.
     *
     * @parameter property="excludeProperties"
     * @since 1.0-alpha-1
     */
    private String excludeProperties = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @parameter property="autoLinkItems" defaultValue="true"
     * @since 1.0-alpha-2
     */
    private Boolean autoLinkItems;

// -------------------------- STATIC METHODS --------------------------

    // -------------------------- OTHER METHODS --------------------------

    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        logInit();
        List<String> current = new ArrayList<String>();
        List<String> updates = new ArrayList<String>();

        Map<Property, PropertyVersions> propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), properties, includeProperties, excludeProperties,
                                                      !Boolean.FALSE.equals( autoLinkItems ) );
        for ( Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet() )
        {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            final String currentVersion = getProject().getProperties().getProperty( property.getName() );
            if ( currentVersion == null )
            {
                continue;
            }

            ArtifactVersion winner =
                version.getNewestVersion( currentVersion, property, this.allowSnapshots, this.reactorProjects,
                                          this.getHelper(), this.resolveLatest );

            if ( winner != null && !currentVersion.equals( winner.toString() ) )
            {
                StringBuilder buf = new StringBuilder();
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
                StringBuilder buf = new StringBuilder();
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

        logLine( false, "" );
        if ( !current.isEmpty() )
        {
            logLine( false, "The following version properties are referencing the newest available version:" );
            for ( String s : current )
            {
                logLine( false, "  " + s );
            }
        }
        if ( updates.isEmpty() && current.isEmpty() )
        {
            logLine( false, "This project does not have any properties associated with versions." );
        }
        else if ( updates.isEmpty() )
        {
            logLine( false, "All version properties are referencing the newest version available." );
        }

        if ( !updates.isEmpty() )
        {
            logLine( false, "The following version property updates are available:" );
            for ( String update : updates )
            {
                logLine( false, "  " + update );
            }
        }
        logLine( false, "" );
    }

    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException, ArtifactMetadataRetrievalException
    {
    }
}
