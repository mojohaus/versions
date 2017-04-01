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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Map;

/**
 * Sets a property to the latest version in a given range of associated artifacts.
 *
 * @author Eric Pabst
 * @since 1.3
 */
@Mojo(name = "update-property", requiresProject = true, requiresDirectInvocation = true)
public class UpdatePropertyMojo
    extends AbstractVersionsUpdaterMojo
{

    // ------------------------------ FIELDS ------------------------------

    /**
     * A property to update.
     * 
     * @since 1.3
     */
    @Parameter (property = "property")
    private String property = null;

    /**
     * The new version to set the property to (can be a version range to find a version within).
     *
     * @since 1.3
     */
    @Parameter (property = "newVersion")
    private String newVersion = null;

    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @parameter property="autoLinkItems" defaultValue="true"
     * @since 1.0-alpha-2
     */
    @Parameter (property = "autoLinkItems", defaultValue = "true")
    private Boolean autoLinkItems;

    /**
     * If a property points to a version like <code>1.2.3-SNAPSHOT</code>
     * and your repo contains a version like <code>1.1.0</code> without
     * settings this to <code>true</code> the property will not being changed.
     * @since 3.0.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    private boolean allowDowngrade;

    // -------------------------- STATIC METHODS --------------------------

    // -------------------------- OTHER METHODS --------------------------

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException when things go wrong in a very bad way
     * @throws XMLStreamException when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(ModifiedPomXMLEventReader)
     * @since 1.0-alpha-1
     */
    protected void update( ModifiedPomXMLEventReader pom )
        throws MojoExecutionException, MojoFailureException, XMLStreamException
    {
        Property propertyConfig = new Property( property );
        propertyConfig.setVersion( newVersion );
        Map<Property, PropertyVersions> propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), new Property[] { propertyConfig }, property, "",
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

            updatePropertyToNewestVersion( pom, property, version, currentVersion, allowDowngrade );

        }
    }

}