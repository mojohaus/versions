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
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;
import java.util.Iterator;
import java.util.Map;

/**
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author Stephen Connolly
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
        Map propertyVersions =
            this.getHelper().getVersionPropertiesMap( getProject(), properties, includeProperties, excludeProperties,
                                                      !Boolean.FALSE.equals( autoLinkItems ) );
        Iterator i = propertyVersions.entrySet().iterator();
        while ( i.hasNext() )
        {
            Map.Entry/*<Property,PropertyVersions>*/ entry = (Map.Entry/*<Property,PropertyVersions>*/) i.next();
            Property property = (Property) entry.getKey();
            PropertyVersions version = (PropertyVersions) entry.getValue();

            final String currentVersion = getProject().getProperties().getProperty( property.getName() );
            if ( currentVersion == null )
            {
                continue;
            }

            updatePropertyToNewestVersion( pom, property, version, currentVersion );

        }
    }

}