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

/**
 * LinkItem represents information specified for a linked artifact version manged through a property.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
public class LinkItem
{

// ------------------------------ FIELDS ------------------------------

    /**
     * Group Id of the Artifact.
     *
     * @parameter
     * @required
     * @since 1.0-alpha-1
     */
    private String groupId;

    /**
     * Artifact Id of the Artifact.
     *
     * @parameter
     * @required
     * @since 1.0-alpha-1
     */
    private String artifactId;

    /**
     * The property that defines the version of the artifact to use.
     *
     * @parameter
     * @required
     * @since 1.0-alpha-1
     */
    private String property;

    /**
     * Version specification to control artifact resolution.
     *
     * @parameter
     * @since 1.0-alpha-1
     */
    private String version = null;

    /**
     * Override snapshot permisibility.
     *
     * @parameter
     * @since 1.0-alpha-1
     */
    private Boolean allowSnapshots = null;

// --------------------------- CONSTRUCTORS ---------------------------

    /**
     * Constructs a new LinkItem.
     *
     * @since 1.0-alpha-1
     */
    public LinkItem()
    {
    }

// --------------------- GETTER / SETTER METHODS ---------------------

    /**
     * Getter for property 'allowSnapshots'.
     *
     * @return Value for property 'allowSnapshots'.
     * @since 1.0-alpha-1
     */
    public Boolean getAllowSnapshots()
    {
        return allowSnapshots;
    }

    /**
     * Setter for property 'allowSnapshots'.
     *
     * @param allowSnapshots Value to set for property 'allowSnapshots'.
     * @since 1.0-alpha-1
     */
    public void setAllowSnapshots( Boolean allowSnapshots )
    {
        this.allowSnapshots = allowSnapshots;
    }

    /**
     * Getter for property 'artifactId'.
     *
     * @return Value for property 'artifactId'.
     * @since 1.0-alpha-1
     */
    public String getArtifactId()
    {
        return artifactId;
    }

    /**
     * Setter for property 'artifactId'.
     *
     * @param artifactId Value to set for property 'artifactId'.
     * @since 1.0-alpha-1
     */
    public void setArtifactId( String artifactId )
    {
        this.artifactId = artifactId;
    }

    /**
     * Getter for property 'groupId'.
     *
     * @return Value for property 'groupId'.
     * @since 1.0-alpha-1
     */
    public String getGroupId()
    {
        return groupId;
    }

    /**
     * Setter for property 'groupId'.
     *
     * @param groupId Value to set for property 'groupId'.
     * @since 1.0-alpha-1
     */
    public void setGroupId( String groupId )
    {
        this.groupId = groupId;
    }

    /**
     * Getter for property 'property'.
     *
     * @return Value for property 'property'.
     * @since 1.0-alpha-1
     */
    public String getProperty()
    {
        return property;
    }

    /**
     * Setter for property 'property'.
     *
     * @param property Value to set for property 'property'.
     * @since 1.0-alpha-1
     */
    public void setProperty( String property )
    {
        this.property = property;
    }

    /**
     * Getter for property 'version'.
     *
     * @return Value for property 'version'.
     * @since 1.0-alpha-1
     */
    public String getVersion()
    {
        return version;
    }

    /**
     * Setter for property 'version'.
     *
     * @param version Value to set for property 'version'.
     * @since 1.0-alpha-1
     */
    public void setVersion( String version )
    {
        this.version = version;
    }

// ------------------------ CANONICAL METHODS ------------------------

    /**
     * {@inheritDoc}
     *
     * @since 1.0-alpha-1
     */
    public String toString()
    {
        StringBuffer buf = new StringBuffer( "${" );
        buf.append( property );
        buf.append( "} = " );
        buf.append( groupId );
        buf.append( ':' );
        buf.append( artifactId );
        if ( version != null )
        {
            buf.append( ':' );
            buf.append( version );
        }
        return buf.toString();
    }

}
