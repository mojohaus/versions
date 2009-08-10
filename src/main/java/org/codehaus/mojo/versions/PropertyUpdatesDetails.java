package org.codehaus.mojo.versions;

import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.codehaus.mojo.versions.api.ArtifactAssocation;
import org.codehaus.mojo.versions.api.DefaultVersionUpdateDetails;
import org.codehaus.mojo.versions.api.PropertyVersions;

/*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/
/**
 * Created by IntelliJ IDEA.
 *
 * @author connollys
 * @since Jul 27, 2009 2:47:50 PM
 */
public class PropertyUpdatesDetails
    extends DefaultVersionUpdateDetails
{
    public PropertyUpdatesDetails(PropertyVersions v, boolean includeSnapshots)
        throws ArtifactMetadataRetrievalException
    {
        super( v, includeSnapshots );
    }

    public String getName()
    {
        return ((PropertyVersions)getVersionDetails()).getName();
    }

    public ArtifactAssocation[] getAssociations()
    {
        return ((PropertyVersions)getVersionDetails()).getAssociations();
    }

}
