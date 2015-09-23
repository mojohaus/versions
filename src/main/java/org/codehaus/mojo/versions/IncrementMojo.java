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
import org.apache.maven.shared.release.versions.DefaultVersionInfo;
import org.apache.maven.shared.release.versions.VersionInfo;
import org.apache.maven.shared.release.versions.VersionParseException;

/**
 * Increments the current project's version, updating the details of any child modules as necessary.
 *
 * @author Pete Cornish
 * @goal increment
 * @aggregator
 * @requiresProject true
 * @requiresDirectInvocation true
 * @since 2.0
 */
public class IncrementMojo
    extends AbstractVersionsSetMojo
{

    @Override
    protected String getNewVersion() throws MojoExecutionException, VersionParseException {
		return incrementVersion(getOldVersion());
    }

    /**
     * Increment the version number.
     * 
     * @param version
     * @return
     * @throws VersionParseException 
     */
    private String incrementVersion( String version ) throws VersionParseException {
    	VersionInfo versionInfo = new DefaultVersionInfo(version);
    	
    	if (versionInfo.isSnapshot())
    	{
    		return versionInfo.getNextVersion().getSnapshotVersionString();
    	}
    	else
    	{
    		return versionInfo.getNextVersion().getReleaseVersionString();
    	}
	}

}

