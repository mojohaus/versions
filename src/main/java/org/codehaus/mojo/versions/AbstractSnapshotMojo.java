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
 * Abstract base class for Snapshot modification Mojos.
 *
 * @author Petr Újezdský
 * @since 2.3
 */
public abstract class AbstractSnapshotMojo extends AbstractVersionsSetMojo {

    private final boolean addSnapshot;

    protected AbstractSnapshotMojo(boolean addSnapshot) {
        this.addSnapshot = addSnapshot;
    }

    @Override
    protected String getNewVersion() throws MojoExecutionException, VersionParseException {
        VersionInfo versionInfo = new DefaultVersionInfo(getOldVersion());

        if (!addSnapshot && versionInfo.isSnapshot())
        {
            return versionInfo.getReleaseVersionString();
        }
        else if (addSnapshot && !versionInfo.isSnapshot())
        {
            return versionInfo.getSnapshotVersionString();
        } else {
            String message = addSnapshot ?
                    "Trying to add -SNAPSHOT to version that already contains it (" + getOldVersion() + ")"
                    :"Trying to remove -SNAPSHOT from version that does not contain it (" + getOldVersion() + ")";
            throw new MojoExecutionException(message);
        }
    }
}
