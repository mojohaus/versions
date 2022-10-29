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

import java.nio.file.Files;
import java.nio.file.Paths;

import org.codehaus.mojo.versions.change.VersionChange;
import org.hamcrest.Matchers;
import org.junit.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * Unit tests for {@link UpdatePropertiesMojo}
 */
public class UpdatePropertyMojoTest extends UpdatePropertiesMojoTestBase
{
    @Test
    public void testAllowMajorUpdates() throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml" ),
                Paths.get( pomDir.toString(), "pom.xml" ), REPLACE_EXISTING );
        UpdatePropertyMojo mojo = setUpMojo( "update-property" );
        mojo.property = "artifact-version";
        mojo.execute();
        assertThat( changeRecorder.getChanges(), Matchers.hasItem( new VersionChange( "default-group",
                "default-artifact", "1.0.0", "2.0.0-M1" ) ) );
    }

    @Test
    public void testAllowMinorUpdates() throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml" ),
                Paths.get( pomDir.toString(), "pom.xml" ), REPLACE_EXISTING );
        UpdatePropertyMojo mojo = setUpMojo( "update-property" );
        mojo.property = "artifact-version";
        mojo.allowMajorUpdates = false;
        mojo.execute();
        assertThat( changeRecorder.getChanges(), Matchers.hasItem( new VersionChange( "default-group",
                "default-artifact", "1.0.0", "1.1.0-alpha" ) ) );
    }

    @Test
    public void testAllowIncrementalUpdates() throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/update-properties/issue-454-pom.xml" ),
                Paths.get( pomDir.toString(), "pom.xml" ), REPLACE_EXISTING );
        UpdatePropertyMojo mojo = setUpMojo( "update-property" );
        mojo.property = "artifact-version";
        mojo.allowMajorUpdates = false;
        mojo.allowMinorUpdates = false;
        mojo.execute();
        assertThat( changeRecorder.getChanges(), Matchers.hasItem( new VersionChange( "default-group",
                "default-artifact", "1.0.0", "1.0.1-rc1" ) ) );
    }
}
