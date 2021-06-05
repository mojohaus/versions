package org.codehaus.mojo.versions.api;

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

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.codehaus.mojo.versions.ordering.MercuryVersionComparator;
import org.hamcrest.Matchers;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

public class ArtifactVersionsTest
{

    @Test
    public void test4DigitVersions() throws Exception {
        ArtifactVersion[] versions = versions( "1.0.0.1", "1.0.0.2", "2.121.2.1", "2.100.0.1", "3.1.0.1", "1.1.1");
        final DefaultArtifact artifact =
                new DefaultArtifact( "group", "artifact", VersionRange.createFromVersionSpec( "[1.0,3.0]" ), "foo", "bar",
                                     "jar", new DefaultArtifactHandler() );
        // TODO This should also work for the MavenVersionComparator when using maven 3.x libraries
        ArtifactVersions instance =
                new ArtifactVersions(artifact, Arrays.asList( versions ), new MercuryVersionComparator() );
        assertEquals( "artifact", instance.getArtifactId() );
        assertEquals( "group", instance.getGroupId() );
        assertThat(instance.getVersions(),
                   Matchers.arrayContaining(versions( "1.0.0.1", "1.0.0.2", "1.1.1", "2.100.0.1", "2.121.2.1",
                                                      "3.1.0.1" )));
        assertThat(instance.getVersions( new DefaultArtifactVersion( "1.1" ), null ),
                   Matchers.arrayContaining(versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1")));

        assertThat(instance.getVersions( new DefaultArtifactVersion( "1.0.0.2" ), null ),
                          //Matchers.arrayContaining(versions("1.1.1", "2.121.2.1", "2.100.0.1", "3.1.0.1")));
                   Matchers.arrayContaining(versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1")));

        assertEquals( new DefaultArtifactVersion( "2.121.2.1" ),
                      instance.getNewestVersion( new DefaultArtifactVersion( "1.0" ),
                                                 new DefaultArtifactVersion( "3.0" ) ) );
        assertNull(
                instance.getNewestVersion( new DefaultArtifactVersion( "1.1.1" ),
                                           new DefaultArtifactVersion( "2.0" ) ) );
    }

    @Test
    public void testSmokes()
        throws Exception
    {
        ArtifactVersion[] versions = versions( "1.0", "3.0", "1.1", "1.0", "1.0.1" );
        final DefaultArtifact artifact =
            new DefaultArtifact( "group", "artifact", VersionRange.createFromVersionSpec( "[1.0,3.0]" ), "foo", "bar",
                                 "jar", new DefaultArtifactHandler() );
        ArtifactVersions instance =
            new ArtifactVersions( artifact, Arrays.asList( versions ), new MavenVersionComparator() );
        assertEquals( "artifact", instance.getArtifactId() );
        assertEquals( "group", instance.getGroupId() );
        assertArrayEquals(
            versions( "1.0", "1.0.1", "1.1", "3.0" ),
            instance.getVersions() );
        assertArrayEquals( versions( "3.0" ),
                           instance.getVersions( new DefaultArtifactVersion( "1.1" ), null ) );
        assertArrayEquals(
            versions( "1.1", "3.0" ),
            instance.getVersions( new DefaultArtifactVersion( "1.0.1" ), null ) );
        assertEquals( new DefaultArtifactVersion( "1.1" ),
                      instance.getNewestVersion( new DefaultArtifactVersion( "1.0" ),
                                                 new DefaultArtifactVersion( "3.0" ) ) );
        assertNull(
            instance.getNewestVersion( new DefaultArtifactVersion( "1.1" ), new DefaultArtifactVersion( "3.0" ) ) );
    }

    private ArtifactVersion[] versions(String... versions) {
        ArtifactVersion[] artifactVersions = new ArtifactVersion[versions.length];
        for ( int i = 0; i < versions.length; i++ ) {
            artifactVersions[i] = new DefaultArtifactVersion( versions[i] );
        }
        return artifactVersions;
    }
}