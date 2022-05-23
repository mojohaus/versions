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
import org.junit.Test;

import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;

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
        assertThat("artifact").isEqualTo(instance.getArtifactId());
        assertThat("group").isEqualTo(instance.getGroupId());
        assertThat(instance.getVersions()).containsExactly(versions("1.0.0.1",
                                                                    "1.0.0.2",
                                                                    "1.1.1",
                                                                    "2.100.0.1",
                                                                    "2.121.2.1",
                                                                    "3.1.0.1"));

        assertThat(instance.getVersions(new DefaultArtifactVersion("1.1"), null)).containsExactly(versions("1.1.1",
                                                                                                           "2.100.0.1",
                                                                                                           "2.121.2.1",
                                                                                                           "3.1.0.1"));
        assertThat(instance.getVersions(new DefaultArtifactVersion("1.0.0.2"), null))
                   //Matchers.arrayContaining(versions("1.1.1", "2.121.2.1", "2.100.0.1", "3.1.0.1")));
        .containsExactly(versions("1.1.1", "2.100.0.1", "2.121.2.1", "3.1.0.1"));

        assertThat(new DefaultArtifactVersion("2.121.2.1")).isEqualTo(
                instance.getNewestVersion(new DefaultArtifactVersion("1.0"),
                                          new DefaultArtifactVersion(
                                                  "3.0")));
        assertThat(instance.getNewestVersion(new DefaultArtifactVersion("1.1.1"),
                                             new DefaultArtifactVersion("2.0"))).isNull();
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
        assertThat("artifact").isEqualTo(instance.getArtifactId());
        assertThat("group").isEqualTo(instance.getGroupId());
        assertThat(versions("1.0", "1.0.1", "1.1", "3.0")).isEqualTo(instance.getVersions());
        assertThat(versions("3.0")).isEqualTo(instance.getVersions(new DefaultArtifactVersion("1.1"), null));
        assertThat(versions("1.1", "3.0")).isEqualTo(instance.getVersions(new DefaultArtifactVersion("1.0.1"), null));
        assertThat(new DefaultArtifactVersion("1.1"))
                .isEqualTo(instance.getNewestVersion(new DefaultArtifactVersion("1.0"),
                                                     new DefaultArtifactVersion("3.0")));
        assertThat(instance.getNewestVersion(new DefaultArtifactVersion("1.1"),
                                             new DefaultArtifactVersion("3.0"))).isNull();
    }

    private ArtifactVersion[] versions(String... versions) {
        ArtifactVersion[] artifactVersions = new ArtifactVersion[versions.length];
        for ( int i = 0; i < versions.length; i++ ) {
            artifactVersions[i] = new DefaultArtifactVersion( versions[i] );
        }
        return artifactVersions;
    }
}