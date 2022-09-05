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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.ordering.MavenVersionComparator;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.core.Is.is;

/**
 * Unit tests for {@linkplain DisplayPluginUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayPluginUpdatesMojoTest
{
    @Test
    public void testOutputLineLength() throws IOException
    {
        Map<Dependency, ArtifactVersions> updates = Collections.singletonMap(
                DependencyBuilder.dependencyWith( "default-group", "default-artifact", "1.0.0" ), new ArtifactVersions(
                        new DefaultArtifact( "default-group", "default-artifact", "1.0.0", SCOPE_COMPILE, "pom",
                                "default", null ), Collections.singletonList( new DefaultArtifactVersion( "1.0.1" ) ),
                        new MavenVersionComparator() ) );
        Stream.of( 60, 80, 120, 150 ).forEach( lineLength -> {
            DisplayPluginUpdatesMojo mojo = new DisplayPluginUpdatesMojo();
            mojo.outputLineWidth = lineLength;
            mojo.outputEncoding = "UTF-8";
            try
            {
                mojo.outputFile = File.createTempFile( "displayDependencyUpdates", "txt" );
            }
            catch ( IOException e )
            {
                throw new RuntimeException( e );
            }
            try
            {
                mojo.execute();
                String output = String.join( "\n", Files.readAllLines( mojo.outputFile.toPath() ) );
                Pattern pattern =
                        Pattern.compile( ".*\n(\\s*default-group:default-artifact\\s.*\\s1.0.0 -> 1.0.1\\s*)\n.*" );
                Matcher matcher = pattern.matcher( output );
                assertThat( "Pattern should match with line length " + lineLength, output, matchesPattern( pattern ) );
                assertThat( matcher.matches(), is( true ) );
                String dependencyUpdateLine = matcher.toMatchResult().group( 1 );
                assertThat( "Line length of the dependency update line should match " + lineLength,
                        dependencyUpdateLine.length(), is( lineLength ) );
            }
            catch ( IOException e )
            {
                throw new RuntimeException( e );
            }
            catch ( MojoExecutionException e )
            {
                throw new RuntimeException( e );
            }
            catch ( MojoFailureException e )
            {
                throw new RuntimeException( e );
            }
            finally
            {
                assertThat( mojo.outputFile.delete(), is( true ) );
            }

        } );
    }
}
