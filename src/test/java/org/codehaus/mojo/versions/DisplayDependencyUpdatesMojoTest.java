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
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.model.TestIgnoreVersions;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.TYPE_REGEX;
import static org.codehaus.mojo.versions.model.TestIgnoreVersions.matches;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactMetadataSource;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

/**
 * Basic tests for {@linkplain DisplayDependencyUpdatesMojo}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DisplayDependencyUpdatesMojoTest extends AbstractMojoTestCase
{
    @Rule
    public final MojoRule mojoRule = new MojoRule( this );

    @Test
    public void testValidateGAVListSuccessful() throws MojoExecutionException
    {
        DisplayDependencyUpdatesMojo.validateGAVList( Arrays.asList( "group", "group:artifact",
                        "group:artifact:version" ), 3, "" );
    }

    @Test
    public void testValidateGAVListFailed()
    {
        try
        {
            DisplayDependencyUpdatesMojo.validateGAVList( Arrays.asList( "group:artifact:version",
                    "group:artifact:version:type" ), 3, "" );
            fail( "Method should have thrown a MojoExecutionException" );
        }
        catch ( MojoExecutionException e )
        {
        }
    }

    @Test
    public void testRuleSetPresentAndWorking() throws Exception
    {
        File outputFile = null;
        try
        {
            outputFile = File.createTempFile( "display-dependency-updates", "" );
            assert outputFile.exists();

            DisplayDependencyUpdatesMojo mojo = (DisplayDependencyUpdatesMojo) mojoRule.lookupConfiguredMojo(
                    new File( "target/test-classes/org/codehaus/mojo/display-dependency-updates/ruleset" ),
                    "display-dependency-updates" );
            assertThat( mojo.ruleSet, notNullValue() );
            assertThat( mojo.ruleSet.getIgnoreVersions(), notNullValue() );
            assertThat( mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize( 3  ) );
            assertThat( mojo.ruleSet.getIgnoreVersions(), hasItem( matches(
                    new TestIgnoreVersions().withVersion( "1.0.1" ) ) ) );
            assertThat( mojo.ruleSet.getIgnoreVersions(), containsInAnyOrder(
                    matches( new TestIgnoreVersions().withVersion( "1.0.1" ) ),
                    matches( new TestIgnoreVersions().withType( TYPE_REGEX ).withVersion( ".+-SNAPSHOT" ) ),
                    matches( new TestIgnoreVersions().withType( TYPE_REGEX ).withVersion( ".+-M\\d+" ) ) ) );

            // This is just an example of how to create it-style tests as unit tests; the advantage is easier debugging
            mojo.outputFile = outputFile;
            mojo.artifactMetadataSource = mockArtifactMetadataSource( new HashMap<String, String[]>()
            {{
                put( "dummy-api", new String[] { "1.0.0", "1.0.1", "1.1.0-M1", "1.2.0-SNAPSHOT" } );
            }} );

            assertThat( mojo.ruleSet.getIgnoreVersions(), Matchers.hasSize( 3 ) );
            mojo.execute();
            List<String> output = Files.readAllLines( outputFile.toPath(), UTF_8 );
            assertThat( output, not( hasItem( containsString( "1.1.0-M1" ) ) ) );
        }
        finally
        {
            assert outputFile == null || !outputFile.exists() || outputFile.delete();
        }
    }
}
