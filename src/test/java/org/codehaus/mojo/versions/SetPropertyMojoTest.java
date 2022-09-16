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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.apache.maven.plugin.testing.stubs.StubArtifactRepository;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.matchesPattern;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
public class SetPropertyMojoTest extends AbstractMojoTestCase
{
    @Rule
    MojoRule mojoRule = new MojoRule( this );

    private Path pomDir;

    @Before
    public void setUp() throws Exception
    {
        super.setUp();
        pomDir = Files.createTempDirectory( "set-property-" );
    }

    @After
    public void tearDown() throws Exception
    {
        try
        {
            if ( pomDir != null && pomDir.toFile().exists() )
            {
                Arrays.stream( Objects.requireNonNull( pomDir.toFile().listFiles() ) ).forEach( File::delete );
                pomDir.toFile().delete();
            }
        }
        finally
        {
            super.tearDown();
        }
    }
       @Test
    public void testNullNewVersion()
            throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/set-property/null-new-version/pom.xml" ),
                Paths.get( pomDir.toString(),  "pom.xml" ), REPLACE_EXISTING );
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo( pomDir.toFile(),
                "set-property" );
        mojo.localRepository = new StubArtifactRepository( pomDir.toString() );
        setVariableValueToObject( mojo, "newVersion", null );

        mojo.execute();

        String output = String.join( "", Files.readAllLines( mojo.getProject().getFile().toPath() ) )
                .replaceAll( "\\s*", "" );
        assertThat( output,
                matchesPattern( ".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*" ) );
    }

    @Test
    public void testNewVersionEmpty()
            throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/set-property/null-new-version/pom.xml" ),
                Paths.get( pomDir.toString(),  "pom.xml" ), REPLACE_EXISTING );
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo( pomDir.toFile(),
                "set-property" );
        mojo.localRepository = new StubArtifactRepository( pomDir.toString() );
        setVariableValueToObject( mojo, "newVersion", "" );

        mojo.execute();

        String output = String.join( "", Files.readAllLines( mojo.getProject().getFile().toPath() ) )
                .replaceAll( "\\s*", "" );
        assertThat( output,
                matchesPattern( ".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*" ) );
    }

    @Test
    public void testNullProperty()
            throws Exception
    {
        Files.copy( Paths.get( "src/test/resources/org/codehaus/mojo/set-property/null-property/pom.xml" ),
                Paths.get( pomDir.toString(),  "pom.xml" ), REPLACE_EXISTING );
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo( pomDir.toFile(),
                "set-property" );

        try
        {
            mojo.update( null );
            fail();
        }
        catch ( MojoExecutionException e )
        {
            assertThat( e.getMessage(),
                    containsString( "Please provide either 'property' or 'propertiesVersionsFile' parameter." ) );
        }
    }
}
