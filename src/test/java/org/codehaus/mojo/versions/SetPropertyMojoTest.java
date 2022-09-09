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

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.mojo.versions.utils.BaseMojoTestCase;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesPattern;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
public class SetPropertyMojoTest extends BaseMojoTestCase
{
    @Test
    public void testNullNewVersion()
            throws Exception
    {
        SetPropertyMojo mojo = createMojo( "set-property",
                "target/test-classes/org/codehaus/mojo/set-property/pom.xml" );
        assertThat( mojo.getProject().getProperties(), is( mojo.getProject().getModel().getProperties() ) );

        setVariableValueToObject( mojo, "property", "dummy-api-version" );
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
        SetPropertyMojo mojo = createMojo( "set-property",
                "target/test-classes/org/codehaus/mojo/set-property/pom.xml" );
        assertThat( mojo.getProject().getProperties(), is( mojo.getProject().getModel().getProperties() ) );

        setVariableValueToObject( mojo, "property", "dummy-api-version" );
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
        SetPropertyMojo mojo = createMojo( "set-property",
                "src/test/resources/org/codehaus/mojo/set-property/pom.xml" );

        setVariableValueToObject( mojo, "property", null );
        setVariableValueToObject( mojo, "propertiesVersionsFile", null );
        setVariableValueToObject( mojo, "newVersion", "2.0.0" );
        try
        {
            mojo.execute();
            fail();
        }
        catch ( MojoExecutionException e )
        {
            assertThat( e.getMessage(),
                    containsString( "Please provide either 'property' or 'propertiesVersionsFile' parameter." ) );
        }
    }
}
