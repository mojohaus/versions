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

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.utils.TestChangeRecorder;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;

/**
 * Base class for {@link UpdatePropertiesMojo} and {@link UpdatePropertyMojo} test suites
 */
public abstract class UpdatePropertiesMojoTestBase extends AbstractMojoTestCase
{
    @Rule
    public MojoRule mojoRule = new MojoRule( this );
    protected Path pomDir;
    protected org.eclipse.aether.RepositorySystem aetherRepositorySystem;
    protected TestChangeRecorder changeRecorder;

    @Before
    public void setUp() throws Exception
    {
        super.setUp();
        pomDir = createTempDir( "update-property" );
        aetherRepositorySystem = mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "default-artifact", new String[] {"1.0.0", "1.0.1-rc1", "1.1.0-alpha", "2.0.0-M1"} );
        }} );
    }

    @After
    public void tearDown() throws Exception
    {
        try
        {
            tearDownTempDir( pomDir );
        }
        finally
        {
            super.tearDown();
        }
    }

    @SuppressWarnings( "unchecked" )
    protected  <T extends Mojo> T setUpMojo( String goal ) throws Exception
    {
        T mojo = (T) mojoRule.lookupConfiguredMojo( pomDir.toFile(), goal );
        setVariableValueToObject( mojo, "aetherRepositorySystem", aetherRepositorySystem );
        setVariableValueToObject( mojo, "generateBackupPoms", false );
        setVariableValueToObject( mojo, "changeRecorderFormat", "test" );
        changeRecorder = (TestChangeRecorder)
            ( (Map<String, ChangeRecorder>) getVariableValueFromObject( mojo, "changeRecorders" ) )
                .get( "test" );

        return (T) mojo;
    }
}
