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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.OutputStream;
import java.util.Locale;

import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.doxia.tools.SiteTool;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockSiteTool;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.not;

/**
 * Unit tests for {@link PropertyUpdatesReportMojo}
 */
public class PropertyUpdatesReportMojoTest extends AbstractMojoTestCase
{
    @Rule
    public MojoRule mojoRule = new MojoRule( this );
    private static final org.eclipse.aether.RepositorySystem AETHER_REPOSITORY_SYSTEM = mockAetherRepositorySystem();
    private static final SiteTool SITE_TOOL = mockSiteTool();

    @Test
    public void testIncludeParentTrueShouldContainProperty() throws Exception
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();

        PropertyUpdatesReportMojo mojo =
                (PropertyUpdatesReportMojo) mojoRule.lookupConfiguredMojo(
                        new File( "src/test/resources/org/codehaus/mojo/display-property-updates/issue-367/child" ),
                        "property-updates-report" );
        setVariableValueToObject( mojo, "siteTool", SITE_TOOL );
        setVariableValueToObject( mojo, "aetherRepositorySystem", AETHER_REPOSITORY_SYSTEM );
        setVariableValueToObject( mojo, "includeParent", true );
        mojo.generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString()
                .replaceAll( "<[^>]+>", " " )
                .replaceAll( "&[^;]+;", " " )
                .replaceAll( "\\s+", " " );
        assertThat( output, containsString( "${ver}" ) );
    }

    @Test
    public void testIncludeParentFalseShouldNotContainProperty() throws Exception
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();

        PropertyUpdatesReportMojo mojo =
                (PropertyUpdatesReportMojo) mojoRule.lookupConfiguredMojo(
                        new File( "src/test/resources/org/codehaus/mojo/display-property-updates/issue-367/child" ),
                        "property-updates-report" );
        setVariableValueToObject( mojo, "siteTool", SITE_TOOL );
        setVariableValueToObject( mojo, "aetherRepositorySystem", AETHER_REPOSITORY_SYSTEM );
        setVariableValueToObject( mojo, "includeParent", false );
        mojo.generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString()
                .replaceAll( "<[^>]+>", " " )
                .replaceAll( "&[^;]+;", " " )
                .replaceAll( "\\s+", " " );
        assertThat( output, not( containsString( "${ver}" ) ) );
    }
}
