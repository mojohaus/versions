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
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.Locale;
import java.util.Set;

import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_RUNTIME;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactMetadataSource;
import static org.codehaus.mojo.versions.utils.MockUtils.mockI18N;
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.matchesPattern;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Basic tests for {@linkplain PluginUpdatesReport}.
 *
 * @author Andrzej Jarmoniuk
 */
public class PluginUpdatesReportTest
{
    private static class TestPluginUpdatesReport extends PluginUpdatesReport
    {
        TestPluginUpdatesReport()
        {
            super( mockI18N(), mockRepositorySystem(), null, mockArtifactMetadataSource(), null );
            siteTool = MockUtils.mockSiteTool();

            project = new MavenProject();
            project.setBuild( new Build() );
            project.getBuild().setPluginManagement( new PluginManagement() );

            artifactMetadataSource = mockArtifactMetadataSource();
        }

        public TestPluginUpdatesReport withPlugins( Plugin... plugins )
        {
            project.getBuild().setPlugins( Arrays.asList( plugins ) );
            return this;
        }

        public TestPluginUpdatesReport withPluginManagement( Plugin... pluginManagement )
        {
            project.getBuild().getPluginManagement().setPlugins( Arrays.asList( pluginManagement ) );
            return this;
        }

        public TestPluginUpdatesReport withOnlyUpgradable( boolean onlyUpgradable )
        {
            this.onlyUpgradable = onlyUpgradable;
            return this;
        }

        public TestPluginUpdatesReport withOnlyProjectPlugins( boolean onlyProjectPlugins )
        {
            this.onlyProjectPlugins = onlyProjectPlugins;
            return this;
        }

        public TestPluginUpdatesReport withRuleSet(
                RuleSet ruleSet )
        {
            this.ruleSet = ruleSet;
            return this;
        }

        public TestPluginUpdatesReport withIgnoredVersions(
                Set<String> ignoredVersions )
        {
            this.ignoredVersions = ignoredVersions;
            return this;
        }

        private static RepositorySystem mockRepositorySystem()
        {
            RepositorySystem repositorySystem = mock( RepositorySystem.class );
            when( repositorySystem.createPluginArtifact( any( Plugin.class ) ) ).thenAnswer(
                invocation ->
                {
                    Plugin plugin = invocation.getArgument( 0 );
                    return new DefaultArtifact( plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion(),
                                                SCOPE_RUNTIME, "maven-plugin", "jar", null );
                } );
            return repositorySystem;
        }
    }

    private static Plugin pluginOf( String artifactId )
    {
        return new Plugin()
        {
            {
                setGroupId( "defaultGroup" );
                setArtifactId( artifactId );
                setVersion( "1.0.0" );
            }
        };
    }

    @Test
    public void testOnlyUpgradablePlugins() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
            .withPlugins( pluginOf( "artifactA" ), pluginOf( "artifactB" ),
                          pluginOf( "artifactC" ) )
            .withOnlyUpgradable( true )
            .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, allOf( containsString( "artifactA" ), containsString( "artifactB" ) ) );
        assertThat( output, not( containsString( "artifactC" ) ) );
    }

    @Test
    public void testOnlyUpgradableWithPluginManagement() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
            .withPluginManagement( pluginOf( "artifactA" ), pluginOf( "artifactB" ),
                                   pluginOf( "artifactC" ) )
            .withOnlyUpgradable( true )
            .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, allOf( containsString( "artifactA" ), containsString( "artifactB" ) ) );
        assertThat( output, not( containsString( "artifactC" ) ) );
    }

    @Test
    public void testOnlyProjectPlugins() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
            .withPlugins( pluginOf( "artifactA" ) )
            .withPluginManagement( pluginOf( "artifactA" ), pluginOf( "artifactB" ),
                                   pluginOf( "artifactC" ) )
            .withOnlyUpgradable( true )
            .withOnlyProjectPlugins( true )
            .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, containsString( "artifactA" ) );
        assertThat( output, not( anyOf( containsString( "artifactB" ), containsString( "artifactC" ) ) ) );
    }

    @Test
    public void testOnlyProjectPluginsWithIgnoredVersions() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestPluginUpdatesReport()
                .withPlugins( pluginOf( "artifactA" ) )
                .withPluginManagement( pluginOf( "artifactA" ), pluginOf( "artifactB" ),
                        pluginOf( "artifactC" ) )
                .withOnlyUpgradable( true )
                .withOnlyProjectPlugins( true )
                .withIgnoredVersions( Collections.singleton( "2.0.0" ) )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString().replaceAll( "\\s", " " )
                .replaceAll( "<[^>]+>", " " ).replaceAll( "&[^;]+;", " " );
        assertThat( output, matchesPattern( ".*\\breport.overview.numNewerVersionAvailable\\s+0\\b.*" ) );
    }
}
