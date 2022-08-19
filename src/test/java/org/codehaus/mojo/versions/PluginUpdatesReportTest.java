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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.doxia.module.xhtml5.Xhtml5SinkFactory;
import org.apache.maven.doxia.sink.SinkFactory;
import org.apache.maven.doxia.tools.SiteTool;
import org.apache.maven.doxia.tools.SiteToolException;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginManagement;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.plexus.i18n.I18N;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_RUNTIME;
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
        @SuppressWarnings( "deprecation" )
        public TestPluginUpdatesReport()
        {
            mockPlexusComponents();

            project = new MavenProject();
            project.setBuild( new Build() );
            project.getBuild().setPluginManagement( new PluginManagement() );

            artifactMetadataSource = mock( ArtifactMetadataSource.class );
            try
            {
                when( artifactMetadataSource.retrieveAvailableVersions( any( Artifact.class ), any(), any() ) ).then(
                        invocation -> {
                            Artifact artifact = invocation.getArgument( 0 );
                            if ( "artifactA".equals( artifact.getArtifactId() ) && "1.0.0".equals(
                                    artifact.getVersion() ) )
                            {
                                return Arrays.asList( new DefaultArtifactVersion( artifact.getVersion() ),
                                        new DefaultArtifactVersion( "2.0.0" ) );
                            }
                            if ( "artifactB".equals( artifact.getArtifactId() ) && "1.0.0".equals(
                                    artifact.getVersion() ) )
                            {
                                return Arrays.asList( new DefaultArtifactVersion( artifact.getVersion() ),
                                        new DefaultArtifactVersion( "1.1.0" ) );
                            }
                            return Collections.singletonList( new DefaultArtifactVersion( artifact.getVersion() ) );
                        } );
            }
            catch ( ArtifactMetadataRetrievalException e )
            {
                throw new RuntimeException( e );
            }
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

        /**
         * <p></p>Mocks some Plexus components to speed up test execution.</p>
         * <p>Note: these components could just as well be injected using
         * <code>org.codehaus.plexus.PlexusTestCase.lookup</code>,
         * but that method greatly slows down test execution.</p>
         *
         * @see <a
         * href="https://codehaus-plexus.github.io/guides/developer-guide/building-components/component-testing.html">Testing
         * Plexus Components</a>
         */
        private void mockPlexusComponents()
        {
            i18n = mock( I18N.class );
            when( i18n.getString( anyString(), any(), anyString() ) ).thenAnswer(
                    invocation -> invocation.getArgument( 2 ) );

            repositorySystem = mock( RepositorySystem.class );
            when( repositorySystem.createPluginArtifact( any( Plugin.class ) ) ).thenAnswer( invocation -> {
                Plugin plugin = invocation.getArgument( 0 );
                return new DefaultArtifact( plugin.getGroupId(), plugin.getArtifactId(), plugin.getVersion(),
                        SCOPE_RUNTIME, "maven-plugin", "jar", null );
            } );

            Artifact skinArtifact = mock( Artifact.class );
            when( skinArtifact.getId() ).thenReturn( "" );
            siteTool = mock( SiteTool.class );
            try
            {
                when( siteTool.getSkinArtifactFromRepository( any(), any(), any() ) ).thenReturn( skinArtifact );
            }
            catch ( SiteToolException e )
            {
                throw new RuntimeException( e );
            }
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
}