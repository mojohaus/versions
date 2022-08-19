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
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.plexus.i18n.I18N;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
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
 * Basic tests for {@linkplain DependencyUpdatesReport}.
 *
 * @author Andrzej Jarmoniuk
 */
public class DependencyUpdatesReportTest
{
    private static class TestDependencyUpdatesReport extends DependencyUpdatesReport
    {
        @SuppressWarnings( "deprecation" )
        public TestDependencyUpdatesReport()
        {
            mockPlexusComponents();

            project = new MavenProject();
            project.setOriginalModel( new Model() );
            project.getOriginalModel().setDependencyManagement( new DependencyManagement() );
            project.getModel().setDependencyManagement( new DependencyManagement() );

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

        public TestDependencyUpdatesReport withDependencies( Dependency... dependencies )
        {
            project.setDependencies( Arrays.asList( dependencies ) );
            return this;
        }

        public TestDependencyUpdatesReport withOriginalDependencyManagement(
                Dependency... originalDependencyManagement )
        {
            project.getOriginalModel().getDependencyManagement()
                    .setDependencies( Arrays.asList( originalDependencyManagement ) );
            return this;
        }

        public TestDependencyUpdatesReport withDependencyManagement( Dependency... dependencyManagement )
        {
            project.getModel().getDependencyManagement().setDependencies( Arrays.asList( dependencyManagement ) );
            return this;
        }

        public TestDependencyUpdatesReport withOnlyUpgradable( boolean onlyUpgradable )
        {
            this.onlyUpgradable = onlyUpgradable;
            return this;
        }

        public TestDependencyUpdatesReport withProcessDependencyManagement( boolean processDependencyManagement )
        {
            this.processDependencyManagement = processDependencyManagement;
            return this;
        }

        public TestDependencyUpdatesReport withProcessDependencyManagementTransitive(
                boolean processDependencyManagementTransitive )
        {
            this.processDependencyManagementTransitive = processDependencyManagementTransitive;
            return this;
        }

        public TestDependencyUpdatesReport withOnlyProjectDependencies(
                boolean onlyProjectDependencies )
        {
            this.onlyProjectDependencies = onlyProjectDependencies;
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
            when( repositorySystem.createDependencyArtifact( any( Dependency.class ) ) ).thenAnswer( invocation -> {
                Dependency dependency = invocation.getArgument( 0 );
                return new DefaultArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                        dependency.getVersion(), dependency.getScope(), dependency.getType(),
                        dependency.getClassifier(), null );
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

    private static Dependency dependencyOf( String artifactId )
    {
        return DependencyBuilder.dependencyWith( "groupA", artifactId, "1.0.0", "default", "pom", SCOPE_COMPILE );
    }

    @Test
    public void testOnlyUpgradableDependencies() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReport()
                .withOnlyUpgradable( true )
                .withDependencies(
                        dependencyOf( "artifactA" ), dependencyOf( "artifactB" ),
                        dependencyOf( "artifactC" ) )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, allOf( containsString( "artifactA" ), containsString( "artifactB" ) ) );
        assertThat( output, not( containsString( "artifactC" ) ) );
    }

    @Test
    public void testOnlyUpgradableWithOriginalDependencyManagement() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReport()
                .withOriginalDependencyManagement( dependencyOf( "artifactA" ), dependencyOf( "artifactB" ),
                        dependencyOf( "artifactC" ) )
                .withProcessDependencyManagement( true )
                .withOnlyUpgradable( true )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, allOf( containsString( "artifactA" ), containsString( "artifactB" ) ) );
        assertThat( output, not( containsString( "artifactC" ) ) );
    }

    @Test
    public void testOnlyUpgradableWithTransitiveDependencyManagement() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReport()
                .withDependencyManagement(
                        dependencyOf( "artifactA" ), dependencyOf( "artifactB" ),
                        dependencyOf( "artifactC" ) )
                .withProcessDependencyManagement( true )
                .withProcessDependencyManagementTransitive( true )
                .withOnlyUpgradable( true )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, allOf( containsString( "artifactA" ), containsString( "artifactB" ) ) );
        assertThat( output, not( containsString( "artifactC" ) ) );
    }

    @Test
    public void testOnlyProjectDependencies() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReport()
                .withDependencies( dependencyOf( "artifactA" ) )
                .withDependencyManagement( dependencyOf( "artifactA" ), dependencyOf( "artifactB" ),
                        dependencyOf( "artifactC" ) )
                .withProcessDependencyManagement( true )
                .withOnlyProjectDependencies( true )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString();
        assertThat( output, containsString( "artifactA" ) );
        assertThat( output, not( anyOf( containsString( "artifactB" ), containsString( "artifactC" ) ) ) );
    }
}