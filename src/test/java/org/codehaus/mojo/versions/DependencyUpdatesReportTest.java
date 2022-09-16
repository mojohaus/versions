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
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.apache.maven.reporting.MavenReportException;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.utils.MockUtils;
import org.junit.Test;

import static org.apache.maven.artifact.Artifact.SCOPE_COMPILE;
import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactMetadataSource;
import static org.codehaus.mojo.versions.utils.MockUtils.mockI18N;
import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.mockito.ArgumentMatchers.any;
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
        TestDependencyUpdatesReport()
        {
            super( mockI18N(), mockRepositorySystem(), null, mockArtifactMetadataSource(), null );
            siteTool = MockUtils.mockSiteTool();

            project = new MavenProject();
            project.setOriginalModel( new Model() );
            project.getOriginalModel().setDependencyManagement( new DependencyManagement() );
            project.getModel().setDependencyManagement( new DependencyManagement() );
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

        public TestDependencyUpdatesReport withRuleSet(
                RuleSet ruleSet )
        {
            this.ruleSet = ruleSet;
            return this;
        }

        public TestDependencyUpdatesReport withIgnoredVersions(
                Set<String> ignoredVersions )
        {
            this.ignoredVersions = ignoredVersions;
            return this;
        }

        private static RepositorySystem mockRepositorySystem()
        {
            RepositorySystem repositorySystem = mock( RepositorySystem.class );
            when( repositorySystem.createDependencyArtifact( any( Dependency.class ) ) ).thenAnswer(
                invocation ->
                {
                    Dependency dependency = invocation.getArgument( 0 );
                    return new DefaultArtifact( dependency.getGroupId(), dependency.getArtifactId(),
                                                dependency.getVersion(), dependency.getScope(), dependency.getType(),
                                                dependency.getClassifier(), null );
                } );
            return repositorySystem;
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

    @Test
    public void testOnlyProjectDependenciesWithIgnoredVersions() throws IOException, MavenReportException
    {
        OutputStream os = new ByteArrayOutputStream();
        SinkFactory sinkFactory = new Xhtml5SinkFactory();
        new TestDependencyUpdatesReport()
                .withDependencies( dependencyOf( "artifactA" ) )
                .withDependencyManagement( dependencyOf( "artifactA" ), dependencyOf( "artifactB" ),
                        dependencyOf( "artifactC" ) )
                .withProcessDependencyManagement( true )
                .withOnlyProjectDependencies( true )
                .withIgnoredVersions( Collections.singleton( "2.0.0" ) )
                .generate( sinkFactory.createSink( os ), sinkFactory, Locale.getDefault() );

        String output = os.toString().replaceAll( "\n", "" );
        assertThat( output, containsString( "report.noUpdatesAvailable" ) );
    }
}
