package org.apache.maven.plugins.enforcer;
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

import java.util.HashMap;

import org.apache.maven.enforcer.rule.api.EnforcerRuleException;
import org.apache.maven.enforcer.rule.api.EnforcerRuleHelper;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluationException;
import org.codehaus.plexus.component.repository.exception.ComponentLookupException;
import org.junit.Test;
import org.mockito.ArgumentMatchers;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.dependencyWith;
import static org.codehaus.mojo.versions.utils.MockUtils.mockAetherRepositorySystem;
import static org.codehaus.mojo.versions.utils.MockUtils.mockMavenSession;
import static org.codehaus.mojo.versions.utils.MockUtils.mockRepositorySystem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MaxDependencyUpdatesTest
{
    private static EnforcerRuleHelper mockRuleHelper( MavenProject mavenProject,
                                                      org.eclipse.aether.RepositorySystem aetherRepositorySystem )
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mock( EnforcerRuleHelper.class );
        when( ruleHelper.evaluate( anyString() ) )
                .then( ( a ) -> "${project}".equals( a.getArgument( 0 ) )
                        ? mavenProject
                        : "${session}".equals( a.getArgument( 0 ) )
                            ? mockMavenSession()
                            : "${mojoExecution}".equals( a.getArgument( 0 ) )
                                ? mock( MojoExecution.class )
                                : null );
        when( ruleHelper.getComponent( ArgumentMatchers.<Class<?>>any() ) )
                .then( ( a ) -> a.getArgument( 0 ) == RepositorySystem.class
                        ? mockRepositorySystem()
                        : a.getArgument( 0 ) == org.eclipse.aether.RepositorySystem.class
                            ? aetherRepositorySystem
                            : null );
        return ruleHelper;
    }

    @Test
    public void testRuleFailsByMaxUpdatesExceeded()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( asList(
                    dependencyWith( "group", "artifactA", "1.0.0" ),
                    dependencyWith( "group", "artifactB", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "artifactA", new String[] { "1.0.0", "2.0.0" } );
            put( "artifactB", new String[] { "1.0.0", "2.0.0" } );
        }} ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                maxUpdates = 1;
            }}.execute( ruleHelper );

            fail( "EnforcerRuleException should have been thrown" );
        }
        catch ( EnforcerRuleException e )
        {
            assertThat( e.getMessage(), containsString( "More than 1 upgradable artifacts detected" ) );
        }
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceeded()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( singletonList(
                    dependencyWith( "group", "artifactA", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( singletonMap( "artifactA", new String[] { "1.0.0", "2.0.0" } ) ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                maxUpdates = 1;
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyIncludes()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( asList(
                    dependencyWith( "group", "artifactA", "1.0.0" ),
                    dependencyWith( "group", "artifactB", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "artifactA", new String[] { "1.0.0", "2.0.0" } );
            put( "artifactB", new String[] { "1.0.0" } );
        }} ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                dependencyIncludes = singletonList( "group:artifactB" );
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyExcludes()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( asList(
                    dependencyWith( "group", "artifactA", "1.0.0" ),
                    dependencyWith( "group", "artifactB", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "artifactA", new String[] { "1.0.0", "2.0.0" } );
            put( "artifactB", new String[] { "1.0.0" } );
        }} ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                dependencyExcludes = singletonList( "group:artifactA" );
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testRulePassesByMaxUpdatesNotExceededDependencyIncludesExcludes()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( asList(
                    dependencyWith( "group", "artifactA", "1.0.0" ),
                    dependencyWith( "group", "artifactB", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( new HashMap<String, String[]>()
        {{
            put( "artifactA", new String[] { "1.0.0", "2.0.0" } );
            put( "artifactB", new String[] { "1.0.0" } );
        }} ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                dependencyIncludes = singletonList( "group:*" );
                dependencyExcludes = singletonList( "group:artifactA" );
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testIgnoreSubIncrementalUpdates()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( singletonList(
                    dependencyWith( "group", "artifactA", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( singletonMap( "artifactA",
                new String[] { "1.0.0", "1.0.0-1" } ) ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                ignoreSubIncrementalUpdates = true;
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testIgnoreIncrementalUpdates()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( singletonList(
                    dependencyWith( "group", "artifactA", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( singletonMap( "artifactA",
                new String[] { "1.0.0", "1.0.0-1", "1.0.1" } ) ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                ignoreIncrementalUpdates = true;
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }

    @Test
    public void testIgnoreMinorUpdates()
            throws ExpressionEvaluationException, ComponentLookupException
    {
        EnforcerRuleHelper ruleHelper = mockRuleHelper( new MavenProject()
        {{
            setDependencies( asList(
                    dependencyWith( "group", "artifactA", "1.0.0" ) ) );
        }}, mockAetherRepositorySystem( singletonMap( "artifactA",
                new String[] { "1.0.0", "1.0.0-1", "1.0.1", "1.1.0" } ) ) );

        try
        {
            new MaxDependencyUpdates()
            {{
                ignoreMinorUpdates = true;
            }}.execute( ruleHelper );
        }
        catch ( EnforcerRuleException e )
        {
            fail( "No EnforcerRuleException should have been thrown" );
        }
    }
}
