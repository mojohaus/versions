package org.codehaus.mojo.versions.api;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.manager.WagonManager;
import org.apache.maven.artifact.repository.DefaultArtifactRepository;
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout;
import org.apache.maven.artifact.resolver.DefaultArtifactResolver;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.codehaus.mojo.versions.utils.VersionStub;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.version.Version;
import org.hamcrest.CoreMatchers;
import org.junit.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.IsIterableContaining.hasItems;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test {@link DefaultVersionsHelper}
 */
public class DefaultVersionsHelperTest extends AbstractMojoTestCase
{

    @Test
    public void testPerRuleVersionsIgnored() throws Exception
    {
        final org.eclipse.aether.RepositorySystem repositorySystem = mock( org.eclipse.aether.RepositorySystem.class );
        final Artifact artifact = mock( Artifact.class );
        when( artifact.getGroupId() ).thenReturn( "com.mycompany.maven" );
        when( artifact.getArtifactId() ).thenReturn( "artifact-one" );
        when( artifact.getType() ).thenReturn( "jar" );
        when( artifact.getArtifactHandler() ).thenReturn( new DefaultArtifactHandlerStub( "default" ) );
        when( repositorySystem.resolveVersionRange( any(), any( VersionRangeRequest.class ) ) )
                .then( i -> new VersionRangeResult( i.getArgument( 1 ) )
                        .setVersions( Arrays.asList(
                                new VersionStub( "one" ),
                                new VersionStub( "two" ),
                                new VersionStub( "three" ),
                                new VersionStub( "1.200" ),
                                new VersionStub( "illegalVersion" ) ) ) );

        VersionsHelper helper = createHelper( repositorySystem );

        final ArtifactVersions versions = helper.lookupArtifactVersions( artifact, true );

        final List<String> actual = Arrays.stream( versions.getVersions( true ) )
                .map( ArtifactVersion::toString )
                .collect( Collectors.toList() );

        assertEquals( 3, actual.size() );
        assertThat( actual, hasItems( "three", "1.200", "illegalVersion" ) );
    }

    @Test
    public void testGlobalRuleVersionsIgnored() throws Exception
    {
        final org.eclipse.aether.RepositorySystem repositorySystem = mock( org.eclipse.aether.RepositorySystem.class );
        final Artifact artifact = mock( Artifact.class );
        when( artifact.getGroupId() ).thenReturn( "other.company" );
        when( artifact.getArtifactId() ).thenReturn( "artifact-two" );
        when( artifact.getType() ).thenReturn( "jar" );
        when( artifact.getArtifactHandler() ).thenReturn( new DefaultArtifactHandlerStub( "default" ) );

        final List<Version> artifactVersions = new ArrayList<>();

        final Version one = new VersionStub( "one" );
        final Version two = new VersionStub( "two" );
        final Version three = new VersionStub( "three" );
        artifactVersions.add( one );
        artifactVersions.add( two );
        artifactVersions.add( new VersionStub( "three-alpha" ) );
        artifactVersions.add( new VersionStub( "three-beta" ) );
        artifactVersions.add( three );
        final Version illegal = new VersionStub( "illegalVersion" );
        artifactVersions.add( illegal );

        when( repositorySystem.resolveVersionRange( any(), any( VersionRangeRequest.class ) ) )
                .then( i -> new VersionRangeResult( i.getArgument( 1 ) )
                        .setVersions( artifactVersions ) );

        VersionsHelper helper = createHelper( repositorySystem );

        final ArtifactVersions versions = helper.lookupArtifactVersions( artifact, true );

        final List<Version> actual = Arrays.stream( versions.getVersions( true ) )
                .map( ArtifactVersion::toString )
                .map( VersionStub::new )
                .collect( Collectors.toList() );

        assertEquals( 4, actual.size() );
        assertThat( actual, hasItems( one, two, three, illegal ) );
    }

    @Test
    public void testWildcardMatching()
        throws Exception
    {
        assertTrue( DefaultVersionsHelper.exactMatch( "*", "com.foo.bar" ) );
        assertFalse( DefaultVersionsHelper.exactMatch( "com.bar*", "com-bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "com?foo.bar", "com.foo.bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "co*.foo.b?r", "com.foo.bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "c*oo*r", "com.foo.bar" ) );
    }

    @Test
    public void testRuleSets()
        throws Exception
    {
        VersionsHelper helper = createHelper();

        assertEquals( "no match gives default", VersionComparators.getVersionComparator( "maven" ),
                      helper.getVersionComparator( "net.foo", "bar" ) );
        assertEquals( "matches wildcard", VersionComparators.getVersionComparator( "mercury" ),
                      helper.getVersionComparator( "org.apache.maven", "plugins" ) );
        assertEquals( "exact match wins over initial match", VersionComparators.getVersionComparator( "mercury" ),
                      helper.getVersionComparator( "com.mycompany.custom.maven", "plugins" ) );
        assertEquals( "non-wildcard prefix wins over wildcard prefix match",
                      VersionComparators.getVersionComparator( "maven" ),
                      helper.getVersionComparator( "com.mycompany.maven.plugins", "plugins" ) );
        assertEquals( VersionComparators.getVersionComparator( "maven" ),
                      helper.getVersionComparator( "com.mycompany.maven", "new-maven-plugin" ) );
        assertEquals( VersionComparators.getVersionComparator( "mercury" ),
                      helper.getVersionComparator( "com.mycompany.maven", "old-maven-plugin" ) );
    }


    @Test
    public void testMVERSIONS159ExcludedAndNotIncluded()
        throws Exception
    {
        VersionsHelper helper = createHelper();
        MavenProject project = null;

        Property[] propertyDefinitions = new Property[] {
            new Property( "bar.version" )
        };
        // should not throw an IllegalStateException
        Map<Property, PropertyVersions> result =
            helper.getVersionPropertiesMap( VersionsHelper.VersionPropertiesMapRequest.builder()
                    .withMavenProject( project )
                    .withPropertyDefinitions( propertyDefinitions )
                    .withIncludeProperties( "foo.version" )
                    .withExcludeProperties( "bar.version" )
                    .withIncludeParent( false )
                    .withAutoLinkItems( false )
                    .build() );
        assertTrue( result.isEmpty() );
    }

    @Test
    public void testIsClasspathUriDetectsClassPathProtocol() throws Exception
    {
        DefaultVersionsHelper helper = createHelper();
        String uri = "classpath:/p/a/c/k/a/g/e/resource.res";

        assertThat( DefaultVersionsHelper.isClasspathUri( uri ), CoreMatchers.is( true ) );
    }

    @Test
    public void testIsClasspathUriDetectsThatItIsDifferentProtocol() throws Exception
    {
        DefaultVersionsHelper helper = createHelper();
        String uri = "http://10.10.10.10/p/a/c/k/a/g/e/resource.res";

        assertThat( DefaultVersionsHelper.isClasspathUri( uri ), CoreMatchers.is( false ) );
    }


    private DefaultVersionsHelper createHelper()
        throws Exception
    {
        return createHelper( null );
    }

    private DefaultVersionsHelper createHelper( org.eclipse.aether.RepositorySystem aetherRepositorySystem )
            throws Exception
    {
        final String resourcePath = "/" + getClass().getPackage().getName().replace( '.', '/' ) + "/rules.xml";
        final String rulesUri = Objects.requireNonNull( getClass().getResource( resourcePath ) ).toExternalForm();
        MavenSession mavenSession = mock( MavenSession.class );
        when( mavenSession.getCurrentProject() ).thenReturn( mock( MavenProject.class ) );
        when( mavenSession.getCurrentProject().getRemotePluginRepositories() )
                .thenReturn( emptyList() );
        when( mavenSession.getCurrentProject().getRemotePluginRepositories() )
                .thenReturn( emptyList() );
        return new DefaultVersionsHelper.Builder()
                .withRepositorySystem( lookup( RepositorySystem.class ) )
                .withArtifactResolver( new DefaultArtifactResolver() )
                .withAetherRepositorySystem( aetherRepositorySystem )
                .withLocalRepository( new DefaultArtifactRepository(
                        "", "", new DefaultRepositoryLayout() ) )
                .withWagonManager( lookup( WagonManager.class ) )
                .withSettings( new Settings() )
                .withServerId( "" )
                .withRulesUri( rulesUri )
                .withLog( mock( Log.class ) )
                .withMavenSession( mavenSession )
                .withMojoExecution( mock( MojoExecution.class ) ).build();
    }

    @Test
    public void testIgnoredVersionsShouldBeTheOnlyPresentInAnEmptyRuleSet()
            throws MojoExecutionException, IllegalAccessException
    {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog( new SystemStreamLog() )
                .withIgnoredVersions( Arrays.asList( ".*-M.", ".*-SNAPSHOT" ) )
                .build();
        RuleSet ruleSet = (RuleSet) getVariableValueFromObject( versionsHelper, "ruleSet" );
        assertThat( ruleSet.getIgnoreVersions(), hasSize( 2 ) );
        assertThat( ruleSet.getIgnoreVersions().stream().map( IgnoreVersion::getVersion )
                .collect( Collectors.toList() ), containsInAnyOrder( ".*-M.", ".*-SNAPSHOT" ) );
    }

    @Test
    public void testDefaultsShouldBePresentInAnEmptyRuleSet()
            throws MojoExecutionException, IllegalAccessException
    {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog( new SystemStreamLog() )
                .withIgnoredVersions( singletonList( ".*-M." ) )
                .build();
        RuleSet ruleSet = (RuleSet) getVariableValueFromObject( versionsHelper, "ruleSet" );
        assertThat( ruleSet.getComparisonMethod(), is( "maven" ) );
    }

    @Test
    public void testIgnoredVersionsShouldExtendTheRuleSet() throws MojoExecutionException, IllegalAccessException
    {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog( new SystemStreamLog() )
                .withRuleSet( new RuleSet()
                {{
                    setIgnoreVersions( new ArrayList<>( singletonList( new IgnoreVersion()
                    {{
                        setVersion( "1.0.0" );
                    }} ) ) );
                    setRules( singletonList( new Rule()
                    {{
                        setGroupId( "org.slf4j" );
                        setArtifactId( "slf4j-api" );
                        setIgnoreVersions( singletonList( new IgnoreVersion()
                        {{
                            setType( "regex" );
                            setVersion( "^[^1]\\.*" );
                        }} ) );
                    }} ) );
                }} )
                .withIgnoredVersions( Arrays.asList( ".*-M.", ".*-SNAPSHOT" ) )
                .build();
        RuleSet ruleSet = (RuleSet) getVariableValueFromObject( versionsHelper, "ruleSet" );
        assertThat( ruleSet.getIgnoreVersions(), hasSize( 3 ) );
        assertThat( ruleSet.getIgnoreVersions().stream().map( IgnoreVersion::getVersion )
                .collect( Collectors.toList() ), containsInAnyOrder( ".*-M.", ".*-SNAPSHOT", "1.0.0" ) );
    }

}
