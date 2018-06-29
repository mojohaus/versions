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

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.DefaultArtifactFactory;
import org.apache.maven.artifact.manager.DefaultWagonManager;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.repository.DefaultArtifactRepository;
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.path.DefaultPathTranslator;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.artifact.MavenMetadataSource;
import org.apache.maven.settings.Settings;
import org.apache.maven.shared.artifact.resolve.internal.DefaultArtifactResolver;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.providers.file.FileWagon;
import org.apache.maven.wagon.repository.Repository;
import org.codehaus.mojo.versions.Property;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.hamcrest.CoreMatchers;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;
import static org.junit.Assert.*;
import static org.junit.matchers.JUnitMatchers.hasItems;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.same;

/**
 * Test {@link DefaultVersionsHelper}
 */
public class DefaultVersionsHelperTest
{

    @Test
    public void testPerRuleVersionsIgnored() throws Exception
    {
        final ArtifactMetadataSource metadataSource = mock( ArtifactMetadataSource.class );
        final Artifact artifact = mock( Artifact.class );
        when( artifact.getGroupId() ).thenReturn( "com.mycompany.maven" );
        when( artifact.getArtifactId() ).thenReturn( "artifact-one" );
        
        final List<ArtifactVersion> artifactVersions = new ArrayList<ArtifactVersion>();
        
        artifactVersions.add( new DefaultArtifactVersion( "one" ) );
        artifactVersions.add( new DefaultArtifactVersion( "two" ) );
        final ArtifactVersion three = new DefaultArtifactVersion( "three" );
        artifactVersions.add( three );
        final ArtifactVersion oneTwoHundred = new DefaultArtifactVersion( "1.200" );
        artifactVersions.add( oneTwoHundred );
        final ArtifactVersion illegal = new DefaultArtifactVersion( "illegalVersion" );
        artifactVersions.add( illegal );

        when( metadataSource.retrieveAvailableVersions( same( artifact ), any( ArtifactRepository.class ), anyList() ) )
            .thenReturn( artifactVersions );
        
        VersionsHelper helper = createHelper( metadataSource );
        
        final ArtifactVersions versions = helper.lookupArtifactVersions( artifact, true );
        
        final List<ArtifactVersion> actual = asList( versions.getVersions( true ) );
        
        assertEquals( 3, actual.size() );
        assertThat( actual, hasItems( three, oneTwoHundred, illegal ) );
    }

    @Test
    public void testGlobalRuleVersionsIgnored() throws Exception
    {
        final ArtifactMetadataSource metadataSource = mock( ArtifactMetadataSource.class );
        final Artifact artifact = mock( Artifact.class );
        when( artifact.getGroupId() ).thenReturn( "other.company" );
        when( artifact.getArtifactId() ).thenReturn( "artifact-two" );
        
        final List<ArtifactVersion> artifactVersions = new ArrayList<ArtifactVersion>();
        
        final ArtifactVersion one = new DefaultArtifactVersion( "one" );
        final ArtifactVersion two = new DefaultArtifactVersion( "two" );
        final ArtifactVersion three = new DefaultArtifactVersion( "three" );
        artifactVersions.add( one );
        artifactVersions.add( two );
        artifactVersions.add( new DefaultArtifactVersion( "three-alpha" ) );
        artifactVersions.add( new DefaultArtifactVersion( "three-beta" ) );
        artifactVersions.add( three );
        final ArtifactVersion illegal = new DefaultArtifactVersion( "illegalVersion" );
        artifactVersions.add( illegal );

        when(metadataSource.retrieveAvailableVersions( same( artifact ), any( ArtifactRepository.class ), anyList() ) )
            .thenReturn( artifactVersions );
        
        VersionsHelper helper = createHelper( metadataSource );
        
        final ArtifactVersions versions = helper.lookupArtifactVersions( artifact, true );
        
        final List<ArtifactVersion> actual = asList( versions.getVersions( true ) );
        
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
    public void testMVERSIONS159_ExcludedAndNotIncluded()
        throws MojoExecutionException
    {
        VersionsHelper helper = createHelper();
        MavenProject project = null;

        Property[] propertyDefinitions = new Property[] {
            new Property( "bar.version" )
        };
        // should not throw an IllegalStateException
        Map<Property, PropertyVersions> result = helper.getVersionPropertiesMap( project, propertyDefinitions, "foo.version", "bar.version", false );
        assertTrue( result.isEmpty() );
    }

    @Test
    public void testIsClasspathUriDetectsClassPathProtocol() throws MojoExecutionException {
        DefaultVersionsHelper helper = createHelper();
        String uri = "classpath:/p/a/c/k/a/g/e/resource.res";

        assertThat(DefaultVersionsHelper.isClasspathUri(uri), CoreMatchers.is(true));
    }

    @Test
    public void testIsClasspathUriDetectsThatItIsDifferentProtocol() throws MojoExecutionException {
        DefaultVersionsHelper helper = createHelper();
        String uri = "http://10.10.10.10/p/a/c/k/a/g/e/resource.res";

        assertThat(DefaultVersionsHelper.isClasspathUri(uri), CoreMatchers.is(false));
    }


    private DefaultVersionsHelper createHelper()
        throws MojoExecutionException
    {
        return createHelper( new MavenMetadataSource() );
    }
    
    private DefaultVersionsHelper createHelper( ArtifactMetadataSource metadataSource ) throws MojoExecutionException
    {
        final String resourcePath = "/" + getClass().getPackage().getName().replace( '.', '/' ) + "/rules.xml";
        final String rulesUri = getClass().getResource( resourcePath ).toExternalForm();
        DefaultVersionsHelper helper = createHelper( rulesUri, metadataSource );
        return helper;
    }

    private DefaultVersionsHelper createHelper( String rulesUri, ArtifactMetadataSource metadataSource )
        throws MojoExecutionException
    {
        final DefaultWagonManager wagonManager = new DefaultWagonManager()
        {
            @Override
            public AuthenticationInfo getAuthenticationInfo( String id )
            {
                return new AuthenticationInfo();
            }

            public Wagon getWagon( Repository repository )
            {
                return new FileWagon();
            }
        };

        DefaultVersionsHelper helper =
            new DefaultVersionsHelper( new DefaultArtifactFactory(), new DefaultArtifactResolver(), metadataSource, new ArrayList<ArtifactRepository>(),
                                       new ArrayList<ArtifactRepository>(),
                                       new DefaultArtifactRepository( "", "", new DefaultRepositoryLayout() ),
                                       wagonManager, new Settings(), "", rulesUri, mock( Log.class ), mock( MavenSession.class ),
                                       new DefaultPathTranslator());
        return helper;
    }

}
