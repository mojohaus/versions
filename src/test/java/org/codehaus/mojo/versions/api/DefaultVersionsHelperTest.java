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

import junit.framework.TestCase;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.DefaultArtifactFactory;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.manager.DefaultWagonManager;
import org.apache.maven.artifact.manager.WagonConfigurationException;
import org.apache.maven.artifact.metadata.ArtifactMetadata;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.repository.DefaultArtifactRepository;
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.OverConstrainedVersionException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.artifact.MavenMetadataSource;
import org.apache.maven.settings.Settings;
import org.apache.maven.wagon.UnsupportedProtocolException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.providers.file.FileWagon;
import org.apache.maven.wagon.repository.Repository;
import org.codehaus.mojo.versions.ordering.VersionComparators;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Test {@link DefaultVersionsHelper}
 */
public class DefaultVersionsHelperTest
    extends TestCase
{
    public void testWildcardMatching()
        throws Exception
    {
        assertTrue( DefaultVersionsHelper.exactMatch( "*", "com.foo.bar" ) );
        assertFalse( DefaultVersionsHelper.exactMatch( "com.bar*", "com-bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "com?foo.bar", "com.foo.bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "co*.foo.b?r", "com.foo.bar" ) );
        assertTrue( DefaultVersionsHelper.exactMatch( "c*oo*r", "com.foo.bar" ) );
    }

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

    public void testIsIncludedWithNoRules()
        throws Exception
    {
        VersionsHelper instance = createHelper();

        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null, null ) );
    }

    public void testIsIncludedWithIncludeRulesOnly()
        throws Exception
    {
        VersionsHelper instance = createHelper();

        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "yourgroup,mygroup", null, null, null ) );
        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "yourgroup,mygroup,theirgroup", null, null,
                                 null ) );
        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "mygroup,theirgroup", null, null, null ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "mygroup", null, null, null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "yourgroup,theirgroup", null, null,
                                 null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "yourgroup,ourgroup,theirgroup", null,
                                 null, null ) );

        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, "yourartifact,myartifact", null,
                                 null ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null,
                                         "yourartifact,myartifact,theirartifact", null, null ) );
        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, "myartifact,theirartifact", null,
                                 null ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, "myartifact", null, null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, "yourartifact,theirartifact", null,
                                 null ) );
        assertFalse( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null,
                                          "yourartifact,ourartifact,theirartifact", null, null ) );

        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "mygroup", "myartifact", null, null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "mygroup", "yourartifact", null, null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), "yourgroup", "myartifact", null, null ) );
    }

    public void testIsIncludedWithExcludeRulesOnly()
        throws Exception
    {
        VersionsHelper instance = createHelper();

        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "yourgroup,mygroup", null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "yourgroup,mygroup,theirgroup",
                                 null ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "mygroup,theirgroup", null ) );
        assertFalse( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "mygroup", null ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "yourgroup,theirgroup",
                                         null ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null,
                                         "yourgroup,ourgroup,theirgroup", null ) );

        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null, "yourartifact,myartifact" ) );
        assertFalse( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null,
                                         "yourartifact,myartifact,theirartifact" ) );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null, "myartifact,theirartifact") );
        assertFalse( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null, "myartifact") );
        assertTrue(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null, "yourartifact,theirartifact" ) );
        assertTrue( instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, null,
                                          "yourartifact,ourartifact,theirartifact") );

        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "mygroup", "myartifact") );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "mygroup", "yourartifact") );
        assertFalse(
            instance.isIncluded( newMockArtifact( "mygroup", "myartifact" ), null, null, "yourgroup", "myartifact") );

    }

    private Artifact newMockArtifact( final String groupId, final String artifactId )
    {
        return new Artifact()
        {
            public String getGroupId()
            {
                return groupId;
            }

            public String getArtifactId()
            {
                return artifactId;
            }

            public String getVersion()
            {
                return null;
            }

            public void setVersion( String s )
            {

            }

            public String getScope()
            {
                return null;
            }

            public String getType()
            {
                return null;
            }

            public String getClassifier()
            {
                return null;
            }

            public boolean hasClassifier()
            {
                return false;
            }

            public File getFile()
            {
                return null;
            }

            public void setFile( File file )
            {

            }

            public String getBaseVersion()
            {
                return null;
            }

            public void setBaseVersion( String s )
            {

            }

            public String getId()
            {
                return null;
            }

            public String getDependencyConflictId()
            {
                return null;
            }

            public void addMetadata( ArtifactMetadata artifactMetadata )
            {

            }

            public Collection getMetadataList()
            {
                return null;
            }

            public void setRepository( ArtifactRepository artifactRepository )
            {

            }

            public ArtifactRepository getRepository()
            {
                return null;
            }

            public void updateVersion( String s, ArtifactRepository artifactRepository )
            {

            }

            public String getDownloadUrl()
            {
                return null;
            }

            public void setDownloadUrl( String s )
            {

            }

            public ArtifactFilter getDependencyFilter()
            {
                return null;
            }

            public void setDependencyFilter( ArtifactFilter artifactFilter )
            {

            }

            public ArtifactHandler getArtifactHandler()
            {
                return null;
            }

            public List getDependencyTrail()
            {
                return null;
            }

            public void setDependencyTrail( List list )
            {

            }

            public void setScope( String s )
            {

            }

            public VersionRange getVersionRange()
            {
                return null;
            }

            public void setVersionRange( VersionRange versionRange )
            {

            }

            public void selectVersion( String s )
            {

            }

            public void setGroupId( String s )
            {

            }

            public void setArtifactId( String s )
            {

            }

            public boolean isSnapshot()
            {
                return false;
            }

            public void setResolved( boolean b )
            {

            }

            public boolean isResolved()
            {
                return false;
            }

            public void setResolvedVersion( String s )
            {

            }

            public void setArtifactHandler( ArtifactHandler artifactHandler )
            {

            }

            public boolean isRelease()
            {
                return false;
            }

            public void setRelease( boolean b )
            {

            }

            public List getAvailableVersions()
            {
                return null;
            }

            public void setAvailableVersions( List list )
            {

            }

            public boolean isOptional()
            {
                return false;
            }

            public void setOptional( boolean b )
            {

            }

            public ArtifactVersion getSelectedVersion()
                throws OverConstrainedVersionException
            {
                return null;
            }

            public boolean isSelectedVersionKnown()
                throws OverConstrainedVersionException
            {
                return false;
            }

            public int compareTo( Object o )
            {
                return 0;
            }
        };
    }

    private VersionsHelper createHelper()
        throws MojoExecutionException
    {
        final String resourcePath = "/" + getClass().getPackage().getName().replace( '.', '/' ) + "/rules.xml";
        final String rulesUri = getClass().getResource( resourcePath ).toExternalForm();
        VersionsHelper helper = createHelper( rulesUri );
        return helper;
    }

    private VersionsHelper createHelper( String rulesUri )
        throws MojoExecutionException
    {
        final DefaultWagonManager wagonManager = new DefaultWagonManager()
        {
            public Wagon getWagon( Repository repository )
                throws UnsupportedProtocolException, WagonConfigurationException
            {
                return new FileWagon();
            }
        };

        VersionsHelper helper =
            new DefaultVersionsHelper( new DefaultArtifactFactory(), new MavenMetadataSource(), new ArrayList(),
                                       new ArrayList(),
                                       new DefaultArtifactRepository( "", "", new DefaultRepositoryLayout() ),
                                       wagonManager, new Settings(), "", rulesUri, null, new MockLog() );
        return helper;
    }

    private static class MockLog
        implements Log
    {
        public boolean isDebugEnabled()
        {
            return false;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public void debug( CharSequence charSequence )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void debug( CharSequence charSequence, Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void debug( Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public boolean isInfoEnabled()
        {
            return false;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public void info( CharSequence charSequence )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void info( CharSequence charSequence, Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void info( Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public boolean isWarnEnabled()
        {
            return false;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public void warn( CharSequence charSequence )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void warn( CharSequence charSequence, Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void warn( Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public boolean isErrorEnabled()
        {
            return false;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public void error( CharSequence charSequence )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void error( CharSequence charSequence, Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public void error( Throwable throwable )
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }
}
