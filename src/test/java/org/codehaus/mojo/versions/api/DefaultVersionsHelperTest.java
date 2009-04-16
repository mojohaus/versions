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
import org.apache.maven.artifact.manager.DefaultWagonManager;
import org.apache.maven.artifact.manager.WagonConfigurationException;
import org.apache.maven.artifact.repository.DefaultArtifactRepository;
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout;
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

import java.util.ArrayList;

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

        assertTrue( instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), null, null, null, null ) );
    }

    public void testIsIncludedWithIncludeRulesOnly()
        throws Exception
    {
        VersionsHelper instance = createHelper();

        assertTrue(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "yourgroup,mygroup", null, null,
                                 null ) );
        assertTrue(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "yourgroup,mygroup,theirgroup", null, null,
                                 null ) );
        assertTrue(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "mygroup,theirgroup", null, null,
                                 null ) );
        assertTrue(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "mygroup", null, null,
                                 null ) );
        assertFalse(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "yourgroup,theirgroup", null, null,
                                 null ) );
        assertFalse(
            instance.isIncluded( makeArtifact( instance, "mygroup", "myartifact" ), "yourgroup,ourgroup,theirgroup", null, null,
                                 null ) );
    }

    private Artifact makeArtifact( VersionsHelper instance, String artifactId, String groupId )
    {
        return instance.createDependencyArtifact( groupId, artifactId, VersionRange.createFromVersion( "1.0" ), "pom",
                                                  null, "compile", false );
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
