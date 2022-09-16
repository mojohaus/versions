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

package org.codehaus.mojo.versions.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.doxia.tools.SiteTool;
import org.apache.maven.doxia.tools.SiteToolException;
import org.codehaus.plexus.i18n.I18N;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Various mock creating utilities
 */
public class MockUtils
{
    private static final Map<String, String[]> DEFAULT_VERSION_MAP = new HashMap<String, String[]>()
    {{
        put( "artifactA", new String[] {"1.0.0", "2.0.0"} );
        put( "artifactB", new String[] {"1.0.0", "1.1.0"} );
        put( "artifactC", new String[] {"1.0.0"} );
    }};

    /**
     * Creates a mocked  {@linkplain ArtifactMetadataSource}, providing the default version set
     * @return mocked {@linkplain ArtifactMetadataSource}
     */
    public static ArtifactMetadataSource mockArtifactMetadataSource()
    {
        return mockArtifactMetadataSource( DEFAULT_VERSION_MAP );
    }

    /**
     * Creates a mocked  {@linkplain ArtifactMetadataSource}, providing the version map given in the argument
     * @param versionMap requested version map
     * @return mocked {@linkplain ArtifactMetadataSource}
     */
    public static ArtifactMetadataSource mockArtifactMetadataSource( Map<String, String[]> versionMap )
    {
        ArtifactMetadataSource artifactMetadataSource = mock( ArtifactMetadataSource.class );
        try
        {
            when( artifactMetadataSource.retrieveAvailableVersions( any( Artifact.class ), any(), any() ) ).then(
                    invocation ->
                    {
                        Artifact artifact = invocation.getArgument( 0 );
                        return versionMap.entrySet().stream()
                                .filter( e -> e.getKey().equals( artifact.getArtifactId() ) )
                                .findAny()
                                .map( e -> Arrays.stream( e.getValue() )
                                        .map( DefaultArtifactVersion::new )
                                        .collect( ArrayList::new, ArrayList::add, ArrayList::add ) )
                                .orElse( null ); // should tell us if we haven't populated all cases in the test
                    } );
        }
        catch ( ArtifactMetadataRetrievalException e )
        {
            throw new RuntimeException( e );
        }
        return artifactMetadataSource;
    }

    public static I18N mockI18N()
    {
        I18N i18n = mock( I18N.class );
        when( i18n.getString( anyString(), any(), anyString() ) ).thenAnswer(
            invocation -> invocation.getArgument( 2 ) );
        return i18n;
    }

    public static SiteTool mockSiteTool()
    {
        Artifact skinArtifact = mock( Artifact.class );
        when( skinArtifact.getId() ).thenReturn( "" );
        SiteTool siteTool = mock( SiteTool.class );
        try
        {
            when( siteTool.getSkinArtifactFromRepository( any(), any(), any() ) ).thenReturn( skinArtifact );
        }
        catch ( SiteToolException e )
        {
            throw new RuntimeException( e );
        }
        return siteTool;
    }
}
