package org.codehaus.mojo.versions.filtering;

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

import java.util.Objects;

import org.apache.maven.model.Dependency;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Hamcrest-Matcher that matches a {@link Dependency} GAV
 */
public class HasGAVMatcher extends TypeSafeMatcher<Dependency>
{
    private final String groupId;
    private final String artifactId;
    private final String version;

    public HasGAVMatcher( String groupId, String artifactId, String version )
    {
        this.groupId = groupId;
        this.artifactId = artifactId;
        this.version = version;
    }

    public static HasGAVMatcher hasGAVOf( Dependency dependency )
    {
        return hasGAV( dependency.getGroupId(), dependency.getArtifactId(), dependency.getVersion() );
    }

    public static HasGAVMatcher hasGAV( String groupId, String artifactId, String version )
    {
        return new HasGAVMatcher( groupId, artifactId, version );
    }

    @Override
    protected boolean matchesSafely( Dependency item )
    {
        boolean result = Objects.equals( groupId, item.getGroupId() )
                && Objects.equals( artifactId, item.getArtifactId() )
                && Objects.equals( version, item.getVersion() );

        return result;
    }

    @Override
    public void describeTo( Description description )
    {
        description.appendText( String.format( "has GAV %s:%s:%s", groupId, artifactId, version ) );
    }
}
