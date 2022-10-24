package org.codehaus.mojo.versions.utils;

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

import org.eclipse.aether.version.Version;

/**
 * Stubs the {@link Version}
 */
public class VersionStub implements Version
{
    private final String version;

    /**
     * Creates a new instance with the given version string
     * @param version version to be set
     */
    public VersionStub( String version )
    {
        assert version != null;
        this.version = version;
    }

    @Override
    public String toString()
    {
        return version;
    }

    @Override
    public int compareTo( Version o )
    {
        return o != null
                ? version.compareTo( o.toString() )
                : 1;
    }

    @Override
    public boolean equals( Object o )
    {
        if ( this == o )
        {
            return true;
        }

        if ( !( o instanceof Version ) )
        {
            return false;
        }

        return version.equals( o.toString() );
    }

    @Override
    public int hashCode()
    {
        return version.hashCode();
    }
}
