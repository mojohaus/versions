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

import java.util.Comparator;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.model.Dependency;

/**
 * A comparator used to sort dependencies by group id, artifact id and finally version.
 *
 * @since 1.0-alpha-1
 */
public enum DependencyComparator implements Comparator<Dependency>
{
    INSTANCE;

    /**
     * @param d1 the first dependency
     * @param d2 the second dependency.
     * @return the comparison result
     * @see java.util.Comparator#compare(Object, Object)
     * @since 1.0-alpha-1
     */
    @SuppressWarnings( "checkstyle:InnerAssignment" )
    public int compare( Dependency d1, Dependency d2 )
    {
        int r;
        return d1 == d2
                ? 0
                : d1 == null
                    ? 1
                    : d2 == null
                        ? -1
                        : ( r = StringUtils.compare( d1.getGroupId(), d2.getGroupId() ) ) != 0
                            ? r
                            : ( r = StringUtils.compare( d1.getArtifactId(), d2.getArtifactId() ) ) != 0
                                ? r
                                : StringUtils.compare( d1.getVersion(), d2.getVersion() );
    }

}
