package org.codehaus.mojo.versions.xml;

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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.api.AbstractVersionDetails;
import org.codehaus.mojo.versions.api.Segment;

import static java.util.Optional.of;
import static java.util.Optional.ofNullable;

/**
 * Common utils for Xml report renderers
 */
class CommonXmlReportRendererUtils
{
    static void setSection( AbstractVersionDetails versions, Segment segment, Consumer<List<String>> setterFunction )
    {
        ofNullable( versions.getAllUpdates( of( segment ) ) )
                .map( v -> Arrays.stream( v )
                        .map( ArtifactVersion::toString )
                        .collect( Collectors.toList() ) )
                .ifPresent( setterFunction );
    }

    static String statusFor( String lastVersion, Collection<?> incrementals, Collection<?> minors )
    {
        return lastVersion == null
                ? "no new available"
                : incrementals != null && !incrementals.isEmpty()
                    ? "incremental available"
                    : minors != null && !minors.isEmpty()
                        ? "minor available"
                        : "major available";
    }
}
