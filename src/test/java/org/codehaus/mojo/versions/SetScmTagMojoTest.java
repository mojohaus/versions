package org.codehaus.mojo.versions;

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

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.codehaus.mojo.versions.utils.BaseMojoTestCase;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.matchesPattern;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
public class SetScmTagMojoTest extends BaseMojoTestCase
{
    @Test
    public void testNewScmValues() throws Exception
    {
        Path pomFile = Paths.get( "target/test-classes/org/codehaus/mojo/set-scm-tag/issue-342-pom.xml" );
        createMojo( "set-scm-tag", pomFile.toString() )
                .execute();
        String output = String.join( "", Files.readAllLines( pomFile ) )
                .replaceAll( "\\s*", "" );
        assertThat( output, allOf(
                        matchesPattern( ".*<scm>.*<tag>\\s*newTag\\s*</tag>.*</scm>.*" ),
                        matchesPattern( ".*<scm>.*<url>\\s*url\\s*</url>.*</scm>.*" ),
                        matchesPattern( ".*<scm>.*<connection>\\s*connection\\s*</connection>.*</scm>.*" ),
                        matchesPattern( ".*<scm>.*<developerConnection>\\s*"
                                + "developerConnection\\s*</developerConnection>.*</scm>.*" )
                )
        );
    }
}
