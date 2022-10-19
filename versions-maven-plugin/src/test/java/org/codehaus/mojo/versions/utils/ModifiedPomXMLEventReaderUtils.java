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

import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

/**
 * <p>Utilities for the {@link ModifiedPomXMLEventReader} class</p>
 *
 * @author Andrzej Jarmoniuk
 */
public class ModifiedPomXMLEventReaderUtils
{
    public static <P extends ModifiedPomXMLEventReader> Matcher<P> matches( String pattern )
    {
        return new TypeSafeMatcher<P>()
        {
            @Override
            public void describeTo( Description description )
            {
                description.appendText( pattern );
            }

            @Override
            protected void describeMismatchSafely( P pom, Description description )
            {
                description.appendText( asString( pom ) );
            }

            @Override
            protected boolean matchesSafely( P pom )
            {
                return pattern.matches( asString( pom ) );
            }

            private String asString( P pom )
            {
                return pom.asStringBuilder().toString().replaceAll( "\\s", "" );
            }
        };
    }
}
