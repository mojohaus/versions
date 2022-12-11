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

package org.codehaus.mojo.versions.model;

import java.util.Objects;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

/**
 * Convenience class for quickly creating {@link IgnoreVersion} instances.
 */
public class TestIgnoreVersions extends IgnoreVersion {
    public static final String TYPE_REGEX = "regex";

    public static final String TYPE_EXACT = "exact";

    /**
     * Sets the given type returning the modified instance.
     *
     * @param type required type
     * @return modified instance
     */
    public TestIgnoreVersions withType(String type) {
        setType(type);
        return this;
    }

    /**
     * Sets the given version returning the modified instance.
     *
     * @param version required version
     * @return modified instance
     */
    public TestIgnoreVersions withVersion(String version) {
        setVersion(version);
        return this;
    }

    /**
     * Produces a {@linkplain Matcher} instance to match against {@linkplain IgnoreVersion} instances,
     * which don't have {@linkplain #equals(Object)} and {@linkplain #hashCode()}
     * @param ignoreVersion this ignoreVersion object
     * @return Matcher returning true if another object matches the given instance
     * @param <P> class of the ignoreVersion instance
     */
    public static <P extends IgnoreVersion> Matcher<P> matches(P ignoreVersion) {
        return new TypeSafeMatcher<P>() {
            @Override
            public void describeTo(Description description) {
                description.appendText(Objects.toString(ignoreVersion));
            }

            @Override
            protected void describeMismatchSafely(P other, Description description) {
                description.appendText(Objects.toString(other));
            }

            @Override
            protected boolean matchesSafely(P other) {
                return Objects.equals(ignoreVersion.getType(), other.getType())
                        && Objects.equals(ignoreVersion.getVersion(), other.getVersion());
            }
        };
    }
}
