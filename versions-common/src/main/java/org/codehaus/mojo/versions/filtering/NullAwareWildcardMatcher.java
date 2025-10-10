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

/**
 * An extended version of the {@link WildcardMatcher} where an additional special keyword {@code null}
 * will match tokens which are null.
 */
public class NullAwareWildcardMatcher extends WildcardMatcher {
    /**
     * Null keyword
     */
    public static final String NULL_KEYWORD = "null";

    /**
     * Creates a new instance of the given pattern
     *
     * @param pattern pattern to be matched against
     */
    public NullAwareWildcardMatcher(String pattern) {
        super(pattern);
    }

    @Override
    public boolean test(String token) {
        if (NULL_KEYWORD.equals(getPattern())) {
            return token == null;
        }

        return super.test(token);
    }
}
