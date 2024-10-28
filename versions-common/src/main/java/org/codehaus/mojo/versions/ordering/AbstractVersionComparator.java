package org.codehaus.mojo.versions.ordering;

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

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;

/**
 * Base class for version comparators.
 *
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionComparator implements VersionComparator {
    @Override
    public abstract int compare(ArtifactVersion o1, ArtifactVersion o2);

    @Override
    public final int getSegmentCount(ArtifactVersion v) {
        if (v == null) {
            return 0;
        }
        if (ArtifactUtils.isSnapshot(v.toString())) {
            return innerGetSegmentCount(VersionComparators.stripSnapshot(v));
        }
        return innerGetSegmentCount(v);
    }

    protected abstract int innerGetSegmentCount(ArtifactVersion v);

    /**
     * Returns a hash code value for the comparator class.
     *
     * @return the hash code.
     */
    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    /**
     * Returns true if this object is the same type of comparator as the parameter.
     *
     * @param obj the reference object with which to compare.
     * @return <code>true</code> if this object is the same as the obj argument; <code>false</code> otherwise.
     * @see #hashCode()
     * @see java.util.Hashtable
     */
    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj != null && getClass().equals(obj.getClass()));
    }
}
