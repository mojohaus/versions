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

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.codehaus.mojo.versions.utils.DefaultArtifactVersionCache;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.lessThan;

/**
 * Abstract base class for {@link MavenVersionComparatorTest}, {@link MercuryVersionComparatorTest},
 * and {@link NumericVersionComparatorTest}
 */
public abstract class VersionComparatorTestBase {
    protected final VersionComparator instance;

    public VersionComparatorTestBase(VersionComparator instance) {
        this.instance = instance;
    }

    protected static ArtifactVersion version(String version) {
        return DefaultArtifactVersionCache.of(version);
    }

    @Test
    public void testVersionComparatorRow1() {
        assertThat(instance.compare(version("1"), version("2")), lessThan(0));
        assertThat(instance.compare(version("1.0"), version("2.0")), lessThan(0));
        assertThat(instance.compare(version("1.0"), version("1.1")), lessThan(0));
        assertThat(instance.compare(version("1.0"), version("1.1")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow2() {
        assertThat(instance.compare(version("1.0"), version("2.0")), lessThan(0));
        assertThat(instance.compare(version("1.0"), version("1.1")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow3() {
        assertThat(instance.compare(version("1.0.0"), version("2.0.0")), lessThan(0));
        assertThat(instance.compare(version("1.0.0"), version("1.1.0")), lessThan(0));
        assertThat(instance.compare(version("1.0.0"), version("1.0.1")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow4() {
        assertThat(instance.compare(version("1.0.0-1"), version("2.0.0-1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-1"), version("1.1.0-1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-1"), version("1.0.1-1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-1"), version("1.0.0-2")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow5() {
        assertThat(instance.compare(version("1.0.0-sp1"), version("2.0.0-sp1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-sp1"), version("1.1.0-sp1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-sp1"), version("1.0.1-sp1")), lessThan(0));
        assertThat(instance.compare(version("1.0.0-sp1"), version("1.0.0-1-sp1")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow6() {
        assertThat(instance.compare(version("foobar"), version("foobar-1")), lessThan(0));
    }

    @Test
    public void testVersionComparatorRow7() {
        assertThat(instance.compare(version("1-alpha-1"), version("2-alpha-1")), lessThan(0));
        assertThat(instance.compare(version("1-alpha-1"), version("1-alpha-2")), lessThan(0));
    }
}
