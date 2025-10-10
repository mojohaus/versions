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

import java.io.File;
import java.util.Comparator;
import java.util.Map;

import org.apache.maven.model.Model;
import org.codehaus.mojo.versions.api.PomHelper;

/**
 * Compares project paths relative to the base directory based on their depth in a reactor.
 * <p>
 * The comparator uses the reactor {@link Model} instances to determine the number of
 * parent levels for each project and orders shallower projects before deeper ones. If
 * two projects have the same depth the result falls back to comparing their GAV strings.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 14:54:42
 */
public class ReactorDepthComparator implements Comparator<File> {
    private final Map<File, Model> reactor;

    /**
     * Creates a new comparator using the given reactor map.
     *
     * @param reactor map of project base directories to their Maven {@link Model} instances; must not be {@code null}
     */
    public ReactorDepthComparator(Map<File, Model> reactor) {
        this.reactor = reactor;
    }

    /**
     * Compare two project base directories by their reactor depth, then by their GAV.
     *
     * @param o1 the first project base directory to compare
     * @param o2 the second project base directory to compare
     * @return negative if {@code o1} is ordered before {@code o2}, positive if after, zero if equal
     */
    @Override
    public int compare(File o1, File o2) {
        final Model m1 = reactor.get(o1);
        final Model m2 = reactor.get(o2);
        final int d1 = PomHelper.getReactorParentCount(reactor, m1);
        final int d2 = PomHelper.getReactorParentCount(reactor, m2);
        if (d1 < d2) {
            return -1;
        } else if (d1 > d2) {
            return 1;
        }
        return PomHelper.getGAV(m1).compareTo(PomHelper.getGAV(m2));
    }
}
