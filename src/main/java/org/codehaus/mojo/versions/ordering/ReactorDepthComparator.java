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
*  http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
*/

import org.apache.maven.model.Model;
import org.codehaus.mojo.versions.api.PomHelper;

import java.util.Comparator;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 14:54:42
 */
public class ReactorDepthComparator
        implements Comparator {
    private final Map reactor;

    public ReactorDepthComparator(Map reactor) {
        this.reactor = reactor;
    }

    public int compare(Object o1, Object o2) {
        final Model m1 = (Model) reactor.get(o1);
        final Model m2 = (Model) reactor.get(o2);
        final int d1 = PomHelper.getReactorParentCount(reactor, m1);
        final int d2 = PomHelper.getReactorParentCount(reactor, m2);
        if (d1 < d2) {
            return -1;
        } else if (d1 > d2) {
            return 1;
        }
        return 0;
    }
}
