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

import org.apache.maven.model.Plugin;
import org.apache.maven.model.ReportPlugin;

/**
 * A comparator used to sort plugins and report plugins by group id, artifact id and finally version.
 *
 * @since 1.0-beta-1
 */
public enum PluginComparator implements Comparator<Object> {
    /**
     * The singleton instance.
     */
    INSTANCE;

    private static boolean isPluginOrReportPlugin(Object o) {
        return o instanceof Plugin || o instanceof ReportPlugin;
    }

    private static String getGroupId(Object o) {
        return o instanceof Plugin
                ? ((Plugin) o).getGroupId()
                : o instanceof ReportPlugin ? ((ReportPlugin) o).getGroupId() : "";
    }

    private static String getArtifactId(Object o) {
        return o instanceof Plugin
                ? ((Plugin) o).getArtifactId()
                : o instanceof ReportPlugin ? ((ReportPlugin) o).getArtifactId() : "";
    }

    private static String getVersion(Object o) {
        return o instanceof Plugin
                ? ((Plugin) o).getVersion()
                : o instanceof ReportPlugin ? ((ReportPlugin) o).getVersion() : "";
    }

    /**
     * Compares to {@link Plugin} or {@link ReportPlugin} instances.
     *
     * @param o1 the first object
     * @param o2 the second object.
     * @return the comparison result
     * @see java.util.Comparator#compare(Object, Object)
     * @since 1.0-beta-1
     */
    public int compare(Object o1, Object o2) {
        if (!isPluginOrReportPlugin(o1) || !isPluginOrReportPlugin(o2)) {
            throw new IllegalArgumentException(
                    "This comparator can only be used to compare Plugin and ReportPlugin instances");
        }

        if (o1 == o2) {
            return 0;
        }
        return Comparator.nullsLast(Comparator.comparing(PluginComparator::getGroupId)
                        .thenComparing(PluginComparator::getArtifactId)
                        .thenComparing(PluginComparator::getVersion, VersionStringComparator.STRICT))
                .compare(o1, o2);
    }
}
