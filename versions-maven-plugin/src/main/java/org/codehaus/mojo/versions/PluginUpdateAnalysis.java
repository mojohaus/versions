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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.maven.artifact.versioning.ArtifactVersion;

/** Immutable, presentation-independent result of analyzing one plugin declaration. */
final class PluginUpdateAnalysis {
    private final String effectiveVersion;
    private final ArtifactVersion compatibleVersion;
    private final ArtifactVersion requiredMavenVersion;
    private final Map<ArtifactVersion, ArtifactVersion> mavenUpgrades;
    private final Map<ArtifactVersion, String> upgradeFromVersions;

    PluginUpdateAnalysis(
            String effectiveVersion,
            ArtifactVersion compatibleVersion,
            ArtifactVersion requiredMavenVersion,
            Map<ArtifactVersion, ArtifactVersion> mavenUpgrades,
            Map<ArtifactVersion, String> upgradeFromVersions) {
        this.effectiveVersion = effectiveVersion;
        this.compatibleVersion = compatibleVersion;
        this.requiredMavenVersion = requiredMavenVersion;
        this.mavenUpgrades = Collections.unmodifiableMap(new LinkedHashMap<>(mavenUpgrades));
        this.upgradeFromVersions = Collections.unmodifiableMap(new LinkedHashMap<>(upgradeFromVersions));
    }

    String getEffectiveVersion() {
        return effectiveVersion;
    }

    ArtifactVersion getCompatibleVersion() {
        return compatibleVersion;
    }

    ArtifactVersion getRequiredMavenVersion() {
        return requiredMavenVersion;
    }

    Map<ArtifactVersion, ArtifactVersion> getMavenUpgrades() {
        return mavenUpgrades;
    }

    Map<ArtifactVersion, String> getUpgradeFromVersions() {
        return upgradeFromVersions;
    }
}
