package org.codehaus.mojo.versions.api;

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

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;

import static java.util.Optional.ofNullable;

/**
 * Builds {@link org.codehaus.mojo.versions.api.PropertyVersions} instances.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public class PropertyVersionsBuilder {
    private final String name;

    private final String profileId;

    private final Set<ArtifactAssociation> associations;

    private final Map<String, Boolean> upperBounds = new LinkedHashMap<>();

    private final Map<String, Boolean> lowerBounds = new LinkedHashMap<>();

    private final Log log;

    private final VersionsHelper helper;

    private ArtifactVersion currentVersion;

    private VersionRange currentVersionRange;

    /**
     * Constructs a new {@link org.codehaus.mojo.versions.api.PropertyVersions}.
     *
     * @param profileId The profileId.
     * @param name The property name.
     * @param helper The {@link org.codehaus.mojo.versions.api.DefaultVersionsHelper}.
     */
    PropertyVersionsBuilder(VersionsHelper helper, String profileId, String name, Log log) {
        this.helper = helper;
        this.profileId = profileId;
        this.name = name;
        this.associations = new TreeSet<>();
        this.log = log;
    }

    public PropertyVersionsBuilder withAssociation(Artifact artifact, boolean usePluginRepositories) {
        associations.add(new DefaultArtifactAssociation(artifact, usePluginRepositories));
        return this;
    }

    public void clearAssociations() {
        associations.clear();
    }

    public boolean isAssociated() {
        return !associations.isEmpty();
    }

    public PropertyVersions build() throws VersionRetrievalException {
        SortedSet<ArtifactVersion> resolvedVersions = resolveAssociatedVersions(helper, associations);
        PropertyVersions instance = new PropertyVersions(profileId, name, log, associations, resolvedVersions);
        instance.setCurrentVersion(currentVersion);
        instance.setCurrentVersionRange(currentVersionRange);
        return instance;
    }

    public String getName() {
        return name;
    }

    public String getVersionRange() {
        if (lowerBounds.isEmpty() && upperBounds.isEmpty()) {
            return null;
        }

        Optional<Pair<ArtifactVersion, Boolean>> lowerBound = lowerBounds.entrySet().stream()
                .min(Map.Entry.comparingByKey())
                .map(e -> Pair.of(e.getKey(), e.getValue()))
                .map(p -> Pair.of(ArtifactVersionService.getArtifactVersion(p.getKey()), p.getValue()));

        Optional<Pair<ArtifactVersion, Boolean>> upperBound = upperBounds.entrySet().stream()
                .max(Map.Entry.comparingByKey())
                .map(e -> Pair.of(e.getKey(), e.getValue()))
                .map(p -> Pair.of(ArtifactVersionService.getArtifactVersion(p.getKey()), p.getValue()));

        return (lowerBound.map(Pair::getValue).orElse(false) ? '[' : '(')
                + lowerBound.map(Pair::getKey).map(Object::toString).orElse("")
                + ','
                + upperBound.map(Pair::getKey).map(Object::toString).orElse("")
                + (upperBound.map(Pair::getValue).orElse(false) ? ']' : ')');
    }

    public PropertyVersionsBuilder withLowerBound(String lowerBound, boolean includeLower) {
        lowerBounds.compute(lowerBound, (__, oldValue) -> ofNullable(oldValue).orElse(true) && includeLower);
        return this;
    }

    public PropertyVersionsBuilder withUpperBound(String upperBound, boolean includeUpper) {
        upperBounds.compute(upperBound, (__, oldValue) -> ofNullable(oldValue).orElse(true) && includeUpper);
        return this;
    }

    public PropertyVersionsBuilder withCurrentVersion(ArtifactVersion currentVersion) {
        this.currentVersion = currentVersion;
        return this;
    }

    public PropertyVersionsBuilder withCurrentVersionRange(VersionRange currentVersionRange) {
        this.currentVersionRange = currentVersionRange;
        return this;
    }

    private SortedSet<ArtifactVersion> resolveAssociatedVersions(
            VersionsHelper helper, Set<ArtifactAssociation> associations) throws VersionRetrievalException {
        SortedSet<ArtifactVersion> result = new TreeSet<>();
        for (ArtifactAssociation association : associations) {
            ArtifactVersions artifactVersions =
                    helper.lookupArtifactVersions(association.getArtifact(), association.isUsePluginRepositories());
            List<ArtifactVersion> associatedVersions = Arrays.asList(artifactVersions.getVersions(true));
            if (result.isEmpty()) {
                result.addAll(associatedVersions);
            } else {
                result.retainAll(associatedVersions);
            }
        }
        return result;
    }
}
