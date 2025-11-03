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

    private final Set<ArtifactAssociation> associations = new TreeSet<>();

    private final Map<String, Boolean> upperBounds = new LinkedHashMap<>();

    private final Map<String, Boolean> lowerBounds = new LinkedHashMap<>();

    private final Log log;

    private final ResolverAdapter resolverAdapter;

    private ArtifactVersion currentVersion;

    private VersionRange currentVersionRange;

    /**
     * Constructs a new {@link org.codehaus.mojo.versions.api.PropertyVersions}.
     *
     * @param resolverAdapter {@link ResolverAdapter} instance
     * @param profileId The profileId.
     * @param name The property name.
     * @param log The {@link Log} instance.
     */
    PropertyVersionsBuilder(ResolverAdapter resolverAdapter, String profileId, String name, Log log) {
        this.profileId = profileId;
        this.name = name;
        this.log = log;
        this.resolverAdapter = resolverAdapter;
    }

    /**
     * Provides the associated artifact
     * @param artifact artifact associated with the property
     * @param usePluginRepositories whether to use plugin repositories
     * @return builder instance
     */
    public PropertyVersionsBuilder withAssociation(Artifact artifact, boolean usePluginRepositories) {
        associations.add(new DefaultArtifactAssociation(artifact, usePluginRepositories));
        return this;
    }

    /**
     * Clears the set of artifacts with which the property is associated
     */
    public void clearAssociations() {
        associations.clear();
    }

    /**
     * Returns a set of {@link ArtifactAssociation}, listing artifacts with which the property is associated
     * @return a set of {@link ArtifactAssociation}, listing artifacts with which the property is associated
     */
    public Set<ArtifactAssociation> getAssociations() {
        return associations;
    }

    /**
     * Returns {@code true} is the property is associated with any artifact
     * @return {@code true} is the property is associated with any artifact
     */
    public boolean isAssociated() {
        return !associations.isEmpty();
    }

    /**
     * <p>Creates a new instance of {@link PropertyVersions}, based on the values provided to the builder.
     * The object is a mutable view on a property along with all its associated dependencies or plugins.</p>
     * <p>The builder uses {@link ResolverAdapter} to resolve available versions for all associations.</p>
     * @return new {@link PropertyVersions} instance
     * @throws VersionRetrievalException thrown if there are problems retrieving versions of artifacts linked
     * with the property
     */
    public PropertyVersions build() throws VersionRetrievalException {
        SortedSet<ArtifactVersion> resolvedVersions = resolveAssociatedVersions(associations);
        PropertyVersions instance = new PropertyVersions(profileId, name, log, associations, resolvedVersions);
        instance.setCurrentVersion(currentVersion);
        instance.setCurrentVersionRange(currentVersionRange);
        return instance;
    }

    /**
     * Returns the property name
     * @return property name
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the version range
     * @return version range
     */
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

    /**
     * Provides the lower bound of the property version and whether the lower bound should be included
     * @param lowerBound lower bound of the property version
     * @param includeLower whether the lower bound should be included
     * @return builder instance
     */
    public PropertyVersionsBuilder withLowerBound(String lowerBound, boolean includeLower) {
        lowerBounds.compute(lowerBound, (__, oldValue) -> ofNullable(oldValue).orElse(true) && includeLower);
        return this;
    }

    /**
     * Provides the upper bound of the property version and whether the upper bound should be included
     * @param upperBound upper bound of the property version
     * @param includeUpper whether the upper bound should be included
     * @return builder instance
     */
    public PropertyVersionsBuilder withUpperBound(String upperBound, boolean includeUpper) {
        upperBounds.compute(upperBound, (__, oldValue) -> ofNullable(oldValue).orElse(true) && includeUpper);
        return this;
    }

    /**
     * Provides the current version of the linked artifact
     * @param currentVersion current version of the linked artifact
     * @return builder instance
     */
    public PropertyVersionsBuilder withCurrentVersion(ArtifactVersion currentVersion) {
        this.currentVersion = currentVersion;
        return this;
    }

    /**
     * Provides the current version range of the linked artifact
     * @param currentVersionRange current version range of the linked artifact
     * @return builder instance
     */
    public PropertyVersionsBuilder withCurrentVersionRange(VersionRange currentVersionRange) {
        this.currentVersionRange = currentVersionRange;
        return this;
    }

    private SortedSet<ArtifactVersion> resolveAssociatedVersions(
            ResolverAdapter resolverAdapter, Set<ArtifactAssociation> associations) throws VersionRetrievalException {
        SortedSet<ArtifactVersion> result = new TreeSet<>();
        for (ArtifactAssociation association : associations) {
            ArtifactVersions artifactVersions = resolverAdapter.resolveArtifactVersions(
                    association.getArtifact(),
                    association.isUsePluginRepositories(),
                    !association.isUsePluginRepositories());
            List<ArtifactVersion> associatedVersions = Arrays.asList(artifactVersions.getVersions(true));
            if (result.isEmpty()) {
                result.addAll(associatedVersions);
            } else {
                result.retainAll(associatedVersions);
            }
        }
        return result;
    }

    @Deprecated
    private SortedSet<ArtifactVersion> resolveAssociatedVersions(Set<ArtifactAssociation> associations)
            throws VersionRetrievalException {
        SortedSet<ArtifactVersion> result = new TreeSet<>();
        for (ArtifactAssociation association : associations) {
            ArtifactVersions artifactVersions = resolverAdapter.resolveArtifactVersions(
                    association.getArtifact(),
                    association.isUsePluginRepositories(),
                    !association.isUsePluginRepositories());
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
