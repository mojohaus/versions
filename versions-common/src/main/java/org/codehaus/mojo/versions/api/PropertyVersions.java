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
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.OverConstrainedVersionException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.ordering.BoundArtifactVersion;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;

import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Manages a property that is associated with one or more artifacts.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
public class PropertyVersions extends AbstractVersionDetails {
    private final String name;

    private final String profileId;

    private final Set<ArtifactAssociation> associations;

    /**
     * The available versions.
     *
     * @since 1.0-beta-1
     */
    private final SortedSet<ArtifactVersion> resolvedVersions;

    private final Log log;

    PropertyVersions(
            String profileId,
            String name,
            Log log,
            Set<ArtifactAssociation> associations,
            SortedSet<ArtifactVersion> resolvedVersions) {
        this.profileId = profileId;
        this.name = name;
        this.log = log;
        this.associations = new TreeSet<>(associations);
        this.resolvedVersions = resolvedVersions;
    }

    public ArtifactAssociation[] getAssociations() {
        return associations.toArray(new ArtifactAssociation[0]);
    }

    /**
     * Uses the supplied {@link Collection} of {@link Artifact} instances to see if an ArtifactVersion can be provided.
     *
     * @param artifacts The {@link Collection} of {@link Artifact} instances .
     * @return The versions that can be resolved from the supplied Artifact instances or an empty array if no version
     *         can be resolved (i.e. the property is not associated with any of the supplied artifacts or the property
     *         is also associated to an artifact that has not been provided).
     * @since 1.0-alpha-3
     */
    public ArtifactVersion[] getVersions(Collection<Artifact> artifacts) {
        SortedSet<ArtifactVersion> result = new TreeSet<>();
        // go through all the associations
        // see if they are met from the collection
        // add the version if they are
        // go through all the versions
        // see if the version is available for all associations
        for (ArtifactAssociation association : associations) {
            for (Artifact artifact : artifacts) {
                if (association.getArtifact().getGroupId().equals(artifact.getGroupId())
                        && association.getArtifact().getArtifactId().equals(artifact.getArtifactId())) {
                    try {
                        result.add(artifact.getSelectedVersion());
                    } catch (OverConstrainedVersionException e) {
                        // ignore this one as we cannot resolve a valid version
                    }
                }
            }
        }

        // Filter versions that satisfy all associations
        result.removeIf(candidate -> associations.stream()
                .anyMatch(association -> artifacts.stream().noneMatch(artifact -> {
                    try {
                        return association.getArtifact().getGroupId().equals(artifact.getGroupId())
                                && association.getArtifact().getArtifactId().equals(artifact.getArtifactId())
                                && candidate
                                        .toString()
                                        .equals(artifact.getSelectedVersion().toString());
                    } catch (OverConstrainedVersionException e) {
                        // ignore this one as we cannot resolve a valid version
                        return false;
                    }
                })));

        return result.toArray(new ArtifactVersion[0]);
    }

    /**
     * Uses the {@link DefaultVersionsHelper} to find all available versions that match all the associations with this
     * property.
     *
     * @param includeSnapshots Whether to include snapshot versions in our search.
     * @return The (possibly empty) array of versions.
     */
    public ArtifactVersion[] getVersions(boolean includeSnapshots) {
        return resolvedVersions.stream()
                .filter(v -> includeSnapshots || !ArtifactUtils.isSnapshot(v.toString()))
                .toArray(ArtifactVersion[]::new);
    }

    /**
     * Returns the name of the property
     * @return name of the property
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the id of the profile
     * @return id of the profile
     */
    public String getProfileId() {
        return profileId;
    }

    /**
     * Says whether the property is associated with a dependency
     * @return {@code true} if the property is associated with a dependency
     */
    public boolean isAssociated() {
        return !associations.isEmpty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "PropertyVersions{" + (profileId == null ? "" : "profileId='" + profileId + "', ")
                + "name='" + name + '\''
                + ", associations=" + associations + '}';
    }

    /**
     * Retrieves the newest artifact version for the given property-denoted artifact or {@code null} if no newer
     * version could be found.
     *
     * @param versionString     current version of the artifact
     * @param property          property name indicating the artifact
     * @param allowSnapshots    whether snapshots should be considered
     * @param reactorProjects   collection of reactor projects
     * @param allowDowngrade    whether downgrades should be allowed
     * @param upperBoundSegment the upper bound segment; empty() means no upper bound
     * @return newest artifact version fulfilling the criteria or null if no newer version could be found
     * @throws InvalidSegmentException              thrown if the {@code unchangedSegment} is not valid (e.g. greater
     *                                              than the number
     *                                              of segments in the version string)
     * @throws InvalidVersionSpecificationException thrown if the version string in the property is not valid
     */
    public ArtifactVersion getNewestVersion(
            String versionString,
            Property property,
            boolean allowSnapshots,
            Collection<MavenProject> reactorProjects,
            boolean allowDowngrade,
            Optional<Segment> upperBoundSegment)
            throws InvalidSegmentException, InvalidVersionSpecificationException {
        boolean includeSnapshots = !property.isBanSnapshots() && allowSnapshots;
        log.debug("getNewestVersion(): includeSnapshots='" + includeSnapshots + "'");
        log.debug("Property ${" + property.getName() + "}: Set of valid available versions is "
                + Arrays.asList(getVersions(includeSnapshots)));

        VersionRange range =
                property.getVersion() != null ? VersionRange.createFromVersionSpec(property.getVersion()) : null;
        log.debug("Property ${" + property.getName() + "}: Restricting results to " + range);

        ArtifactVersion currentVersion = ArtifactVersionService.getArtifactVersion(versionString);
        ArtifactVersion lowerBound = allowDowngrade
                ? getLowerBound(currentVersion, upperBoundSegment)
                        .map(ArtifactVersionService::getArtifactVersion)
                        .orElse(null)
                : currentVersion;
        log.debug("lowerBoundArtifactVersion: " + lowerBound);

        ArtifactVersion upperBound = upperBoundSegment.isPresent()
                ? upperBoundSegment
                        .map(s -> new BoundArtifactVersion(
                                currentVersion, s.isMajorTo(SUBINCREMENTAL) ? Segment.minorTo(s) : s))
                        .orElse(null)
                : null;
        log.debug("Property ${" + property.getName() + "}: upperBound is: " + upperBound);

        Restriction restriction = new Restriction(lowerBound, allowDowngrade, upperBound, allowDowngrade);
        ArtifactVersion result = getNewestVersion(range, restriction, includeSnapshots);

        log.debug("Property ${" + property.getName() + "}: Current winner is: " + result);

        if (property.isSearchReactor()) {
            log.debug("Property ${" + property.getName() + "}: Searching reactor for a valid version...");
            Set<Artifact> reactorArtifacts =
                    reactorProjects.stream().map(MavenProject::getArtifact).collect(Collectors.toSet());
            ArtifactVersion[] reactorVersions = getVersions(reactorArtifacts);
            log.debug("Property ${" + property.getName() + "}: Set of valid available versions from the reactor is "
                    + Arrays.asList(reactorVersions));

            ArtifactVersion fromReactor = Arrays.stream(reactorVersions)
                    .filter(version -> range == null || ArtifactVersions.isVersionInRange(version, range))
                    .reduce((first, second) -> second)
                    .orElse(null);

            if (fromReactor != null && (result == null || !currentVersion.equals(fromReactor.toString()))) {
                result = property.isPreferReactor() || result == null
                        ? fromReactor
                        : result.compareTo(fromReactor) < 0 ? fromReactor : result;
                log.debug("Property ${" + property.getName() + "}: "
                        + (result.equals(fromReactor)
                                ? "Reactor has a valid version"
                                : "Reactor has the same or older version"));
            }
        }

        return result;
    }
}
