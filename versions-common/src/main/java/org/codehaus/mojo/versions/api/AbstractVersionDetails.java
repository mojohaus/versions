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
import java.util.Collections;
import java.util.Iterator;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.artifact.versioning.VersionRange;
import org.codehaus.mojo.versions.ordering.BoundArtifactVersion;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.ordering.VersionComparator;
import org.codehaus.mojo.versions.utils.DefaultArtifactVersionCache;

import static java.util.Collections.reverseOrder;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.SUBINCREMENTAL;

/**
 * Base class for {@link org.codehaus.mojo.versions.api.VersionDetails}.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
public abstract class AbstractVersionDetails implements VersionDetails {

    private static final Pattern PREVIEW_PATTERN =
            Pattern.compile("(?i)(?:.*[-.](alpha|a|beta|b|milestone|m|preview|rc)"
                    + "[-.]?(\\d{0,2}[a-z]?|\\d{6}\\.\\d{4})|\\d{8}(?:\\.?\\d{6})?)$");

    /**
     * Current version of the dependency artifact.
     *
     * @since 1.0-beta-1
     */
    private ArtifactVersion currentVersion = null;

    protected boolean verboseDetail = true;

    protected AbstractVersionDetails() {}

    @Override
    public Restriction restrictionFor(Optional<Segment> scope) throws InvalidSegmentException {
        // one range spec can have multiple restrictions, and multiple 'lower bound', we want the highest one.
        // [1.0,2.0),[3.0,4.0) -> 3.0
        ArtifactVersion highestLowerBound = currentVersion;
        if (currentVersion != null) {
            try {
                highestLowerBound =
                        VersionRange.createFromVersionSpec(currentVersion.toString()).getRestrictions().stream()
                                .map(Restriction::getLowerBound)
                                .filter(Objects::nonNull)
                                .max(getVersionComparator())
                                .orElse(currentVersion);
            } catch (InvalidVersionSpecificationException ignored) {
                ignored.printStackTrace(System.err);
            }
        }

        final ArtifactVersion currentVersion = highestLowerBound;
        ArtifactVersion nextVersion = scope.filter(s -> s.isMajorTo(SUBINCREMENTAL))
                .map(s -> (ArtifactVersion) new BoundArtifactVersion(currentVersion, Segment.of(s.value() + 1)))
                .orElse(currentVersion);
        return new Restriction(
                nextVersion,
                false,
                scope.filter(MAJOR::isMajorTo)
                        .map(s -> (ArtifactVersion) new BoundArtifactVersion(currentVersion, s))
                        .orElse(null),
                false);
    }

    @Override
    public Restriction restrictionForIgnoreScope(Optional<Segment> ignored) {
        ArtifactVersion nextVersion = ignored.map(s -> (ArtifactVersion) new BoundArtifactVersion(currentVersion, s))
                .orElse(currentVersion);
        return new Restriction(nextVersion, false, null, false);
    }

    @Override
    public final boolean isCurrentVersionDefined() {
        return getCurrentVersion() != null;
    }

    @Override
    public final ArtifactVersion getCurrentVersion() {
        return currentVersion;
    }

    @Override
    public final void setCurrentVersion(ArtifactVersion currentVersion) {
        this.currentVersion = currentVersion;
    }

    @Override
    public final void setCurrentVersion(String currentVersion) {
        setCurrentVersion(currentVersion == null ? null : DefaultArtifactVersionCache.of(currentVersion));
    }

    @Override
    public final ArtifactVersion[] getVersions(VersionRange versionRange, boolean includeSnapshots) {
        return getVersions(versionRange, null, includeSnapshots);
    }

    @Override
    public final ArtifactVersion[] getVersions(
            ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots) {
        Restriction restriction = new Restriction(lowerBound, false, upperBound, false);
        return getVersions(restriction, includeSnapshots);
    }

    @Override
    public final ArtifactVersion getNewestVersion(
            ArtifactVersion lowerBound, ArtifactVersion upperBound, boolean includeSnapshots) {
        Restriction restriction = new Restriction(lowerBound, false, upperBound, false);
        return getNewestVersion(restriction, includeSnapshots);
    }

    @Override
    public final ArtifactVersion getNewestVersion(
            VersionRange versionRange, Restriction restriction, boolean includeSnapshots) {
        return getNewestVersion(versionRange, restriction, includeSnapshots, false);
    }

    @Override
    public final ArtifactVersion getNewestVersion(
            VersionRange versionRange, Restriction restriction, boolean includeSnapshots, boolean allowDowngrade) {
        // reverseOrder( getVersions( ... ) ) will contain versions sorted from latest to oldest,
        // so we only need to find the first candidate fulfilling the criteria
        return Arrays.stream(getVersions(includeSnapshots))
                .sorted(reverseOrder())
                .filter(candidate -> allowDowngrade
                        || versionRange == null
                        || ArtifactVersions.isVersionInRange(candidate, versionRange))
                .filter(candidate -> restriction == null || isVersionInRestriction(restriction, candidate))
                .filter(candidate -> includeSnapshots || !ArtifactUtils.isSnapshot(candidate.toString()))
                .findAny()
                .orElse(null);
    }

    @Override
    public final ArtifactVersion getNewestVersion(Restriction restriction, boolean includeSnapshots) {
        return getNewestVersion(null, restriction, includeSnapshots);
    }

    @Override
    public final ArtifactVersion getNewestVersion(VersionRange versionRange, boolean includeSnapshots) {
        return getNewestVersion(versionRange, null, includeSnapshots);
    }

    @Override
    public final boolean containsVersion(String version) {
        for (ArtifactVersion candidate : getVersions(true)) {
            if (version.equals(candidate.toString())) {
                return true;
            }
        }
        return false;
    }

    private ArtifactVersion[] getNewerVersions(ArtifactVersion version, boolean includeSnapshots) {
        Restriction restriction = new Restriction(version, false, null, false);
        return getVersions(restriction, includeSnapshots);
    }

    @Override
    public final ArtifactVersion[] getNewerVersions(String version, boolean includeSnapshots) {
        return getNewerVersions(DefaultArtifactVersionCache.of(version), includeSnapshots);
    }

    @Deprecated
    @Override
    public final ArtifactVersion[] getNewerVersions(
            String version, Optional<Segment> upperBoundSegment, boolean includeSnapshots)
            throws InvalidSegmentException {
        return getNewerVersions(version, upperBoundSegment, includeSnapshots, false);
    }

    @Override
    public final ArtifactVersion[] getNewerVersions(
            String versionString, Optional<Segment> unchangedSegment, boolean includeSnapshots, boolean allowDowngrade)
            throws InvalidSegmentException {
        ArtifactVersion currentVersion = DefaultArtifactVersionCache.of(versionString);
        ArtifactVersion lowerBound = allowDowngrade
                ? getLowerBound(currentVersion, unchangedSegment)
                        .map(DefaultArtifactVersionCache::of)
                        .orElse(null)
                : currentVersion;
        ArtifactVersion upperBound = unchangedSegment
                .map(s -> (ArtifactVersion) new BoundArtifactVersion(
                        currentVersion, s.isMajorTo(SUBINCREMENTAL) ? Segment.of(s.value() + 1) : s))
                .orElse(null);

        Restriction restriction = new Restriction(lowerBound, allowDowngrade, upperBound, allowDowngrade);
        return getVersions(restriction, includeSnapshots);
    }

    @Override
    public Optional<ArtifactVersion> getNewestVersion(
            String versionString, Optional<Segment> upperBoundSegment, boolean includeSnapshots, boolean allowDowngrade)
            throws InvalidSegmentException {
        ArtifactVersion currentVersion = DefaultArtifactVersionCache.of(versionString);
        ArtifactVersion lowerBound = allowDowngrade
                ? getLowerBound(currentVersion, upperBoundSegment)
                        .map(DefaultArtifactVersionCache::of)
                        .orElse(null)
                : currentVersion;
        ArtifactVersion upperBound = upperBoundSegment
                .map(s -> (ArtifactVersion) new BoundArtifactVersion(
                        currentVersion, s.isMajorTo(SUBINCREMENTAL) ? Segment.of(s.value() + 1) : s))
                .orElse(null);

        Restriction restriction = new Restriction(lowerBound, allowDowngrade, upperBound, allowDowngrade);
        return Arrays.stream(getVersions(includeSnapshots))
                .filter(candidate -> isVersionInRestriction(restriction, candidate))
                .filter(candidate -> includeSnapshots || !ArtifactUtils.isSnapshot(candidate.toString()))
                .max(getVersionComparator());
    }

    @Override
    public final ArtifactVersion[] getVersions(Restriction restriction, boolean includeSnapshots) {
        return getVersions(null, restriction, includeSnapshots);
    }

    @Override
    public final ArtifactVersion[] getVersions(
            VersionRange versionRange, Restriction restriction, boolean includeSnapshots) {
        return Arrays.stream(getVersions(includeSnapshots))
                .filter(candidate -> versionRange == null || ArtifactVersions.isVersionInRange(candidate, versionRange))
                .filter(candidate -> restriction == null || isVersionInRestriction(restriction, candidate))
                .filter(candidate -> includeSnapshots || !ArtifactUtils.isSnapshot(candidate.toString()))
                .sorted(getVersionComparator())
                .distinct()
                .toArray(ArtifactVersion[]::new);
    }

    @Override
    public final ArtifactVersion getNewestUpdate(
            ArtifactVersion currentVersion, Optional<Segment> updateScope, boolean includeSnapshots) {
        try {
            return getNewestVersion(restrictionFor(updateScope), includeSnapshots);
        } catch (InvalidSegmentException e) {
            return null;
        }
    }

    @Override
    public final ArtifactVersion[] getAllUpdates(
            ArtifactVersion currentVersion, Optional<Segment> updateScope, boolean includeSnapshots) {
        try {
            return getVersions(restrictionFor(updateScope), includeSnapshots);
        } catch (InvalidSegmentException e) {
            return null;
        }
    }

    @Override
    public final ArtifactVersion getNewestUpdate(Optional<Segment> updateScope, boolean includeSnapshots) {
        if (isCurrentVersionDefined()) {
            return getNewestUpdate(getCurrentVersion(), updateScope, includeSnapshots);
        }
        return null;
    }

    @Override
    public final ArtifactVersion[] getAllUpdates(Optional<Segment> updateScope, boolean includeSnapshots) {
        if (isCurrentVersionDefined()) {
            return getAllUpdates(getCurrentVersion(), updateScope, includeSnapshots);
        }
        return null;
    }

    @Override
    public final ArtifactVersion[] getAllUpdates(boolean includeSnapshots) {
        return getAllUpdates((VersionRange) null, includeSnapshots);
    }

    @Override
    public ArtifactVersion[] getAllUpdates(VersionRange versionRange, boolean includeSnapshots) {
        Restriction restriction = new Restriction(getCurrentVersion(), false, null, false);
        return getVersions(versionRange, restriction, includeSnapshots);
    }

    /**
     * Returns the string designation of the lower bound version based on the given artifact version
     * and the lowest unchanged segment index (0-based); -1 means that the whole version string can be changed,
     * implying that there is also no string designation of the lower bound version.
     *
     * @param version {@link ArtifactVersion} object specifying the version for which the lower bound is being computed
     * @param unchangedSegment first segment not to be changed; empty() means anything can change
     * @return {@link Optional} string containing the lowest artifact version with the given segment held
     * @throws InvalidSegmentException if the requested segment is outside of the bounds (less than 1 or greater than
     * the segment count)
     */
    protected Optional<String> getLowerBound(ArtifactVersion version, Optional<Segment> unchangedSegment)
            throws InvalidSegmentException {
        if (!unchangedSegment.isPresent()) {
            return empty();
        }

        int segmentCount = getVersionComparator().getSegmentCount(version);
        if (unchangedSegment.get().value() > segmentCount) {
            throw new InvalidSegmentException(unchangedSegment.get(), segmentCount, version);
        }

        StringBuilder newVersion = new StringBuilder();
        newVersion.append(version.getMajorVersion());

        if (segmentCount > 0) {
            newVersion.append(".").append(unchangedSegment.get().value() >= 1 ? version.getMinorVersion() : 0);
        }
        if (segmentCount > 1) {
            newVersion.append(".").append(unchangedSegment.get().value() >= 2 ? version.getIncrementalVersion() : 0);
        }
        if (segmentCount > 2) {
            if (version.getQualifier() != null) {
                newVersion.append("-").append(unchangedSegment.get().value() >= 3 ? version.getQualifier() : "0");
            } else {
                newVersion.append("-").append(unchangedSegment.get().value() >= 3 ? version.getBuildNumber() : "0");
            }
        }
        return of(newVersion.toString());
    }

    /**
     * Checks if the candidate version is in the range of the restriction.
     * a custom comparator is/can be used to have milestones and rcs before final releases,
     * which is not yet possible with {@link Restriction#containsVersion(ArtifactVersion)}.
     * @param restriction the range to check against.
     * @param candidate the version to check.
     * @return true if the candidate version is within the range of the restriction parameter.
     */
    public boolean isVersionInRestriction(Restriction restriction, ArtifactVersion candidate) {
        ArtifactVersion lowerBound = restriction.getLowerBound();
        ArtifactVersion upperBound = restriction.getUpperBound();
        boolean includeLower = restriction.isLowerBoundInclusive();
        boolean includeUpper = restriction.isUpperBoundInclusive();
        final VersionComparator versionComparator = getVersionComparator();
        int lower = lowerBound == null ? -1 : versionComparator.compare(lowerBound, candidate);
        int upper = upperBound == null ? +1 : versionComparator.compare(upperBound, candidate);
        if (lower > 0 || upper < 0) {
            return false;
        }
        return (includeLower || lower != 0) && (includeUpper || upper != 0);
    }

    /**
     * Returns the latest version newer than the specified current version, and within the specified update scope,
     * or {@code null} if no such version exists.
     * @param updateScope the scope of updates to include.
     * @param includeSnapshots whether snapshots should be included
     * @return the newest version after currentVersion within the specified update scope,
     *         or <code>null</code> if no version is available.
     */
    public final ArtifactVersion getReportNewestUpdate(Optional<Segment> updateScope, boolean includeSnapshots) {
        return getArtifactVersionStream(updateScope, includeSnapshots)
                .min(Collections.reverseOrder(getVersionComparator()))
                .orElse(null);
    }

    /**
     * Returns all versions newer than the specified current version, and within the specified update scope.
     * @param updateScope the scope of updates to include.
     * @param includeSnapshots whether snapshots should be included
     * @return all versions after currentVersion within the specified update scope.
     */
    public final ArtifactVersion[] getReportUpdates(Optional<Segment> updateScope, boolean includeSnapshots) {
        TreeSet<ArtifactVersion> versions = getArtifactVersionStream(updateScope, includeSnapshots)
                .collect(Collectors.toCollection(() -> new TreeSet<>(getVersionComparator())));
        // filter out intermediate minor versions.
        if (!verboseDetail) {
            int major = 0;
            int minor = 0;
            boolean needOneMore = false;
            for (Iterator<ArtifactVersion> it = versions.descendingIterator(); it.hasNext(); ) {
                ArtifactVersion version = it.next();
                boolean isPreview = PREVIEW_PATTERN.matcher(version.toString()).matches();

                // encountered a version in same Major.Minor version, remove it.
                if (version.getMajorVersion() == major && version.getMinorVersion() == minor) {
                    if (needOneMore && !isPreview) {
                        needOneMore = false;
                        continue;
                    }
                    it.remove();
                    continue;
                }

                // encountered a new Major.Minor version, keep it.
                major = version.getMajorVersion();
                minor = version.getMinorVersion();

                // if version is a pre-release, also search for the last release.
                needOneMore = isPreview;
            }
        }
        return versions.toArray(new ArtifactVersion[0]);
    }

    /**
     * Returns all versions newer than the specified current version, and within the specified update scope.
     * @param updateScope the scope of updates to include.
     * @param includeSnapshots whether snapshots should be included
     * @return all versions after currentVersion within the specified update scope.
     */
    private Stream<ArtifactVersion> getArtifactVersionStream(Optional<Segment> updateScope, boolean includeSnapshots) {
        if (isCurrentVersionDefined()) {
            try {
                Restriction restriction = restrictionFor(updateScope);

                return Arrays.stream(getVersions(includeSnapshots))
                        .filter(candidate -> isVersionInRestriction(restriction, candidate));
            } catch (InvalidSegmentException ignored) {
                ignored.printStackTrace(System.err);
            }
        }
        return Stream.empty();
    }
}
