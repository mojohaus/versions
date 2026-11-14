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

import java.io.File;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.filtering.WildcardMatcher;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.repository.ArtifactRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.version.Version;

import static java.util.Collections.singletonList;
import static java.util.Optional.empty;

/**
 * Applies the {@code minimumReleaseAge} filter: a version newer than the one in use is offered as an update only once
 * it has been published for at least the configured number of days.
 *
 * <p>The publication date of a version is the modification time of its POM as the Maven Resolver delivers it. The
 * Resolver's HTTP transports stamp a downloaded file with the repository's {@code Last-Modified} header, so the lookup
 * goes through the session's transport and picks up its proxy, mirror and {@code serverId} authentication settings.
 * A POM that did not come from an HTTP repository, for example one copied into the local repository by
 * {@code mvn install}, carries the time it was written and is therefore treated as published then.</p>
 *
 * <p>Dates are looked up only for versions newer than the current version of the artifact, and are cached for the
 * lifetime of this instance. A version whose date cannot be determined is offered, and a warning says so, so that the
 * filter is never switched off unnoticed. Snapshots pass without a lookup: they are not releases, and whether they are
 * offered is up to {@code allowSnapshots}.</p>
 *
 * @since 2.25.0
 */
public class ArtifactAgeService {

    private final RepositorySystem repositorySystem;

    private final RepositorySystemSession session;

    private final Log log;

    private final int minimumReleaseAge;

    private final List<WildcardMatcher[]> excludes;

    /**
     * Cache of repository id, URL and coordinates to the publication instant, or empty if it is unknown.
     */
    private final Map<String, Optional<Instant>> cache = new ConcurrentHashMap<>();

    /**
     * Creates a new instance.
     *
     * @param repositorySystem  the repository system used to fetch the POMs
     * @param session           the repository session, carrying the proxy, mirror and authentication settings
     * @param log               Maven build log
     * @param minimumReleaseAge the minimum age in days; {@code 0} or less disables the filter
     * @param excludes          {@code groupId:artifactId} patterns of artifacts exempt from the filter; may be
     *                          {@code null}. {@code *} matches any run of characters; a missing {@code artifactId}
     *                          matches every artifact of the group
     */
    public ArtifactAgeService(
            RepositorySystem repositorySystem,
            RepositorySystemSession session,
            Log log,
            int minimumReleaseAge,
            Collection<String> excludes) {
        this.repositorySystem = Objects.requireNonNull(repositorySystem);
        this.session = session;
        this.log = Objects.requireNonNull(log);
        this.minimumReleaseAge = minimumReleaseAge;
        this.excludes = parseExcludes(excludes);
    }

    private static List<WildcardMatcher[]> parseExcludes(Collection<String> patterns) {
        if (patterns == null) {
            return Collections.emptyList();
        }
        List<WildcardMatcher[]> result = new ArrayList<>();
        for (String pattern : patterns) {
            if (pattern == null || pattern.trim().isEmpty()) {
                continue;
            }
            String[] split = pattern.trim().split(":");
            result.add(new WildcardMatcher[] {
                WildcardMatcher.parse(split[0]),
                WildcardMatcher.parse(split.length >= 2 ? split[1] : WildcardMatcher.WILDCARD)
            });
        }
        return result;
    }

    /**
     * Returns whether the filter is switched on.
     *
     * @return {@code true} if {@code minimumReleaseAge} is positive
     */
    public boolean isEnabled() {
        return minimumReleaseAge > 0;
    }

    /**
     * Returns whether the artifact is exempt from the filter.
     *
     * @param artifact the artifact
     * @return {@code true} if its {@code groupId:artifactId} matches one of the exemption patterns
     */
    public boolean isExcluded(Artifact artifact) {
        return excludes.stream()
                .anyMatch(matchers ->
                        matchers[0].test(artifact.getGroupId()) && matchers[1].test(artifact.getArtifactId()));
    }

    /**
     * Drops the versions that are newer than the current version of the artifact and younger than the minimum age.
     * The current version, older ones and snapshots pass without a lookup.
     *
     * @param artifact           the artifact whose versions are being filtered; its version is the current one
     * @param versions           the candidate versions
     * @param versionRangeResult the range result, which names the repository each version came from
     * @return the versions that remain
     */
    public Stream<Version> filterVersionsByAge(
            Artifact artifact, Stream<Version> versions, VersionRangeResult versionRangeResult) {
        if (!isEnabled() || isExcluded(artifact)) {
            return versions;
        }
        ComparableVersion current = currentVersion(artifact);
        return versions.filter(version -> ArtifactUtils.isSnapshot(version.toString())
                || current != null && new ComparableVersion(version.toString()).compareTo(current) <= 0
                || isOldEnough(artifact, version.toString(), versionRangeResult.getRepository(version)));
    }

    private static ComparableVersion currentVersion(Artifact artifact) {
        String version = artifact.getVersion();
        if (version == null || version.isEmpty() || version.startsWith("[") || version.startsWith("(")) {
            return null;
        }
        return new ComparableVersion(version);
    }

    private boolean isOldEnough(Artifact artifact, String version, ArtifactRepository repository) {
        Optional<Instant> publicationDate = getPublicationDate(
                artifact.getGroupId(),
                artifact.getArtifactId(),
                version,
                repository instanceof RemoteRepository ? (RemoteRepository) repository : null);
        if (!publicationDate.isPresent()) {
            return true;
        }
        Instant threshold = Instant.now().minus(minimumReleaseAge, ChronoUnit.DAYS);
        boolean oldEnough = !publicationDate.get().isAfter(threshold);
        if (!oldEnough && log.isDebugEnabled()) {
            log.debug(String.format(
                    "Skipping %s:%s:%s: published %s, %d day(s) ago, less than the minimumReleaseAge of %d",
                    artifact.getGroupId(),
                    artifact.getArtifactId(),
                    version,
                    publicationDate.get(),
                    Duration.between(publicationDate.get(), Instant.now()).toDays(),
                    minimumReleaseAge));
        }
        return oldEnough;
    }

    /**
     * Returns the publication instant of a version, or empty, with a warning logged once, if it cannot be determined.
     *
     * @param groupId    artifact groupId
     * @param artifactId artifact artifactId
     * @param version    the exact version
     * @param repository the remote repository the version was found in; {@code null} if it is not a remote one
     * @return the publication instant, or empty
     */
    Optional<Instant> getPublicationDate(
            String groupId, String artifactId, String version, RemoteRepository repository) {
        String key = (repository == null ? "" : repository.getId() + ":" + repository.getUrl() + ":") + groupId + ":"
                + artifactId + ":" + version;
        return cache.computeIfAbsent(key, k -> resolvePublicationDate(groupId, artifactId, version, repository));
    }

    private Optional<Instant> resolvePublicationDate(
            String groupId, String artifactId, String version, RemoteRepository repository) {
        String gav = groupId + ":" + artifactId + ":" + version;
        if (repository == null) {
            return unknown(gav, "it was not found in a remote repository");
        }
        try {
            ArtifactResult result = repositorySystem.resolveArtifact(
                    session,
                    new ArtifactRequest(
                            new DefaultArtifact(groupId, artifactId, "", "pom", version),
                            singletonList(repository),
                            "minimumReleaseAge"));
            File pom =
                    result.getArtifact() == null ? null : result.getArtifact().getFile();
            long lastModified = pom == null ? 0 : pom.lastModified();
            if (lastModified <= 0) {
                return unknown(gav, "its POM has no modification time");
            }
            return Optional.of(Instant.ofEpochMilli(lastModified));
        } catch (ArtifactResolutionException e) {
            return unknown(gav, "its POM could not be resolved from " + repository.getId() + ": " + e.getMessage());
        }
    }

    private Optional<Instant> unknown(String gav, String reason) {
        log.warn("Cannot determine the publication date of " + gav + " (" + reason
                + "); it is offered although minimumReleaseAge=" + minimumReleaseAge + " is set");
        return empty();
    }
}
