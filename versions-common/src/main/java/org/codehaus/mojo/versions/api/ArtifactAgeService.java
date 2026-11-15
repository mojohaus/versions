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
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
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
import java.util.Set;
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
import org.eclipse.aether.spi.connector.transport.GetTask;
import org.eclipse.aether.spi.connector.transport.Transporter;
import org.eclipse.aether.spi.connector.transport.TransporterProvider;
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
 * <p>The time is read from the POM in the local repository first. The Resolver does not keep the
 * {@code Last-Modified} time on that file everywhere: on Windows with Maven 3.9.x, and with the Wagon transport, the
 * file has the time of its download. Such a time is later than the publication date, never earlier, so a version
 * that it shows to be old enough is accepted. For a version that it shows to be too young, the POM is fetched once
 * more, through the transport and into a temporary file, which the HTTP transports stamp on every system. If the
 * transport sets no time there either, the time in the local repository stands and a warning says so.</p>
 *
 * <p>Dates are looked up only for versions newer than the current version of the artifact, and are cached for the
 * lifetime of this instance. A version whose date cannot be determined is offered, and a warning says so, so that the
 * filter is never switched off unnoticed. Snapshots pass without a lookup: they are not releases, and whether they are
 * offered is up to {@code allowSnapshots}.</p>
 *
 * @since 2.25.0
 */
public class ArtifactAgeService {

    /**
     * File systems store the modification time with a precision of up to two seconds.
     */
    private static final long FILE_TIME_PRECISION_MILLIS = 2000;

    private final RepositorySystem repositorySystem;

    private final RepositorySystemSession session;

    private final Log log;

    private final int minimumReleaseAge;

    private final List<WildcardMatcher[]> excludes;

    private final TransporterProvider transporterProvider;

    /**
     * Cache of repository id, URL and coordinates to the publication instant, or empty if it is unknown.
     */
    private final Map<String, Optional<Instant>> cache = new ConcurrentHashMap<>();

    /**
     * Ids of the repositories already reported as giving no modification time through the transport.
     */
    private final Set<String> repositoriesWithoutModificationTime = ConcurrentHashMap.newKeySet();

    /**
     * Creates a new instance that reads the publication dates from the local repository only.
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
        this(repositorySystem, session, log, minimumReleaseAge, excludes, null);
    }

    /**
     * Creates a new instance.
     *
     * @param repositorySystem    the repository system used to fetch the POMs
     * @param session             the repository session, with the proxy, mirror and authentication settings
     * @param log                 Maven build log
     * @param minimumReleaseAge   the minimum age in days; {@code 0} or less disables the filter
     * @param excludes            {@code groupId:artifactId} patterns of artifacts exempt from the filter; may be
     *                            {@code null}. {@code *} matches any run of characters; a missing {@code artifactId}
     *                            matches every artifact of the group
     * @param transporterProvider provider of the transporters used to read the date of a version that the local
     *                            repository shows to be too young; may be {@code null}, in which case the time in the
     *                            local repository is final
     * @since 2.25.3
     */
    public ArtifactAgeService(
            RepositorySystem repositorySystem,
            RepositorySystemSession session,
            Log log,
            int minimumReleaseAge,
            Collection<String> excludes,
            TransporterProvider transporterProvider) {
        this.repositorySystem = Objects.requireNonNull(repositorySystem);
        this.session = session;
        this.log = Objects.requireNonNull(log);
        this.minimumReleaseAge = minimumReleaseAge;
        this.excludes = parseExcludes(excludes);
        this.transporterProvider = transporterProvider;
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
        boolean oldEnough = isOldEnough(publicationDate.get());
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

    private boolean isOldEnough(Instant publicationDate) {
        return !publicationDate.isAfter(Instant.now().minus(minimumReleaseAge, ChronoUnit.DAYS));
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
        Optional<Instant> localDate = localRepositoryDate(gav, groupId, artifactId, version, repository);
        if (!localDate.isPresent() || isOldEnough(localDate.get())) {
            return localDate;
        }
        // A time that is too young may be the time of the download instead of the publication date
        Optional<Instant> transportDate = transportDate(gav, groupId, artifactId, version, repository);
        return transportDate.isPresent() ? transportDate : localDate;
    }

    /**
     * Returns the modification time of the POM that the Resolver resolves into the local repository.
     */
    private Optional<Instant> localRepositoryDate(
            String gav, String groupId, String artifactId, String version, RemoteRepository repository) {
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

    /**
     * Fetches the POM through the transport into a temporary file and returns the modification time the transport
     * set on that file, or empty if it set none or the POM could not be fetched. The content is not used, so it is
     * not validated.
     */
    private Optional<Instant> transportDate(
            String gav, String groupId, String artifactId, String version, RemoteRepository repository) {
        if (transporterProvider == null
                || session.isOffline()
                || repository.isBlocked()
                || !"default".equals(repository.getContentType())) {
            return empty();
        }
        Transporter transporter = null;
        Path pom = null;
        try {
            transporter = transporterProvider.newTransporter(session, repository);
            pom = Files.createTempFile("versions-minimumReleaseAge-", ".pom");
            long start = System.currentTimeMillis();
            transporter.get(new GetTask(pomLocation(groupId, artifactId, version)).setDataFile(pom.toFile()));
            long lastModified = pom.toFile().lastModified();
            // A transport that sets no time leaves the file with the time it was written, which is not before the start
            if (lastModified > 0 && lastModified < start - FILE_TIME_PRECISION_MILLIS) {
                return Optional.of(Instant.ofEpochMilli(lastModified));
            }
            if (repositoriesWithoutModificationTime.add(repository.getId())) {
                log.warn("Cannot read the publication date of " + gav + " from " + repository.getId()
                        + ": the transport set no modification time. The time its POM was downloaded is used for"
                        + " it and for other versions from this repository, so they can be held back for longer"
                        + " than minimumReleaseAge=" + minimumReleaseAge);
            }
        } catch (Exception e) {
            if (e instanceof InterruptedException) {
                Thread.currentThread().interrupt();
            }
            log.debug("Could not fetch the POM of " + gav + " from " + repository.getId(), e);
        } finally {
            if (transporter != null) {
                transporter.close();
            }
            if (pom != null) {
                try {
                    Files.deleteIfExists(pom);
                } catch (IOException e) {
                    log.debug("Could not delete " + pom, e);
                }
            }
        }
        return empty();
    }

    /**
     * Returns the location of a POM relative to the root of a repository with the {@code default} layout.
     */
    private static URI pomLocation(String groupId, String artifactId, String version) throws URISyntaxException {
        return new URI(
                null,
                null,
                groupId.replace('.', '/') + '/' + artifactId + '/' + version + '/' + artifactId + '-' + version
                        + ".pom",
                null);
    }

    private Optional<Instant> unknown(String gav, String reason) {
        log.warn("Cannot determine the publication date of " + gav + " (" + reason
                + "); it is offered although minimumReleaseAge=" + minimumReleaseAge + " is set");
        return empty();
    }
}
