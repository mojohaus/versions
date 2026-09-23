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

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.logging.Log;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.version.Version;

import static java.util.Optional.empty;
import static org.codehaus.mojo.versions.api.internal.AgeFilteringUtils.toRemoteRepository;

/**
 * Service that resolves the publication date for artifact versions by probing
 * the version POM in the repository that supplied the version and reading its
 * {@code Last-Modified} timestamp.
 *
 * <p>Results are cached in memory so that each {@code groupId:artifactId:version}
 * triplet is only resolved once per build.
 *
 * @since 2.18.0
 */
public class ArtifactAgeService {

    private static final DateTimeFormatter HTTP_DATE_FORMATTER = DateTimeFormatter.ofPattern(
                    "EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
            .withZone(ZoneOffset.UTC);

    private final Log log;

    /** Cache: "groupId:artifactId:version" → publication Instant (or null if unavailable). */
    private final Map<String, Optional<Instant>> cache = new ConcurrentHashMap<>();

    /**
     * Creates a new instance.
     *
     * @param log              Maven build log
     */
    public ArtifactAgeService(Log log) {
        this.log = Objects.requireNonNull(log);
    }

    /**
     * Filters the given version stream to keep only versions older than the requested
     * minimum age.
     *
     * <p>For each version, this method resolves the repository that provided it and
     * inspects the corresponding version POM to determine its publication timestamp.
     * Versions whose publication date is not known are retained so that the build
     * continues without silently excluding candidates.</p>
     *
     * @param artifact the artifact whose versions are being filtered
     * @param minDaysOld the minimum age in days; if the value is {@code <= 0}, no
     *                   filtering is applied
     * @param versions the candidate versions to evaluate
     * @param versionRangeResult the range result used to obtain the repository for
     *                          each version
     * @return a filtered stream containing only versions at least {@code minDaysOld}
     *         days old, or the original stream when {@code minDaysOld <= 0}
     */
    public Stream<Version> filterVersionsByAge(
            Artifact artifact, int minDaysOld, Stream<Version> versions, VersionRangeResult versionRangeResult) {
        if (minDaysOld > 0) {
            versions = versions.filter((Version version) -> isOldEnough(
                    artifact.getGroupId(),
                    artifact.getArtifactId(),
                    version.toString(),
                    minDaysOld,
                    toRemoteRepository(versionRangeResult.getRepository(version))));
        }
        return versions;
    }

    /**
     * Returns the publication {@link Instant} for the given artifact version, or
     * {@link Optional#empty()} if it cannot be determined.
     *
     * @param groupId      artifact groupId
     * @param artifactId   artifact artifactId
     * @param version      the exact version string to look up
     * @param repository   the remote repository that supplied the version
     * @return an {@link Optional} containing the publication {@link Instant}, or empty
     */
    Optional<Instant> getPublicationDate(
            String groupId, String artifactId, String version, RemoteRepository repository) {

        String cacheKey = repository == null
                ? groupId + ":" + artifactId + ":" + version + ":null"
                : repository.getId() + ":" + repository.getUrl() + ":" + groupId + ":" + artifactId + ":" + version;
        return cache.computeIfAbsent(cacheKey, k -> resolvePublicationDate(groupId, artifactId, version, repository));
    }

    /**
     * Determines whether a version is old enough, i.e. its publication date is at least {@code minDaysOld} days in the
     * past relative to now.
     *
     * <p>If the publication date cannot be determined (e.g. for local / file repos or
     * repositories that do not expose {@code Last-Modified}), the version is <em>included</em> (returns {@code true})
     * so that the build is not silently broken.
     *
     * @param groupId    artifact groupId
     * @param artifactId artifact artifactId
     * @param version    the exact version string
     * @param minDaysOld minimum age in days
     * @param repository the remote repository that supplied the version
     * @return {@code true} if the version is old enough or the date is unavailable
     */
    boolean isOldEnough(
            String groupId, String artifactId, String version, int minDaysOld, RemoteRepository repository) {

        Optional<Instant> publicationDate = getPublicationDate(groupId, artifactId, version, repository);
        if (!publicationDate.isPresent()) {
            if (log.isDebugEnabled()) {
                log.debug("Could not determine publication date for " + groupId + ":" + artifactId + ":" + version
                        + "; including it despite minDaysOld=" + minDaysOld);
            }
            return true;
        }
        Instant threshold = Instant.now().minusSeconds((long) minDaysOld * 86400);
        boolean old = publicationDate.get().isBefore(threshold);
        if (!old && log.isDebugEnabled()) {
            log.debug("Skipping " + groupId + ":" + artifactId + ":" + version + " – published " + publicationDate.get()
                    + " which is less than " + minDaysOld + " days ago");
        }
        return old;
    }

    // -------------------------------------------------------------------------

    private Optional<Instant> resolvePublicationDate(
            String groupId, String artifactId, String version, RemoteRepository repository) {
        if (repository == null) {
            return empty();
        }
        return probeVersionPomDate(groupId, artifactId, version, repository);
    }

    /**
     * Issues an HTTP HEAD request for the POM of the given version in the given repository,
     * returning the {@code Last-Modified} header as an {@link Instant}.
     * For {@code file://} repositories, falls back to the POM file's lastModified time.
     */
    private Optional<Instant> probeVersionPomDate(
            String groupId, String artifactId, String version, RemoteRepository repo) {

        String repoUrl = repo.getUrl();
        if (repoUrl == null) {
            return empty();
        }

        // Construct POM path: groupIdPath/artifactId/version/artifactId-version.pom
        String groupIdPath = groupId.replace('.', '/');
        String pomPath = groupIdPath + "/" + artifactId + "/" + version + "/" + artifactId + "-" + version + ".pom";

        if (repoUrl.startsWith("file:")) {
            return probeFileRepository(repoUrl, pomPath);
        }

        return probeHttpRepository(repoUrl, pomPath, groupId, artifactId, version);
    }

    private Optional<Instant> probeFileRepository(String repoUrl, String pomPath) {
        try {
            URL pomUrl = new URL(repoUrl.endsWith("/") ? repoUrl + pomPath : repoUrl + "/" + pomPath);
            java.io.File pomFile = new java.io.File(pomUrl.toURI());
            if (pomFile.exists()) {
                return Optional.of(Instant.ofEpochMilli(pomFile.lastModified()));
            }
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                log.debug("Could not probe file repository: " + e.getMessage());
            }
        }
        return empty();
    }

    private Optional<Instant> probeHttpRepository(
            String repoUrl, String pomPath, String groupId, String artifactId, String version) {

        String pomUrlStr = (repoUrl.endsWith("/") ? repoUrl : repoUrl + "/") + pomPath;
        HttpURLConnection connection = null;
        try {
            URL pomUrl = new URL(pomUrlStr);
            connection = (HttpURLConnection) pomUrl.openConnection();
            connection.setRequestMethod("HEAD");
            connection.setConnectTimeout(10_000);
            connection.setReadTimeout(10_000);
            connection.setInstanceFollowRedirects(true);
            // consume minimal response to avoid leaking connections
            int status = connection.getResponseCode();
            if (status == HttpURLConnection.HTTP_OK) {
                String lastModifiedHeader = connection.getHeaderField("Last-Modified");
                if (lastModifiedHeader != null && !lastModifiedHeader.isEmpty()) {
                    try {
                        return Optional.of(Instant.from(HTTP_DATE_FORMATTER.parse(lastModifiedHeader)));
                    } catch (DateTimeParseException e) {
                        if (log.isDebugEnabled()) {
                            log.debug("Unparseable Last-Modified header '" + lastModifiedHeader + "' for " + groupId
                                    + ":" + artifactId + ":" + version + " at " + pomUrlStr);
                        }
                    }
                }

                long lastModified = connection.getLastModified();
                if (lastModified > 0) {
                    return Optional.of(Instant.ofEpochMilli(lastModified));
                }
                // Some repos don't set Last-Modified; fall through
                if (log.isDebugEnabled()) {
                    log.debug("No Last-Modified header for " + groupId + ":" + artifactId + ":" + version + " at "
                            + pomUrlStr);
                }
            } else if (log.isDebugEnabled()) {
                log.debug("HEAD " + pomUrlStr + " returned HTTP " + status);
            }
        } catch (IOException e) {
            if (log.isDebugEnabled()) {
                log.debug("Could not probe " + pomUrlStr + ": " + e.getMessage());
            }
        } finally {
            if (connection != null) {
                // read and discard the error stream to allow connection reuse
                try {
                    InputStream es = connection.getErrorStream();
                    if (es != null) {
                        es.close();
                    }
                } catch (IOException ignored) {
                    // ignored
                }
                connection.disconnect();
            }
        }
        return empty();
    }
}
