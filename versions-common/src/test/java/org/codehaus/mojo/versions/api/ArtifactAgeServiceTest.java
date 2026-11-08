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
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.plugin.logging.Log;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.LocalRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.Version;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ArtifactAgeServiceTest {

    private static final RemoteRepository REPOSITORY =
            new RemoteRepository.Builder("repo", "default", "https://repo.invalid/").build();

    private final Log log = mock(Log.class);

    private final RepositorySystem repositorySystem = mock(RepositorySystem.class);

    private final RepositorySystemSession session = mock(RepositorySystemSession.class);

    @TempDir
    Path tempDir;

    @BeforeEach
    void setUp() throws Exception {
        when(log.isDebugEnabled()).thenReturn(false);
    }

    private static Version version(String version) throws Exception {
        return new GenericVersionScheme().parseVersion(version);
    }

    private static Artifact artifact(String groupId, String artifactId, String version) {
        return new DefaultArtifact(
                groupId, artifactId, version, "compile", "jar", null, new DefaultArtifactHandler("jar"));
    }

    private ArtifactAgeService service(int minimumReleaseAge, String... excludes) {
        return new ArtifactAgeService(repositorySystem, session, log, minimumReleaseAge, Arrays.asList(excludes));
    }

    /** Makes every POM resolve to a file written ageInDays ago, by version. */
    private void pomsPublished(String... versionAndAgeInDays) throws Exception {
        when(repositorySystem.resolveArtifact(any(), any(ArtifactRequest.class)))
                .then(invocation -> {
                    ArtifactRequest request = invocation.getArgument(1);
                    String version = request.getArtifact().getVersion();
                    for (int i = 0; i < versionAndAgeInDays.length; i += 2) {
                        if (versionAndAgeInDays[i].equals(version)) {
                            return resolved(request, Long.parseLong(versionAndAgeInDays[i + 1]));
                        }
                    }
                    throw new AssertionError("Unexpected lookup of " + version);
                });
    }

    private ArtifactResult resolved(ArtifactRequest request, long ageInDays) throws IOException {
        Path pom = tempDir.resolve(request.getArtifact().getArtifactId() + "-"
                + request.getArtifact().getVersion() + ".pom");
        Files.write(pom, "<project/>".getBytes(StandardCharsets.UTF_8));
        Files.setLastModifiedTime(pom, FileTime.from(Instant.now().minus(ageInDays, ChronoUnit.DAYS)));
        return new ArtifactResult(request).setArtifact(request.getArtifact().setFile(pom.toFile()));
    }

    private List<String> filter(ArtifactAgeService service, Artifact artifact, String... versions) throws Exception {
        VersionRangeResult range = new VersionRangeResult(new VersionRangeRequest());
        List<Version> parsed = new java.util.ArrayList<>();
        for (String v : versions) {
            Version version = version(v);
            parsed.add(version);
            range.addVersion(version).setRepository(version, REPOSITORY);
        }
        return service.filterVersionsByAge(artifact, parsed.stream(), range)
                .map(Version::toString)
                .collect(Collectors.toList());
    }

    @Test
    void leavesOutVersionsYoungerThanTheMinimumAge() throws Exception {
        pomsPublished("1.1.0", "30", "1.2.0", "3");

        assertEquals(
                Arrays.asList("1.0.0", "1.1.0"),
                filter(service(7), artifact("g", "a", "1.0.0"), "1.0.0", "1.1.0", "1.2.0"));
    }

    @Test
    void looksUpOnlyVersionsNewerThanTheCurrentOne() throws Exception {
        pomsPublished("2.0.0", "1");

        assertEquals(
                Arrays.asList("0.9", "1.0.0"),
                filter(service(7), artifact("g", "a", "1.0.0"), "0.9", "1.0.0", "2.0.0"));

        ArgumentCaptor<ArtifactRequest> requests = ArgumentCaptor.forClass(ArtifactRequest.class);
        verify(repositorySystem, times(1)).resolveArtifact(any(), requests.capture());
        assertEquals("2.0.0", requests.getValue().getArtifact().getVersion());
        assertEquals("pom", requests.getValue().getArtifact().getExtension());
        assertEquals(Collections.singletonList(REPOSITORY), requests.getValue().getRepositories());
    }

    @Test
    void checksEveryVersionWhenTheCurrentVersionIsARange() throws Exception {
        pomsPublished("1.0.0", "1", "2.0.0", "1");

        assertEquals(Collections.emptyList(), filter(service(7), artifact("g", "a", "[1.0,)"), "1.0.0", "2.0.0"));
    }

    @Test
    void doesNothingWhenSwitchedOff() throws Exception {
        assertEquals(
                Arrays.asList("1.0.0", "2.0.0"), filter(service(0), artifact("g", "a", "1.0.0"), "1.0.0", "2.0.0"));
        verify(repositorySystem, never()).resolveArtifact(any(), any(ArtifactRequest.class));
    }

    @Test
    void skipsExemptArtifactsWithoutALookup() throws Exception {
        ArtifactAgeService service = service(7, "org.acme:*", "com.trusted", "*:special-*");

        for (Artifact exempt : Arrays.asList(
                artifact("org.acme", "lib", "1.0"),
                artifact("com.trusted", "anything", "1.0"),
                artifact("any.group", "special-one", "1.0"))) {
            assertEquals(Arrays.asList("1.0", "2.0"), filter(service, exempt, "1.0", "2.0"));
        }
        verify(repositorySystem, never()).resolveArtifact(any(), any(ArtifactRequest.class));
        assertFalse(service.isExcluded(artifact("org.acmex", "lib", "1.0")));
        assertFalse(service.isExcluded(artifact("com.other", "anything", "1.0")));
    }

    @Test
    void warnsOnceAndOffersTheVersionWhenThePomCannotBeResolved() throws Exception {
        when(repositorySystem.resolveArtifact(any(), any(ArtifactRequest.class)))
                .thenThrow(new ArtifactResolutionException(Collections.emptyList(), "no route to host"));
        ArtifactAgeService service = service(7);

        assertEquals(Arrays.asList("1.0", "2.0"), filter(service, artifact("g", "a", "1.0"), "1.0", "2.0"));
        assertEquals(Arrays.asList("1.0", "2.0"), filter(service, artifact("g", "a", "1.0"), "1.0", "2.0"));

        verify(log, times(1)).warn(contains("g:a:2.0"));
        verify(repositorySystem, times(1)).resolveArtifact(any(), any(ArtifactRequest.class));
    }

    @Test
    void warnsWhenTheVersionDoesNotComeFromARemoteRepository() throws Exception {
        VersionRangeResult range = new VersionRangeResult(new VersionRangeRequest());
        Version newer = version("2.0");
        range.addVersion(newer).setRepository(newer, new LocalRepository(tempDir.toFile()));

        List<Version> kept = service(7)
                .filterVersionsByAge(artifact("g", "a", "1.0"), Stream.of(newer), range)
                .collect(Collectors.toList());

        assertEquals(Collections.singletonList(newer), kept);
        verify(log).warn(contains("g:a:2.0"));
        verify(repositorySystem, never()).resolveArtifact(any(), any(ArtifactRequest.class));
    }

    @Test
    void warnsWhenThePomHasNoModificationTime() throws Exception {
        when(repositorySystem.resolveArtifact(any(), any(ArtifactRequest.class)))
                .then(invocation -> {
                    ArtifactRequest request = invocation.getArgument(1);
                    return new ArtifactResult(request)
                            .setArtifact(request.getArtifact()
                                    .setFile(tempDir.resolve("missing.pom").toFile()));
                });

        assertEquals(Arrays.asList("1.0", "2.0"), filter(service(7), artifact("g", "a", "1.0"), "1.0", "2.0"));
        verify(log).warn(contains("modification time"));
    }

    @Test
    void looksUpEachVersionOnce() throws Exception {
        pomsPublished("2.0", "30");
        ArtifactAgeService service = service(7);

        filter(service, artifact("g", "a", "1.0"), "2.0");
        filter(service, artifact("g", "a", "1.5"), "2.0");

        verify(repositorySystem, times(1)).resolveArtifact(any(), any(ArtifactRequest.class));
        verify(log, never()).warn(anyString());
    }

    @Test
    void passesTheSessionOnToTheResolver() throws Exception {
        pomsPublished("2.0", "30");

        filter(service(7), artifact("g", "a", "1.0"), "2.0");

        verify(repositorySystem)
                .resolveArtifact(org.mockito.ArgumentMatchers.same(session), any(ArtifactRequest.class));
        assertTrue(service(1).isEnabled());
        assertFalse(service(0).isEnabled());
    }
}
