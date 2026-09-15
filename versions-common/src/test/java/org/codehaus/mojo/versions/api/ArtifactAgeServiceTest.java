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
import java.net.InetSocketAddress;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import com.sun.net.httpserver.HttpServer;
import org.apache.maven.plugin.logging.Log;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ArtifactAgeServiceTest {

    @Mock
    private Log log;

    @Mock
    private RepositorySystemSession session;

    @TempDir
    File tempDir;

    private ArtifactAgeService service;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        service = new ArtifactAgeService(log);
    }

    @Test
    void returnsEmptyWhenRepositoryMissing() {
        Optional<Instant> result = service.getPublicationDate("com.example", "artifact", "1.0.0", null);
        assertFalse(result.isPresent());
    }

    @Test
    void returnsEmptyWhenRepositoryUrlMissing() {
        RemoteRepository repo = new RemoteRepository.Builder("test-repo", "default", "").build();
        Optional<Instant> result = service.getPublicationDate("com.example", "artifact", "1.0.0", repo);
        assertFalse(result.isPresent());
    }

    @Test
    void isOldEnoughReturnsTrueWhenDateUnavailable() {
        boolean result = service.isOldEnough("com.example", "artifact", "1.0.0", 7, null);
        assertTrue(result);
    }

    @Test
    void isOldEnoughReturnsTrueForOldVersionInFileRepo() throws Exception {
        File pomDir = new File(tempDir, "com/example/artifact/1.0.0");
        pomDir.mkdirs();
        File pomFile = new File(pomDir, "artifact-1.0.0.pom");
        java.nio.file.Files.write(pomFile.toPath(), "<project/>".getBytes());
        pomFile.setLastModified(Instant.now().minus(30, ChronoUnit.DAYS).toEpochMilli());

        RemoteRepository fileRepo = new RemoteRepository.Builder(
                        "file-repo", "default", tempDir.toURI().toString())
                .build();

        boolean result = service.isOldEnough("com.example", "artifact", "1.0.0", 7, fileRepo);
        assertTrue(result);
    }

    @Test
    void isOldEnoughReturnsFalseForRecentVersionInFileRepo() throws Exception {
        File pomDir = new File(tempDir, "com/example/artifact/2.0.0");
        pomDir.mkdirs();
        File pomFile = new File(pomDir, "artifact-2.0.0.pom");
        java.nio.file.Files.write(pomFile.toPath(), "<project/>".getBytes());
        pomFile.setLastModified(Instant.now().minus(1, ChronoUnit.DAYS).toEpochMilli());

        RemoteRepository fileRepo = new RemoteRepository.Builder(
                        "file-repo", "default", tempDir.toURI().toString())
                .build();

        boolean result = service.isOldEnough("com.example", "artifact", "2.0.0", 7, fileRepo);
        assertFalse(result);
    }

    @Test
    void readsHttpLastModifiedHeader() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/repo/com/example/artifact/3.0.0/artifact-3.0.0.pom", exchange -> {
            exchange.getResponseHeaders().add("Last-Modified", "Sun, 13 Sep 2026 01:05:37 GMT");
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.start();
        try {
            RemoteRepository repo = new RemoteRepository.Builder(
                            "http-repo",
                            "default",
                            "http://127.0.0.1:" + server.getAddress().getPort() + "/repo")
                    .build();

            Optional<Instant> publicationDate = service.getPublicationDate("com.example", "artifact", "3.0.0", repo);

            assertTrue(publicationDate.isPresent());
            assertTrue(publicationDate.get().toString().startsWith("2026-09-13T01:05:37"));
        } finally {
            server.stop(0);
        }
    }

    @Test
    void supportsConcurrentCacheAccess() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/repo/com/example/artifact/3.1.0/artifact-3.1.0.pom", exchange -> {
            exchange.getResponseHeaders().add("Last-Modified", "Sun, 13 Sep 2026 01:05:37 GMT");
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        server.start();
        try {
            RemoteRepository repo = new RemoteRepository.Builder(
                            "http-repo",
                            "default",
                            "http://127.0.0.1:" + server.getAddress().getPort() + "/repo")
                    .build();
            ExecutorService executor = Executors.newFixedThreadPool(8);
            CountDownLatch start = new CountDownLatch(1);
            try {
                List<Future<Optional<Instant>>> futures = new ArrayList<>();
                for (int i = 0; i < 32; i++) {
                    futures.add(executor.submit(() -> {
                        start.await();
                        return service.getPublicationDate("com.example", "artifact", "3.1.0", repo);
                    }));
                }
                start.countDown();
                for (Future<Optional<Instant>> future : futures) {
                    Optional<Instant> publicationDate = future.get();
                    assertTrue(publicationDate.isPresent());
                }
            } finally {
                executor.shutdownNow();
            }
        } finally {
            server.stop(0);
        }
    }
}
