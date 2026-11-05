package org.codehaus.mojo.versions.rule;

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
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.utils.FileTransporterProvider;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.Authentication;
import org.eclipse.aether.repository.Proxy;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.spi.connector.transport.GetTask;
import org.eclipse.aether.spi.connector.transport.Transporter;
import org.eclipse.aether.spi.connector.transport.TransporterProvider;
import org.eclipse.aether.transfer.NoTransporterException;
import org.eclipse.aether.util.repository.AuthenticationBuilder;
import org.eclipse.aether.util.repository.DefaultAuthenticationSelector;
import org.eclipse.aether.util.repository.DefaultMirrorSelector;
import org.eclipse.aether.util.repository.DefaultProxySelector;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests loading of {@code rulesUri} through a {@link TransporterProvider}.
 */
class RulesServiceBuilderTest {
    private static final String RULES =
            "<ruleset><ignoreVersions><ignoreVersion type=\"regex\">.*-alpha</ignoreVersion></ignoreVersions></ruleset>";

    @Mock
    private Log log;

    @Mock
    private MavenSession mavenSession;

    private DefaultRepositorySystemSession repositorySession;

    @TempDir
    private Path tempDir;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        repositorySession = new DefaultRepositorySystemSession();
        when(mavenSession.getRepositorySession()).thenReturn(repositorySession);
    }

    private RulesServiceBuilder builder(TransporterProvider provider, String rulesUri) {
        return new RulesServiceBuilder()
                .withMavenSession(mavenSession)
                .withLog(log)
                .withServerId("rules-server")
                .withTransporterProvider(provider)
                .withRulesUri(rulesUri);
    }

    @Test
    void testRulesAreLoadedFromFileUri() throws Exception {
        Path rules = Files.write(tempDir.resolve("rules.xml"), RULES.getBytes(StandardCharsets.UTF_8));

        RuleSet ruleSet = builder(new FileTransporterProvider(), rules.toUri().toString())
                .build()
                .getRuleSet();

        assertEquals(1, ruleSet.getIgnoreVersions().size());
        assertEquals(".*-alpha", ruleSet.getIgnoreVersions().get(0).getVersion());
    }

    @Test
    void testMissingFileIsReportedWithTheUri() {
        String uri = tempDir.resolve("missing.xml").toUri().toString();

        MojoExecutionException e = assertThrows(
                MojoExecutionException.class,
                () -> builder(new FileTransporterProvider(), uri).build());

        assertEquals("Could not load specified rules from " + uri, e.getMessage());
        assertNotNull(e.getCause());
    }

    @Test
    void testMalformedRulesAreReportedWithTheUri() throws IOException {
        String uri = Files.write(tempDir.resolve("bad.xml"), "<ruleset".getBytes(StandardCharsets.UTF_8))
                .toUri()
                .toString();

        MojoExecutionException e = assertThrows(
                MojoExecutionException.class,
                () -> builder(new FileTransporterProvider(), uri).build());

        assertEquals("Could not load specified rules from " + uri, e.getMessage());
    }

    @Test
    void testMissingTransportNamesTheProtocol() throws Exception {
        TransporterProvider provider = mock(TransporterProvider.class);
        when(provider.newTransporter(any(), any()))
                .thenThrow(
                        new NoTransporterException(new RemoteRepository.Builder("x", "default", "scp://h/").build()));

        MojoExecutionException e = assertThrows(
                MojoExecutionException.class,
                () -> builder(provider, "scp://example.org/rules/rules.xml").build());

        assertEquals(
                "Could not load specified rules from scp://example.org/rules/rules.xml:"
                        + " no transport available for protocol \"scp\"",
                e.getMessage());
        assertInstanceOf(NoTransporterException.class, e.getCause());
    }

    @Test
    void testMissingProviderIsReported() {
        MojoExecutionException e = assertThrows(
                MojoExecutionException.class,
                () -> builder(null, "https://example.org/rules.xml").build());

        assertTrue(e.getMessage().startsWith("Could not load specified rules from https://example.org/rules.xml"));
    }

    @Test
    void testRepositoryCarriesServerAuthenticationAndProxyButNoMirror() throws Exception {
        Authentication authentication = new AuthenticationBuilder()
                .addUsername("user")
                .addPassword("secret")
                .build();
        DefaultAuthenticationSelector authenticationSelector = new DefaultAuthenticationSelector();
        authenticationSelector.add("rules-server", authentication);
        repositorySession.setAuthenticationSelector(authenticationSelector);
        DefaultProxySelector proxySelector = new DefaultProxySelector();
        proxySelector.add(new Proxy("http", "proxy.example.org", 3128), null);
        repositorySession.setProxySelector(proxySelector);
        DefaultMirrorSelector mirrorSelector = new DefaultMirrorSelector();
        mirrorSelector.add("mirror", "https://mirror.example.org/", "default", false, false, "*", null);
        repositorySession.setMirrorSelector(mirrorSelector);

        Transporter transporter = servingRules();
        TransporterProvider provider = mock(TransporterProvider.class);
        when(provider.newTransporter(any(RepositorySystemSession.class), any(RemoteRepository.class)))
                .thenReturn(transporter);

        builder(provider, "https://example.org/rules/rules.xml").build();

        ArgumentCaptor<RemoteRepository> captor = ArgumentCaptor.forClass(RemoteRepository.class);
        verify(provider).newTransporter(any(RepositorySystemSession.class), captor.capture());
        RemoteRepository repository = captor.getValue();
        assertEquals("rules-server", repository.getId());
        assertEquals("https://example.org/rules/", repository.getUrl());
        assertEquals(authentication, repository.getAuthentication());
        assertEquals("proxy.example.org", repository.getProxy().getHost());
        assertTrue(repository.getMirroredRepositories().isEmpty());

        ArgumentCaptor<GetTask> task = ArgumentCaptor.forClass(GetTask.class);
        verify(transporter).get(task.capture());
        assertEquals("rules.xml", task.getValue().getLocation().toString());
        verify(transporter).close();
    }

    @Test
    void testNoAuthenticationWhenServerIdIsNotConfigured() throws Exception {
        Transporter transporter = servingRules();
        TransporterProvider provider = mock(TransporterProvider.class);
        when(provider.newTransporter(any(RepositorySystemSession.class), any(RemoteRepository.class)))
                .thenReturn(transporter);

        builder(provider, "https://example.org/rules.xml").build();

        ArgumentCaptor<RemoteRepository> captor = ArgumentCaptor.forClass(RemoteRepository.class);
        verify(provider).newTransporter(any(RepositorySystemSession.class), captor.capture());
        assertNull(captor.getValue().getAuthentication());
        assertNull(captor.getValue().getProxy());
    }

    @Test
    void testTransporterIsClosedWhenTheTransferFails() throws Exception {
        Transporter transporter = mock(Transporter.class);
        doAnswer(i -> {
                    throw new IOException("boom");
                })
                .when(transporter)
                .get(any(GetTask.class));
        TransporterProvider provider = mock(TransporterProvider.class);
        when(provider.newTransporter(any(RepositorySystemSession.class), any(RemoteRepository.class)))
                .thenReturn(transporter);

        MojoExecutionException e = assertThrows(
                MojoExecutionException.class,
                () -> builder(provider, "https://example.org/rules.xml").build());

        assertEquals("boom", Objects.requireNonNull(e.getCause()).getMessage());
        verify(transporter).close();
    }

    @Test
    void testClasspathUriDoesNotNeedAProvider() throws Exception {
        RuleSet ruleSet = builder(null, "classpath:///org/codehaus/mojo/versions/rule/rules.xml")
                .build()
                .getRuleSet();

        assertEquals(3, ruleSet.getIgnoreVersions().size());
    }

    private static Transporter servingRules() throws Exception {
        Transporter transporter = mock(Transporter.class);
        doAnswer(i -> {
                    GetTask task = i.getArgument(0);
                    try (OutputStream os = task.newOutputStream()) {
                        os.write(RULES.getBytes(StandardCharsets.UTF_8));
                    }
                    return null;
                })
                .when(transporter)
                .get(any(GetTask.class));
        return transporter;
    }
}
