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
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.codehaus.mojo.versions.rule.RulesServiceBuilder;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.InvalidVersionSpecificationException;
import org.eclipse.aether.version.Version;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test {@link DefaultVersionsHelper}
 */
class DefaultVersionsHelperTest {

    @Mock
    ArtifactFactory artifactFactory;

    @Mock
    PomHelper pomHelper;

    @Mock
    RepositorySystem repositorySystem;

    @Mock
    Artifact artifact;

    @Mock
    MavenSession mavenSession;

    @Mock
    Log log;

    @BeforeEach
    public void beforeEach() {
        MockitoAnnotations.openMocks(this);
        when(mavenSession.getCurrentProject()).thenReturn(mock(MavenProject.class));
        when(mavenSession.getCurrentProject().getRemotePluginRepositories()).thenReturn(emptyList());
        when(mavenSession.getCurrentProject().getRemotePluginRepositories()).thenReturn(emptyList());
        when(mavenSession.getRepositorySession()).thenReturn(new DefaultRepositorySystemSession());
    }

    @Test
    public void testPerRuleVersionsIgnored() throws Exception {
        when(artifact.getGroupId()).thenReturn("com.mycompany.maven");
        when(artifact.getArtifactId()).thenReturn("artifact-one");
        when(artifact.getType()).thenReturn("jar");
        when(artifact.getArtifactHandler()).thenReturn(new DefaultArtifactHandler("default"));
        when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1))
                        .setVersions(Arrays.asList(
                                parseVersion("one"),
                                parseVersion("two"),
                                parseVersion("three"),
                                parseVersion("1.200"),
                                parseVersion("illegalVersion"))));

        VersionsHelper helper = createHelper(repositorySystem);

        final ArtifactVersions versions = helper.lookupArtifactVersions(artifact, true);

        final List<String> actual = Arrays.stream(versions.getVersions(true))
                .map(ArtifactVersion::toString)
                .collect(Collectors.toList());

        assertEquals(3, actual.size());
        assertTrue(actual.containsAll(Arrays.asList("three", "1.200", "illegalVersion")), () -> String.valueOf(actual));
    }

    @Test
    public void testGlobalRuleVersionsIgnored() throws Exception {
        final Artifact artifact = mock(Artifact.class);
        when(artifact.getGroupId()).thenReturn("other.company");
        when(artifact.getArtifactId()).thenReturn("artifact-two");
        when(artifact.getType()).thenReturn("jar");
        when(artifact.getArtifactHandler()).thenReturn(new DefaultArtifactHandler("default"));

        final List<Version> artifactVersions = new ArrayList<>();

        final Version one = parseVersion("one");
        final Version two = parseVersion("two");
        final Version three = parseVersion("three");
        artifactVersions.add(one);
        artifactVersions.add(two);
        artifactVersions.add(parseVersion("three-alpha"));
        artifactVersions.add(parseVersion("three-beta"));
        artifactVersions.add(three);
        final Version illegal = parseVersion("illegalVersion");
        artifactVersions.add(illegal);

        when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)).setVersions(artifactVersions));

        VersionsHelper helper = createHelper(repositorySystem);

        final ArtifactVersions versions = helper.lookupArtifactVersions(artifact, true);

        final List<Version> actual = Arrays.stream(versions.getVersions(true))
                .map(ArtifactVersion::toString)
                .map(DefaultVersionsHelperTest::parseVersion)
                .collect(Collectors.toList());

        assertEquals(4, actual.size());
        assertTrue(actual.containsAll(Arrays.asList(one, two, three, illegal)), () -> String.valueOf(actual));
    }

    @Test
    void testMVERSIONS159ExcludedAndNotIncluded() throws Exception {
        VersionsHelper helper = createHelper();
        MavenProject project = null;

        List<Property> propertyDefinitions = Collections.singletonList(new Property("bar.version"));
        // should not throw an IllegalStateException
        Map<Property, PropertyVersions> result =
                helper.getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                        .withMavenProject(project)
                        .withPropertyDefinitions(propertyDefinitions)
                        .withIncludeProperties("foo.version")
                        .withExcludeProperties("bar.version")
                        .withIncludeParent(false)
                        .withAutoLinkItems(false)
                        .build());
        assertTrue(result.isEmpty());
    }

    private static final List<String> PROPERTY_NAMES =
            Arrays.asList("a.version", "b.version", "c.version", "d.version");

    private static Property propertyOn(String propertyName) {
        Dependency dependency = new Dependency();
        dependency.setGroupId("localhost");
        dependency.setArtifactId(propertyName.replace(".version", "-artifact"));
        dependency.setVersion("1.0");
        Property property = new Property(propertyName);
        property.setAutoLinkDependencies(false);
        property.setDependencies(new Dependency[] {dependency});
        return property;
    }

    private Map<Property, PropertyVersions> versionPropertiesMapOf(DefaultVersionsHelper helper) throws Exception {
        when(artifactFactory.createArtifact(any(Dependency.class))).then(i -> {
            Dependency d = i.getArgument(0);
            return new DefaultArtifact(
                    d.getGroupId(),
                    d.getArtifactId(),
                    d.getVersion(),
                    "compile",
                    "jar",
                    null,
                    new DefaultArtifactHandler("jar"));
        });
        MavenProject project = mock(MavenProject.class);
        when(project.getProperties()).thenReturn(new java.util.Properties());
        return helper.getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                .withMavenProject(project)
                .withPropertyDefinitions(PROPERTY_NAMES.stream()
                        .map(DefaultVersionsHelperTest::propertyOn)
                        .collect(Collectors.toList()))
                .withIncludeParent(false)
                .withAutoLinkItems(false)
                .build());
    }

    @Test
    void testVersionPropertiesMapKeepsContentAndOrder() throws Exception {
        when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> {
                    VersionRangeRequest request = i.getArgument(1);
                    String artifactId = request.getArtifact().getArtifactId();
                    return new VersionRangeResult(request)
                            .setVersions(Arrays.asList(parseVersion("1.0"), parseVersion("2." + artifactId.charAt(0))));
                });

        Map<Property, PropertyVersions> result = versionPropertiesMapOf(createHelper(repositorySystem));

        // the order is that of a HashMap keyed by property name, as before properties were looked up concurrently
        Map<String, Object> expectedOrder = new HashMap<>();
        PROPERTY_NAMES.forEach(n -> expectedOrder.put(n, n));
        assertEquals(
                new ArrayList<>(expectedOrder.keySet()),
                result.keySet().stream().map(Property::getName).collect(Collectors.toList()));
        assertEquals(
                new ArrayList<>(expectedOrder.keySet()),
                result.values().stream().map(PropertyVersions::getName).collect(Collectors.toList()));
        for (Map.Entry<Property, PropertyVersions> e : new LinkedHashMap<>(result).entrySet()) {
            String letter = e.getKey().getName().substring(0, 1);
            assertEquals(
                    Arrays.asList("1.0", "2." + letter),
                    Arrays.stream(e.getValue().getVersions(true))
                            .map(ArtifactVersion::toString)
                            .collect(Collectors.toList()));
        }
    }

    @Test
    void testVersionPropertiesMapLooksUpPropertiesConcurrently() throws Exception {
        CountDownLatch allInFlight = new CountDownLatch(PROPERTY_NAMES.size());
        when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> {
                    allInFlight.countDown();
                    // a sequential implementation never gets the other lookups started, so the wait times out
                    if (!allInFlight.await(5, TimeUnit.SECONDS)) {
                        throw new VersionRangeResolutionException(
                                new VersionRangeResult(i.getArgument(1)), "not concurrent");
                    }
                    return new VersionRangeResult(i.getArgument(1)).setVersions(Arrays.asList(parseVersion("1.0")));
                });

        Map<Property, PropertyVersions> result = versionPropertiesMapOf(createHelper(repositorySystem));

        assertEquals(PROPERTY_NAMES.size(), result.size());
    }

    @Test
    void testVersionPropertiesMapPropagatesRetrievalFailure() throws Exception {
        when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> {
                    VersionRangeRequest request = i.getArgument(1);
                    if (request.getArtifact().getArtifactId().startsWith("c-")) {
                        throw new VersionRangeResolutionException(new VersionRangeResult(request), "boom");
                    }
                    return new VersionRangeResult(request).setVersions(Arrays.asList(parseVersion("1.0")));
                });

        MojoExecutionException e = assertThrows(
                MojoExecutionException.class, () -> versionPropertiesMapOf(createHelper(repositorySystem)));
        assertInstanceOf(VersionRetrievalException.class, e.getCause());
    }

    private DefaultVersionsHelper createHelper() throws Exception {
        return createHelper(mock(RepositorySystem.class));
    }

    @Deprecated
    private static Wagon mockFileWagon(URI rulesUri)
            throws AuthenticationException, ConnectionException, AuthorizationException, TransferFailedException,
                    ResourceDoesNotExistException {
        Wagon fileWagon = mock(Wagon.class);
        doNothing()
                .when(fileWagon)
                .connect(
                        any(org.apache.maven.wagon.repository.Repository.class),
                        any(AuthenticationInfo.class),
                        any(ProxyInfo.class));
        doAnswer(i -> {
                    File tempFile = i.getArgument(1);
                    Files.copy(Paths.get(rulesUri), tempFile.toPath(), REPLACE_EXISTING);
                    return null;
                })
                .when(fileWagon)
                .get(anyString(), any(File.class));
        return fileWagon;
    }

    private DefaultVersionsHelper createHelper(RepositorySystem repositorySystem) throws Exception {
        final String resourcePath = "/" + getClass().getPackage().getName().replace('.', '/') + "/rules.xml";
        final String rulesUri =
                Objects.requireNonNull(getClass().getResource(resourcePath)).toExternalForm();

        return new DefaultVersionsHelper.Builder()
                .withArtifactFactory(artifactFactory)
                .withPomHelper(pomHelper)
                .withRepositorySystem(repositorySystem)
                .withLog(log)
                .withMavenSession(mavenSession)
                .withRuleService(new RulesServiceBuilder()
                        .withMavenSession(mavenSession)
                        .withWagonMap(singletonMap("file", mockFileWagon(new URI(rulesUri))))
                        .withServerId("")
                        .withRulesUri(rulesUri)
                        .withMavenSession(mavenSession)
                        .withLog(log)
                        .build())
                .build();
    }

    @Test
    void testRemoteRepositoryWithNeverUpdatePolicyShouldBeChangToDaily() {

        RemoteRepository repo1 = new RemoteRepository.Builder("id1", "", "")
                .setSnapshotPolicy(new RepositoryPolicy(
                        true, RepositoryPolicy.UPDATE_POLICY_NEVER, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .setReleasePolicy(new RepositoryPolicy(
                        true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .build();

        RemoteRepository repo2 = new RemoteRepository.Builder("id2", "", "")
                .setSnapshotPolicy(new RepositoryPolicy(
                        false, RepositoryPolicy.UPDATE_POLICY_NEVER, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .setReleasePolicy(new RepositoryPolicy(
                        true, RepositoryPolicy.UPDATE_POLICY_NEVER, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .build();

        RemoteRepository repo3 = new RemoteRepository.Builder("id3", "", "")
                .setSnapshotPolicy(new RepositoryPolicy(
                        true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .setReleasePolicy(new RepositoryPolicy(
                        true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
                .build();

        List<RemoteRepository> remoteRepositories =
                DefaultVersionsHelper.forceDailyRemoteRepositoriesRefreshPolicy(Arrays.asList(repo1, repo2, repo3));

        assertEquals(3, remoteRepositories.size(), () -> String.valueOf(remoteRepositories));
        assertNotEquals(repo1, remoteRepositories.get(0));
        assertNotEquals(repo2, remoteRepositories.get(1));
        assertEquals(repo3, remoteRepositories.get(2));

        assertEquals(
                RepositoryPolicy.UPDATE_POLICY_DAILY,
                remoteRepositories.get(0).getPolicy(true).getUpdatePolicy());
        assertEquals(
                RepositoryPolicy.UPDATE_POLICY_DAILY,
                remoteRepositories.get(0).getPolicy(false).getUpdatePolicy());

        assertEquals(
                RepositoryPolicy.UPDATE_POLICY_NEVER,
                remoteRepositories.get(1).getPolicy(true).getUpdatePolicy());
        assertEquals(
                RepositoryPolicy.UPDATE_POLICY_DAILY,
                remoteRepositories.get(1).getPolicy(false).getUpdatePolicy());
    }

    private static Version parseVersion(String version) {
        try {
            return new GenericVersionScheme().parseVersion(version);
        } catch (InvalidVersionSpecificationException e) {
            throw new RuntimeException(e);
        }
    }
}
