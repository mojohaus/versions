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
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.ordering.VersionComparators;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.InvalidVersionSpecificationException;
import org.eclipse.aether.version.Version;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.Test;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Collections.singletonMap;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.core.IsIterableContaining.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
    @Test
    public void testPerRuleVersionsIgnored() throws Exception {
        final org.eclipse.aether.RepositorySystem repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        final Artifact artifact = mock(Artifact.class);
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
        assertThat(actual, hasItems("three", "1.200", "illegalVersion"));
    }

    @Test
    public void testGlobalRuleVersionsIgnored() throws Exception {
        final org.eclipse.aether.RepositorySystem repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
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
        assertThat(actual, hasItems(one, two, three, illegal));
    }

    @Test
    public void testWildcardMatching() throws Exception {
        assertTrue(DefaultVersionsHelper.exactMatch("*", "com.foo.bar"));
        assertFalse(DefaultVersionsHelper.exactMatch("com.bar*", "com-bar"));
        assertTrue(DefaultVersionsHelper.exactMatch("com?foo.bar", "com.foo.bar"));
        assertTrue(DefaultVersionsHelper.exactMatch("co*.foo.b?r", "com.foo.bar"));
        assertTrue(DefaultVersionsHelper.exactMatch("c*oo*r", "com.foo.bar"));
    }

    @Test
    public void testRuleSets() throws Exception {
        VersionsHelper helper = createHelper();

        assertEquals(
                VersionComparators.getVersionComparator("maven"),
                helper.getVersionComparator("net.foo", "bar"),
                "no match gives default");
        assertEquals(
                VersionComparators.getVersionComparator("mercury"),
                helper.getVersionComparator("org.apache.maven", "plugins"),
                "matches wildcard");
        assertEquals(
                VersionComparators.getVersionComparator("mercury"),
                helper.getVersionComparator("com.mycompany.custom.maven", "plugins"),
                "exact match wins over initial match");
        assertEquals(
                VersionComparators.getVersionComparator("maven"),
                helper.getVersionComparator("com.mycompany.maven.plugins", "plugins"),
                "non-wildcard prefix wins over wildcard prefix match");
        assertEquals(
                VersionComparators.getVersionComparator("maven"),
                helper.getVersionComparator("com.mycompany.maven", "new-maven-plugin"));
        assertEquals(
                VersionComparators.getVersionComparator("mercury"),
                helper.getVersionComparator("com.mycompany.maven", "old-maven-plugin"));
    }

    @Test
    void testMVERSIONS159ExcludedAndNotIncluded() throws Exception {
        VersionsHelper helper = createHelper();
        MavenProject project = null;

        Property[] propertyDefinitions = new Property[] {new Property("bar.version")};
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

    @Test
    void testIsClasspathUriDetectsClassPathProtocol() throws Exception {
        String uri = "classpath:/p/a/c/k/a/g/e/resource.res";

        assertThat(DefaultVersionsHelper.isClasspathUri(uri), CoreMatchers.is(true));
    }

    @Test
    void testIsClasspathUriDetectsThatItIsDifferentProtocol() throws Exception {
        String uri = "http://10.10.10.10/p/a/c/k/a/g/e/resource.res";

        assertThat(DefaultVersionsHelper.isClasspathUri(uri), CoreMatchers.is(false));
    }

    private DefaultVersionsHelper createHelper() throws Exception {
        return createHelper(null);
    }

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

    private DefaultVersionsHelper createHelper(org.eclipse.aether.RepositorySystem aetherRepositorySystem)
            throws Exception {
        final String resourcePath = "/" + getClass().getPackage().getName().replace('.', '/') + "/rules.xml";
        final String rulesUri =
                Objects.requireNonNull(getClass().getResource(resourcePath)).toExternalForm();
        MavenSession mavenSession = mock(MavenSession.class);
        when(mavenSession.getCurrentProject()).thenReturn(mock(MavenProject.class));
        when(mavenSession.getCurrentProject().getRemotePluginRepositories()).thenReturn(emptyList());
        when(mavenSession.getCurrentProject().getRemotePluginRepositories()).thenReturn(emptyList());
        when(mavenSession.getRepositorySession()).thenReturn(new DefaultRepositorySystemSession());

        return new DefaultVersionsHelper.Builder()
                .withRepositorySystem(mock(RepositorySystem.class))
                .withAetherRepositorySystem(aetherRepositorySystem)
                .withWagonMap(singletonMap("file", mockFileWagon(new URI(rulesUri))))
                .withServerId("")
                .withRulesUri(rulesUri)
                .withLog(mock(Log.class))
                .withMavenSession(mavenSession)
                .withMojoExecution(mock(MojoExecution.class))
                .build();
    }

    @Test
    void testIgnoredVersionsShouldBeTheOnlyPresentInAnEmptyRuleSet() throws MojoExecutionException {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog(new SystemStreamLog())
                .withIgnoredVersions(Arrays.asList(".*-M.", ".*-SNAPSHOT"))
                .build();
        RuleSet ruleSet = versionsHelper.getRuleSet();
        assertThat(ruleSet.getIgnoreVersions(), hasSize(2));
        assertThat(
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()),
                containsInAnyOrder(".*-M.", ".*-SNAPSHOT"));
    }

    @Test
    void testDefaultsShouldBePresentInAnEmptyRuleSet() throws MojoExecutionException, IllegalAccessException {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog(new SystemStreamLog())
                .withIgnoredVersions(singletonList(".*-M."))
                .build();
        RuleSet ruleSet = versionsHelper.getRuleSet();
        assertThat(ruleSet.getComparisonMethod(), is("maven"));
    }

    @Test
    void testIgnoredVersionsShouldExtendTheRuleSet() throws MojoExecutionException, IllegalAccessException {
        DefaultVersionsHelper versionsHelper = new DefaultVersionsHelper.Builder()
                .withLog(new SystemStreamLog())
                .withRuleSet(new RuleSet() {
                    {
                        setIgnoreVersions(new ArrayList<>(singletonList(new IgnoreVersion() {
                            {
                                setVersion("1.0.0");
                            }
                        })));
                        setRules(singletonList(new Rule() {
                            {
                                setGroupId("org.slf4j");
                                setArtifactId("slf4j-api");
                                setIgnoreVersions(singletonList(new IgnoreVersion() {
                                    {
                                        setType("regex");
                                        setVersion("^[^1]\\.*");
                                    }
                                }));
                            }
                        }));
                    }
                })
                .withIgnoredVersions(Arrays.asList(".*-M.", ".*-SNAPSHOT"))
                .build();
        RuleSet ruleSet = versionsHelper.getRuleSet();
        assertThat(ruleSet.getIgnoreVersions(), hasSize(3));
        assertThat(
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()),
                containsInAnyOrder(".*-M.", ".*-SNAPSHOT", "1.0.0"));
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
                DefaultVersionsHelper.adjustRemoteRepositoriesRefreshPolicy(Arrays.asList(repo1, repo2, repo3));

        assertThat(remoteRepositories, hasSize(3));
        assertThat(remoteRepositories.get(0), not(is(repo1)));
        assertThat(remoteRepositories.get(1), not(is(repo2)));
        assertThat(remoteRepositories.get(2), is(repo3));

        assertThat(
                remoteRepositories.get(0).getPolicy(true).getUpdatePolicy(),
                equalTo(RepositoryPolicy.UPDATE_POLICY_DAILY));
        assertThat(
                remoteRepositories.get(0).getPolicy(false).getUpdatePolicy(),
                equalTo(RepositoryPolicy.UPDATE_POLICY_DAILY));

        assertThat(
                remoteRepositories.get(1).getPolicy(true).getUpdatePolicy(),
                equalTo(RepositoryPolicy.UPDATE_POLICY_NEVER));
        assertThat(
                remoteRepositories.get(1).getPolicy(false).getUpdatePolicy(),
                equalTo(RepositoryPolicy.UPDATE_POLICY_DAILY));
    }

    private static Version parseVersion(String version) {
        try {
            return new GenericVersionScheme().parseVersion(version);
        } catch (InvalidVersionSpecificationException e) {
            throw new RuntimeException(e);
        }
    }
}
