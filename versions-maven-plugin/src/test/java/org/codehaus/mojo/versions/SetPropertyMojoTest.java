package org.codehaus.mojo.versions;

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

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.NoSuchElementException;
import java.util.UUID;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.api.PomHelper;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import static org.codehaus.mojo.versions.utils.TestUtils.copyDir;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesPattern;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
public class SetPropertyMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path pomDir;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        pomDir = createTempDir("set-property");
    }

    @After
    public void tearDown() throws Exception {
        try {
            tearDownTempDir(pomDir);
        } finally {
            super.tearDown();
        }
    }

    @Test
    public void testNullNewVersion() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-new-version"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo(pomDir.toFile(), "set-property");

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", null);

        mojo.execute();

        String output = String.join(
                        "", Files.readAllLines(mojo.getProject().getFile().toPath()))
                .replaceAll("\\s*", "");
        assertThat(output, matchesPattern(".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*"));
    }

    @Test
    public void testNewVersionEmpty() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-new-version"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo(pomDir.toFile(), "set-property");

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", "");

        mojo.execute();

        String output = String.join(
                        "", Files.readAllLines(mojo.getProject().getFile().toPath()))
                .replaceAll("\\s*", "");
        assertThat(output, matchesPattern(".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*"));
    }

    @Test
    public void testNullProperty() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-property"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo(pomDir.toFile(), "set-property");

        try {
            mojo.update(null);
            fail();
        } catch (MojoExecutionException e) {
            assertThat(
                    e.getMessage(),
                    containsString("Please provide either 'property' or 'propertiesVersionsFile' parameter."));
        }
    }

    @Test
    public void testChangeOnlyPropertiesInTheProfile() throws Exception {
        final String newVersion = UUID.randomUUID().toString();
        final Model model = getModelForProfile("test-profile", true, newVersion);

        assertThat(model.getProperties().getProperty("dummy-api-version"), is("1.0.0"));
        assertThat(
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"),
                is(newVersion));
    }

    @Test
    public void testKeepPropertiesInTheProfile() throws Exception {
        final String newVersion = UUID.randomUUID().toString();
        final Model model = getModelForProfile("test-profile", false, newVersion);

        assertThat(model.getProperties().getProperty("dummy-api-version"), is(newVersion));
        assertThat(
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"),
                is("test-value"));
    }

    @Test
    public void testDoNotChangePropertyIfTheProfileNotfound() throws Exception {
        final Model model =
                getModelForProfile("new-profile", true, UUID.randomUUID().toString());

        assertThat(model.getProperties().getProperty("dummy-api-version"), is("1.0.0"));
        assertThat(
                model.getProfiles().stream()
                        .filter(profile -> "new-profile".equals(profile.getId()))
                        .count(),
                is(0L));
        assertThat(
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"),
                is("test-value"));
    }

    private Model getModelForProfile(String profileName, Boolean setProfile, String newVersion) throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/profiled-new-version"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojoRule.lookupConfiguredMojo(pomDir.toFile(), "set-property");

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", newVersion);

        if (setProfile) {
            setVariableValueToObject(mojo, "profileId", profileName);
        }

        mojo.execute();

        final Model model = PomHelper.getRawModel(
                Paths.get(pomDir.toAbsolutePath().toString(), "pom.xml").toFile());
        return model;
    }
}
