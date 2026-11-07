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
import org.codehaus.mojo.versions.api.PomHelper;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TestUtils.copyDir;
import static org.codehaus.mojo.versions.utils.TestUtils.createTempDir;
import static org.codehaus.mojo.versions.utils.TestUtils.tearDownTempDir;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextMatches;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Basic tests for {@linkplain SetPropertyMojoTest}.
 *
 * @author Andrzej Jarmoniuk
 */
@ProjectMojoTest
public class SetPropertyMojoTest {
    protected ProjectMojos mojos;

    private Path pomDir;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        pomDir = createTempDir("set-property");
    }

    @AfterEach
    public void tearDown() throws Exception {
        tearDownTempDir(pomDir);
    }

    @Test
    public void testNullNewVersion() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-new-version"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", pomDir);

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", null);

        mojo.execute();

        String output = String.join(
                        "", Files.readAllLines(mojo.getProject().getFile().toPath()))
                .replaceAll("\\s*", "");
        assertTextMatches(output, ".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*");
    }

    @Test
    public void testNewVersionEmpty() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-new-version"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", pomDir);

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", "");

        mojo.execute();

        String output = String.join(
                        "", Files.readAllLines(mojo.getProject().getFile().toPath()))
                .replaceAll("\\s*", "");
        assertTextMatches(output, ".*<properties>.*<dummy-api-version></dummy-api-version>.*</properties>.*");
    }

    @Test
    public void testNullProperty() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/null-property"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", pomDir);

        try {
            mojo.update(null);
            fail();
        } catch (MojoExecutionException e) {
            assertTextContains(
                    e.getMessage(), "Please provide either 'property' or 'propertiesVersionsFile' parameter.");
        }
    }

    @Test
    public void testChangeOnlyPropertiesInTheProfileWithPropertyAlsoInTheModule() throws Exception {
        testChangeOnlyPropertiesInTheProfile(true);
    }

    @Test
    public void testChangeOnlyPropertiesInTheProfileWithPropertyOnlyInProfile() throws Exception {
        testChangeOnlyPropertiesInTheProfile(false);
    }

    private void testChangeOnlyPropertiesInTheProfile(boolean withPropertyInTheModule) throws Exception {
        final String newVersion = UUID.randomUUID().toString();
        final Model model = getModelForProfile("test-profile", true, newVersion, withPropertyInTheModule);

        if (withPropertyInTheModule) {
            assertEquals("1.0.0", model.getProperties().getProperty("dummy-api-version"));
        }
        assertEquals(
                newVersion,
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"));
    }

    @Test
    public void testKeepPropertiesInTheProfile() throws Exception {
        final String newVersion = UUID.randomUUID().toString();
        final Model model = getModelForProfile("test-profile", false, newVersion);

        assertEquals(newVersion, model.getProperties().getProperty("dummy-api-version"));
        assertEquals(
                "test-value",
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"));
    }

    @Test
    public void testEmptyValue() throws Exception {
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/empty-value"), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", pomDir);

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", "1.0.0");

        mojo.execute();

        String output = String.join(
                        "", Files.readAllLines(mojo.getProject().getFile().toPath()))
                .replaceAll("\\s*", "");
        assertTextMatches(output, ".*<properties>.*<dummy-api-version>1.0.0</dummy-api-version>.*</properties>.*");
    }

    @Test
    public void testDoNotChangePropertyIfTheProfileNotfound() throws Exception {
        final Model model =
                getModelForProfile("new-profile", true, UUID.randomUUID().toString());

        assertEquals("1.0.0", model.getProperties().getProperty("dummy-api-version"));
        assertEquals(
                0L,
                model.getProfiles().stream()
                        .filter(profile -> "new-profile".equals(profile.getId()))
                        .count());
        assertEquals(
                "test-value",
                model.getProfiles().stream()
                        .filter(profile -> "test-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile test-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"));
    }

    private Model getModelForProfile(String profileName, Boolean setProfile, String newVersion) throws Exception {
        return getModelForProfile(profileName, setProfile, newVersion, true);
    }

    private Model getModelForProfile(
            String profileName, Boolean setProfile, String newVersion, Boolean withPropsInModule) throws Exception {
        final String srcModule = withPropsInModule
                ? "src/test/resources/org/codehaus/mojo/set-property/profiled-new-version"
                : "src/test/resources/org/codehaus/mojo/set-property/profiled-new-version-without-props";
        copyDir(Paths.get(srcModule), pomDir);
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", pomDir);

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

    @Test
    public void testSetNew() throws Exception {
        String newVersion = UUID.randomUUID().toString();
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/issue-1268"), pomDir);
        Path childPomDir = pomDir.resolve("child");
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", childPomDir);

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", newVersion);
        setVariableValueToObject(mojo, "property", "dummy-api-version");
        setVariableValueToObject(mojo, "insert", true);
        mojo.execute();

        Model model = PomHelper.getRawModel(
                Paths.get(childPomDir.toAbsolutePath().toString(), "pom.xml").toFile());
        assertEquals(newVersion, model.getProperties().getProperty("dummy-api-version"));
    }

    @Test
    public void testSetNewWithProfile() throws Exception {
        String newVersion = UUID.randomUUID().toString();
        copyDir(Paths.get("src/test/resources/org/codehaus/mojo/set-property/issue-1268"), pomDir);
        Path childPomDir = pomDir.resolve("child");
        SetPropertyMojo mojo = (SetPropertyMojo) mojos.lookup("set-property", childPomDir);

        mojo.repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        when(mojo.repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                .then(i -> new VersionRangeResult(i.getArgument(1)));

        setVariableValueToObject(mojo, "newVersion", newVersion);
        setVariableValueToObject(mojo, "property", "dummy-api-version");
        setVariableValueToObject(mojo, "profileId", "new-profile");
        setVariableValueToObject(mojo, "insert", true);
        mojo.execute();

        Model model = PomHelper.getRawModel(
                Paths.get(childPomDir.toAbsolutePath().toString(), "pom.xml").toFile());
        assertEquals(
                newVersion,
                model.getProfiles().stream()
                        .filter(profile -> "new-profile".equals(profile.getId()))
                        .findFirst()
                        .orElseThrow(() -> new NoSuchElementException("profile new-profile not found"))
                        .getProperties()
                        .getProperty("dummy-api-version"));
    }
}
