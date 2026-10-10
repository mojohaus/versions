package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.apache.maven.api.plugin.testing.Basedir;
import org.apache.maven.api.plugin.testing.InjectMojo;
import org.apache.maven.api.plugin.testing.MojoParameter;
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.TextAssertions.assertTextContains;

/**
 * Scenarios of {@link UpdatePropertiesMojo} that used to be the {@code it-update-properties-NNN} integration tests.
 * Each test runs the POM of the former IT against the versions of its test repository.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/update-properties")
class UpdatePropertiesMojoScenarioTest {

    private String run(UpdatePropertiesMojo mojo) throws Exception {
        mojo.repositorySystem = ItRepository.repositorySystem();
        mojo.execute();
        return new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-001/pom.xml")
    void updatesPropertyConfiguredForADependency(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>3.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-003/pom.xml")
    void updatesPropertyUsedInARangeAndKeepsTheRange(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<version>[${api}]</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-006/pom.xml")
    void updatesTwoConfiguredProperties(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<impl>2.2</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-007/pom.xml")
    void updatesLinkedPropertyAndKeepsTheReference(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<version>${api}</version>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-008/pom.xml")
    void updatesPropertiesSharedBetweenDependencies(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>3.0</api>");
        assertTextContains(pom, "<impl>2.2</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-010/pom.xml")
    void updatesPropertyToTheConfiguredRange(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>3.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-015/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    void staysWithinTheMajorVersion(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.1</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-016/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    void staysWithinTheMajorVersionForConfiguredAndLinkedProperties(UpdatePropertiesMojo mojo) throws Exception {
        String pom = run(mojo);
        assertTextContains(pom, "<api>2.1</api>");
        assertTextContains(pom, "<impl>1.4</impl>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-019/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void keepsVersionWithoutIncrementalUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>2.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-021/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    void appliesIncrementalUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.0.1</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-022/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    @MojoParameter(name = "allowIncrementalUpdates", value = "false")
    void keepsVersionWithoutAnyUpdate(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.0</api>");
    }

    @Test
    @InjectMojo(goal = "update-properties", pom = "it-023/pom.xml")
    @MojoParameter(name = "allowMajorUpdates", value = "false")
    @MojoParameter(name = "allowMinorUpdates", value = "false")
    @MojoParameter(name = "allowIncrementalUpdates", value = "false")
    void appliesBuildNumberUpdateOnly(UpdatePropertiesMojo mojo) throws Exception {
        assertTextContains(run(mojo), "<api>1.1.0-2</api>");
    }
}
