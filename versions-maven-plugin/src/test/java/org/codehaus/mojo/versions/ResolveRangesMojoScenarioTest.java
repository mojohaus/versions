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
import org.junit.jupiter.api.Test;

import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;

/**
 * Scenarios of the {@code resolve-ranges} goal that used to be integration tests. Each test runs the POM of the
 * former IT against the versions of its test repository.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/resolve-ranges")
class ResolveRangesMojoScenarioTest {

    @Test
    @InjectMojo(goal = "resolve-ranges", pom = "resolve-ranges-001/pom.xml")
    void resolvesEachRangeToItsHighestVersion(ResolveRangesMojo mojo) throws Exception {
        mojo.repositorySystem = ItRepository.repositorySystem();
        mojo.execute();

        String pom = new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
        // [1.1, 2.0) of dummy-impl and [1.1.2, 1.1.3 ] of dummy-api, with a space before the bracket
        assertTextContains(pom, "<version>1.4</version>", "<version>1.1.3</version>");
    }
}
