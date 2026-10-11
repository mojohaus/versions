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
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextNotContains;

/**
 * Scenarios of the {@code unlock-snapshots} goal that used to be integration tests.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/unlock-snapshots")
class UnlockSnapshotsMojoScenarioTest {

    @Test
    @InjectMojo(goal = "unlock-snapshots", pom = "unlock-snapshots-001/pom.xml")
    void replacesATimestampedSnapshotWithSnapshot(UnlockSnapshotsMojo mojo) throws Exception {
        mojo.execute();

        String pom = new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
        assertTextContains(pom, "<version>3.4.0-SNAPSHOT</version>");
        assertTextNotContains(pom, "20090311.051742-1");
    }
}
