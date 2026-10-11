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
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.resolution.VersionRequest;
import org.eclipse.aether.resolution.VersionResult;
import org.junit.jupiter.api.Test;

import static org.apache.maven.api.plugin.testing.MojoExtension.setVariableValueToObject;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Scenarios of the {@code lock-snapshots} goal. The integration test repository has no snapshot metadata, so the
 * version resolution is mocked.
 */
@ProjectMojoTest
@Basedir("src/test/resources/org/codehaus/mojo/lock-snapshots")
class LockSnapshotsMojoScenarioTest {

    @Test
    @InjectMojo(goal = "lock-snapshots", pom = "lock-snapshots-property/pom.xml")
    void rewritesASnapshotVersionHeldInAProperty(LockSnapshotsMojo mojo) throws Exception {
        RepositorySystem repositorySystem = mock(RepositorySystem.class);
        when(repositorySystem.resolveVersion(any(), any())).then(i -> {
            VersionRequest request = i.getArgument(1);
            return new VersionResult(request).setVersion("3.4.0-20090311.051742-1");
        });
        setVariableValueToObject(mojo, "repositorySystem", repositorySystem);

        mojo.execute();

        String pom = new String(Files.readAllBytes(mojo.getProject().getFile().toPath()), StandardCharsets.UTF_8);
        assertTextContains(pom, "<api.version>3.4.0-20090311.051742-1</api.version>");
        assertTextContains(pom, "<version>${api.version}</version>");
    }
}
