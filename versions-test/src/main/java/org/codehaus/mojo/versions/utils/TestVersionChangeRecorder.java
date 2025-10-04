package org.codehaus.mojo.versions.utils;

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

import javax.inject.Named;

import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorder;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.VersionChange;
import org.codehaus.mojo.versions.model.VersionsExecution;

@Named("test")
public class TestVersionChangeRecorder implements VersionChangeRecorderFactory, VersionChangeRecorder {
    private final VersionsExecution versionsExecution =
            new VersionsExecution().withDate(ZonedDateTime.now()).withGoal("test-goal");

    @Override
    public void recordChange(VersionChange versionChange) {
        versionsExecution.getVersionChanges().add(versionChange);
    }

    @Override
    public void writeReport(Path outputPath) {}

    @Override
    public String getDefaultFileName() {
        return "versions-changes.xml";
    }

    public List<VersionChange> getChanges() {
        return versionsExecution.getVersionChanges();
    }

    public static Map<String, VersionChangeRecorderFactory> asTestMap() {
        return Collections.singletonMap("none", new TestVersionChangeRecorder());
    }

    @Override
    public VersionChangeRecorder create(
            MavenSession mavenSession, MojoExecution mojoExecution, Map<String, String> ignored) {
        return this;
    }
}
