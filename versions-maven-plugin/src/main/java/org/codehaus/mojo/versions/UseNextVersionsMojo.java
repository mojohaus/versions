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

import javax.inject.Inject;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.eclipse.aether.RepositorySystem;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo(name = "use-next-versions", threadSafe = true)
public class UseNextVersionsMojo extends UseLatestVersionsMojoBase {

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a non-snapshot version within the range fulfilling the criteria.</p>
     * <p>Only valid if <code>allowSnapshots</code> is <code>false</code>.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter(property = "processParent", defaultValue = "false")
    private boolean processParent = false;

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseNextVersionsMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    protected boolean getProcessDependencies() {
        return processDependencies;
    }

    @Override
    protected boolean getProcessDependencyManagement() {
        return processDependencyManagement;
    }

    @Override
    public boolean getProcessParent() {
        return processParent;
    }

    @Override
    protected boolean getAllowMajorUpdates() {
        return true;
    }

    @Override
    protected boolean getAllowMinorUpdates() {
        return true;
    }

    @Override
    protected boolean getAllowIncrementalUpdates() {
        return true;
    }

    @Override
    protected boolean getAllowDowngrade() {
        return allowDowngrade;
    }

    @Override
    protected boolean updateFilter(Dependency dep) {
        return true;
    }

    @Override
    protected boolean artifactVersionsFilter(ArtifactVersion ver) {
        return true;
    }

    @Override
    protected Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream) {
        return stream.findFirst();
    }
}
