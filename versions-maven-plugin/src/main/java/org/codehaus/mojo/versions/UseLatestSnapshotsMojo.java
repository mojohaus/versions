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

import java.util.Comparator;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.eclipse.aether.RepositorySystem;

/**
 * Replaces any release versions with the latest snapshot version (if it has been deployed).
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo(name = "use-latest-snapshots", threadSafe = true)
public class UseLatestSnapshotsMojo extends UseLatestVersionsMojoBase {

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 1.0-beta-1
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "false")
    protected boolean allowMajorUpdates;

    /**
     * Whether to allow the minor version number to be changed.
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     * @since 1.0-beta-1
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "false")
    protected boolean allowMinorUpdates;

    /**
     * Whether to allow the incremental version number to be changed.
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     * @since 1.0-beta-1
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseLatestSnapshotsMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    protected boolean isAllowMajorUpdates() {
        return allowMajorUpdates;
    }

    @Override
    protected boolean isAllowMinorUpdates() {
        return allowMinorUpdates;
    }

    @Override
    protected boolean isAllowIncrementalUpdates() {
        return allowIncrementalUpdates;
    }

    @Override
    protected final boolean updateFilter(Dependency dep) {
        return !ArtifactUtils.isSnapshot(dep.getVersion());
    }

    @Override
    protected final boolean artifactVersionsFilter(ArtifactVersion ver) {
        return ArtifactUtils.isSnapshot(ver.toString());
    }

    @Override
    protected Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream) {
        return stream.max(Comparator.naturalOrder());
    }

    @Override
    protected final boolean isAllowSnapshots() {
        return true;
    }

    @Override
    protected final boolean isAllowDowngrade() {
        return false;
    }
}
