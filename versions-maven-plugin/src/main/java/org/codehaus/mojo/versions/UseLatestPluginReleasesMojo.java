package org.codehaus.mojo.versions;

/*
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

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

/**
 * Replaces any plugin versions with the latest release versions, ignoring snapshots.
 *
 * @since 2.20.0
 */
@Mojo(name = "use-latest-plugin-releases", threadSafe = true)
public class UseLatestPluginReleasesMojo extends UsePluginVersionsMojoBase {

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.20.0
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.20.0
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.20.0
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * Creates a new instance.
     *
     * @param artifactFactory  the artifact factory
     * @param repositorySystem the repository system
     * @param wagonMap         the map of wagon implementations
     * @param changeRecorders  the change recorders
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public UseLatestPluginReleasesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    @Override
    protected boolean getAllowMajorUpdates() {
        return allowMajorUpdates;
    }

    @Override
    protected boolean getAllowMinorUpdates() {
        return allowMinorUpdates;
    }

    @Override
    protected boolean getAllowIncrementalUpdates() {
        return allowIncrementalUpdates;
    }

    @Override
    protected Optional<ArtifactVersion> selectVersionForPlugin(
            Artifact artifact, String currentVersion, Optional<Segment> unchangedSegment)
            throws VersionRetrievalException, InvalidSegmentException, MojoExecutionException {
        ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, true);
        // Get newer versions: (currentVersion, unchangedSegment, includeSnapshots, allowDowngrade)
        // We don't allow snapshots (false) and don't allow downgrades (false)
        ArtifactVersion[] newerVersions = versions.getNewerVersions(currentVersion, unchangedSegment, false, false);

        return Arrays.stream(newerVersions).max(ArtifactVersion::compareTo);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return false;
    }
}
