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
import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.SegmentUtils;
import org.eclipse.aether.RepositorySystem;

/**
 * Replaces any plugin versions with the latest release versions, ignoring snapshots.
 *
 * @since 2.20.0
 */
@Mojo(name = "use-latest-plugin-releases", threadSafe = true)
public class UseLatestPluginReleasesMojo extends AbstractVersionsUpdaterMojo {

    /**
     * Whether to allow the major version number to be changed.
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * Whether to process the plugins section of the project.
     */
    @Parameter(property = "processPlugins", defaultValue = "true")
    private boolean processPlugins = true;

    /**
     * Whether to process the pluginManagement section of the project.
     */
    @Parameter(property = "processPluginManagement", defaultValue = "true")
    private boolean processPluginManagement = true;

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
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                allowMajorUpdates, allowMinorUpdates, allowIncrementalUpdates, getLog());

        Collection<Plugin> plugins = new ArrayList<>();

        if (processPlugins
                && getProject().getBuild() != null
                && getProject().getBuild().getPlugins() != null) {
            plugins.addAll(getProject().getBuild().getPlugins());
        }

        if (processPluginManagement
                && getProject().getBuild() != null
                && getProject().getBuild().getPluginManagement() != null
                && getProject().getBuild().getPluginManagement().getPlugins() != null) {
            plugins.addAll(getProject().getBuild().getPluginManagement().getPlugins());
        }

        updatePlugins(pom, plugins, unchangedSegment);
    }

    private void updatePlugins(
            MutableXMLStreamReader pom, Collection<Plugin> plugins, Optional<Segment> unchangedSegment)
            throws MojoExecutionException, VersionRetrievalException, XMLStreamException {
        for (Plugin plugin : plugins) {
            String version = plugin.getVersion();
            if (version == null || version.trim().isEmpty()) {
                getLog().debug("Skipping plugin " + plugin.getGroupId() + ":" + plugin.getArtifactId()
                        + " - no version specified");
                continue;
            }

            if (version.startsWith("${")) {
                getLog().debug("Skipping plugin " + plugin.getGroupId() + ":" + plugin.getArtifactId()
                        + " - version is a property reference: " + version);
                continue;
            }

            getLog().debug("Looking for updates to plugin " + plugin.getGroupId() + ":" + plugin.getArtifactId()
                    + " from " + version);

            Artifact artifact =
                    artifactFactory.createMavenPluginArtifact(plugin.getGroupId(), plugin.getArtifactId(), version);

            try {
                ArtifactVersions versions = getHelper().lookupArtifactVersions(artifact, true);
                // Get newer versions: (currentVersion, unchangedSegment, includeSnapshots, allowDowngrade)
                // We don't allow snapshots (false) and don't allow downgrades (false)
                ArtifactVersion[] newerVersions = versions.getNewerVersions(version, unchangedSegment, false, false);

                Optional<ArtifactVersion> selectedVersion =
                        Arrays.stream(newerVersions).max(ArtifactVersion::compareTo);

                if (selectedVersion.isPresent()
                        && !version.equals(selectedVersion.get().toString())) {
                    if (PomHelper.setPluginVersion(
                            pom,
                            plugin.getGroupId(),
                            plugin.getArtifactId(),
                            version,
                            selectedVersion.get().toString())) {
                        getLog().info("Updated " + plugin.getGroupId() + ":" + plugin.getArtifactId() + " from "
                                + version + " to " + selectedVersion.get());
                    }
                }
            } catch (InvalidSegmentException e) {
                throw new MojoExecutionException("Error filtering versions: " + e.getMessage(), e);
            }
        }
    }

    @Override
    protected boolean getAllowSnapshots() {
        return false;
    }
}
