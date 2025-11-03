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

import javax.xml.stream.XMLStreamException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
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
 * Common base class for plugin version update mojos.
 *
 * @since 2.20.0
 */
public abstract class UsePluginVersionsMojoBase extends AbstractVersionsUpdaterMojo {

    /**
     * Whether to process the plugins section of the project.
     *
     * @since 2.20.0
     */
    @Parameter(property = "processPlugins", defaultValue = "true")
    private boolean processPlugins = true;

    /**
     * Whether to process the pluginManagement section of the project.
     *
     * @since 2.20.0
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
    protected UsePluginVersionsMojoBase(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * Whether to allow major version updates.
     *
     * @return {@code true} if major updates are allowed
     */
    protected abstract boolean getAllowMajorUpdates();

    /**
     * Whether to allow minor version updates.
     *
     * @return {@code true} if minor updates are allowed
     */
    protected abstract boolean getAllowMinorUpdates();

    /**
     * Whether to allow incremental version updates.
     *
     * @return {@code true} if incremental updates are allowed
     */
    protected abstract boolean getAllowIncrementalUpdates();

    /**
     * Determines which version to select for the given plugin.
     *
     * @param artifact the artifact representing the plugin
     * @param currentVersion the current version
     * @param unchangedSegment the segment that should not be changed
     * @return the selected version, or {@link Optional#empty()} if no suitable version is found
     * @throws VersionRetrievalException if version retrieval fails
     * @throws InvalidSegmentException if segment is invalid
     * @throws MojoExecutionException if execution fails
     */
    protected abstract Optional<ArtifactVersion> selectVersionForPlugin(
            Artifact artifact, String currentVersion, Optional<Segment> unchangedSegment)
            throws VersionRetrievalException, InvalidSegmentException, MojoExecutionException;

    @Override
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        Optional<Segment> unchangedSegment = SegmentUtils.determineUnchangedSegment(
                getAllowMajorUpdates(), getAllowMinorUpdates(), getAllowIncrementalUpdates(), getLog());

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

    /**
     * Updates plugin versions.
     *
     * @param pom the POM stream reader
     * @param plugins the plugins to update
     * @param unchangedSegment the segment that should not be changed
     * @throws MojoExecutionException if execution fails
     * @throws VersionRetrievalException if version retrieval fails
     * @throws XMLStreamException if XML processing fails
     */
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
                Optional<ArtifactVersion> selectedVersion = selectVersionForPlugin(artifact, version, unchangedSegment);

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
}
