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
import javax.xml.stream.XMLStreamException;

import java.util.Comparator;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.model.DependencyChangeKind;
import org.codehaus.mojo.versions.model.DependencyVersionChange;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.mojo.versions.utils.DependencyBuilder;
import org.eclipse.aether.RepositorySystem;

import static org.apache.commons.lang3.StringUtils.isBlank;

/**
 * Sets the parent version to the latest parent version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo(name = "update-parent", threadSafe = true)
public class UpdateParentMojo extends UseLatestVersionsMojoBase {

    // ------------------------------ FIELDS ------------------------------

    /**
     * <p>If {@code skipResolution} is not set, specifies the <em>bottom</em> version considered
     * for target version resolution. If it is a version range, the resolved version will be
     * restricted by that range.</p>
     *
     * <p>If {@code skipResolution} is {@code true}, will specify the target version to which
     * the parent artifact will be updated.</p>
     * @since 1.0-alpha-1
     */
    @Parameter(property = "parentVersion")
    protected String parentVersion = null;

    /**
     * to update parent version by force when it is RELEASE or LATEST
     *
     * @since 2.9
     */
    @Parameter(property = "forceUpdate", defaultValue = "false")
    protected boolean forceUpdate = false;

    /**
     * Skips version resolution, only valid if {@code parentVersion} is set.
     * Will effectively set the new parent version to the one from {@code parentVersion}
     *
     * @since 2.13.0
     */
    @Parameter(property = "skipResolution", defaultValue = "false")
    protected boolean skipResolution = false;

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a version within the range fulfilling the criteria.</p>
     * <p>Default <code>false</code></p>
     *
     * @since 2.12.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates = true;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates = true;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.13.0
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates = true;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    private VersionRange targetVersionRange;

    private VersionsHelper helper;

    // -------------------------- OTHER METHODS --------------------------

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
    public UpdateParentMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
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
    protected boolean getAllowDowngrade() {
        return allowDowngrade;
    }

    @Override
    protected boolean updateFilter(Dependency dep) {
        return true;
    }

    @Override
    protected boolean artifactVersionsFilter(ArtifactVersion ver) {
        return targetVersionRange == null || ArtifactVersions.isVersionInRange(ver, targetVersionRange);
    }

    @Override
    protected Optional<ArtifactVersion> versionProducer(Stream<ArtifactVersion> stream) {
        Artifact artifact = artifactFactory.createArtifact(DependencyBuilder.newBuilder()
                .withGroupId(getProject().getParent().getGroupId())
                .withArtifactId(getProject().getParent().getArtifactId())
                .withVersion(getProject().getParent().getVersion())
                .withType("pom")
                .build());
        return stream.max(Comparator.naturalOrder()).map(candidate -> {
            // check if the new artifact can be resolved (or force it with forceUpdate)
            if (shouldApplyUpdate(artifact, getProject().getParent().getVersion(), candidate, forceUpdate)) {
                return candidate;
            } else {
                getLog().debug("Update not applied. Exiting.");
                return null;
            }
        });
    }

    @Override
    protected boolean getProcessDependencies() {
        return false;
    }

    @Override
    protected boolean getProcessDependencyManagement() {
        return false;
    }

    @Override
    public boolean getProcessParent() {
        return true;
    }

    @Override
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        if (getProject().getParent() == null) {
            getLog().info("Project does not have a parent");
            return;
        }

        if (reactorProjects.contains(getProject().getParent())) {
            getLog().info("Project's parent is part of the reactor");
            return;
        }

        // with !skipResolution, this plugin is basically a slightly modified use-*-* targeting parent only
        if (!skipResolution) {
            this.helper = getHelper();
            try {
                targetVersionRange = getTargetVersionRange().orElse(null);
            } catch (InvalidVersionSpecificationException e) {
                throw new MojoExecutionException("Invalid version range specification: " + parentVersion, e);
            }
            super.update(pom);
        } else {
            if (isBlank(parentVersion)) {
                throw new MojoExecutionException("skipResolution is only valid if parentVersion is set");
            }
            getLog().info("Updating parent from " + getProject().getParent().getVersion() + " to " + parentVersion);

            if (PomHelper.setProjectParentVersion(pom, parentVersion)) {
                if (getLog().isDebugEnabled()) {
                    getLog().debug("Made an update from "
                            + getProject().getParent().getVersion() + " to " + parentVersion);
                }
                getChangeRecorder()
                        .recordChange(new DependencyVersionChange()
                                .withKind(DependencyChangeKind.PARENT_UPDATE)
                                .withGroupId(getProject().getParent().getGroupId())
                                .withArtifactId(getProject().getParent().getArtifactId())
                                .withOldVersion(getProject().getParent().getVersion())
                                .withNewVersion(parentVersion));
            }
        }
    }

    private Optional<VersionRange> getTargetVersionRange() throws InvalidVersionSpecificationException {
        try {
            return Optional.ofNullable(parentVersion)
                    .filter(v -> !StringUtils.isBlank(v))
                    .map(v -> {
                        try {
                            VersionRange r = VersionRange.createFromVersionSpec(v);
                            if (r.getRecommendedVersion() != null) {
                                return r.restrict(VersionRange.createFromVersionSpec(r.getRecommendedVersion() + ",)"));
                            }
                            return r;
                        } catch (InvalidVersionSpecificationException e) {
                            throw new IllegalArgumentException(e);
                        }
                    });
        } catch (IllegalArgumentException e) {
            if (e.getCause() instanceof InvalidVersionSpecificationException) {
                throw (InvalidVersionSpecificationException) e.getCause();
            }
            throw e;
        }
    }
}
