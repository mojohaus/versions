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

import javax.xml.stream.XMLStreamException;

import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.InvalidVersionSpecificationException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.repository.RepositorySystem;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.ArtifactAssociation;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.PropertyVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.recording.DefaultDependencyChangeRecord;
import org.codehaus.mojo.versions.recording.DefaultPropertyChangeRecord;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.codehaus.mojo.versions.api.Segment.INCREMENTAL;
import static org.codehaus.mojo.versions.api.Segment.MAJOR;
import static org.codehaus.mojo.versions.api.Segment.MINOR;

/**
 * Common base class for {@link UpdatePropertiesMojo}
 * and {@link UpdatePropertyMojo}
 */
public abstract class UpdatePropertiesMojoBase extends AbstractVersionsDependencyUpdaterMojo {
    /**
     * Whether properties linking versions should be auto-detected or not.
     *
     * @since 1.0-alpha-2
     */
    @Parameter(property = "autoLinkItems", defaultValue = "true")
    protected boolean autoLinkItems;

    /**
     * If a property points to a version like <code>1.2.3</code> and your repository contains versions like
     * <code>1.2.3</code> and <code>1.1.0</code> without settings this to <code>true</code> the property will never
     * being changed back to <code>1.1.0</code> by using <code>-DnewVersion=[1.1.0]</code>.
     *
     * @since 3.0.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    /**
     * Whether to allow the major version number to be changed.
     *
     * @since 2.4
     */
    @Parameter(property = "allowMajorUpdates", defaultValue = "true")
    protected boolean allowMajorUpdates;

    /**
     * <p>Whether to allow the minor version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates} {@code false}</b></p>
     *
     * @since 2.4
     */
    @Parameter(property = "allowMinorUpdates", defaultValue = "true")
    protected boolean allowMinorUpdates;

    /**
     * <p>Whether to allow the incremental version number to be changed.</p>
     *
     * <p><b>Note: {@code false} also implies {@linkplain #allowMajorUpdates}
     * and {@linkplain #allowMinorUpdates} {@code false}</b></p>
     *
     * @since 2.4
     */
    @Parameter(property = "allowIncrementalUpdates", defaultValue = "true")
    protected boolean allowIncrementalUpdates;

    /**
     * <p>Whether to include parent POMs in the search. Default: {@code true}</p>
     * <p>Setting this to {@code false} can speed up execution, but will not resolve
     * property-bound dependencies, defined in parent POMs.
     *
     * @since 2.14.0
     */
    @Parameter(property = "includeParent", defaultValue = "true")
    protected boolean includeParent = true;

    public UpdatePropertiesMojoBase(
            RepositorySystem repositorySystem,
            org.eclipse.aether.RepositorySystem aetherRepositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(repositorySystem, aetherRepositorySystem, wagonMap, changeRecorders);
    }

    protected void update(ModifiedPomXMLEventReader pom, Map<Property, PropertyVersions> propertyVersions)
            throws XMLStreamException {
        for (Map.Entry<Property, PropertyVersions> entry : propertyVersions.entrySet()) {
            Property property = entry.getKey();
            PropertyVersions version = entry.getValue();

            final String currentVersion = getProject().getProperties().getProperty(property.getName());
            if (currentVersion == null) {
                continue;
            }
            boolean canUpdateProperty = true;
            for (ArtifactAssociation association : version.getAssociations()) {
                if (!(isIncluded(association.getArtifact()))) {
                    getLog().info("Not updating the property ${" + property.getName()
                            + "} because it is used by artifact "
                            + association.getArtifact().toString()
                            + " and that artifact is not included in the list of "
                            + " allowed artifacts to be updated.");
                    canUpdateProperty = false;
                    break;
                }
            }

            if (canUpdateProperty) {
                Log log = getLog();
                if (log != null && !allowIncrementalUpdates) {
                    log.info("Assuming allowMinorUpdates false because allowIncrementalUpdates is false.");
                }

                if (log != null && !allowMinorUpdates) {
                    log.info("Assuming allowMajorUpdates false because allowMinorUpdates is false.");
                }

                Optional<Segment> unchangedSegment1 = allowMajorUpdates && allowMinorUpdates && allowIncrementalUpdates
                        ? empty()
                        : allowMinorUpdates && allowIncrementalUpdates
                                ? of(MAJOR)
                                : allowIncrementalUpdates ? of(MINOR) : of(INCREMENTAL);
                if (log != null && log.isDebugEnabled()) {
                    log.debug(unchangedSegment1
                                    .map(Segment::minorTo)
                                    .map(Segment::toString)
                                    .orElse("ALL") + " version changes allowed");
                }
                Optional<Segment> unchangedSegment = unchangedSegment1;
                try {
                    ArtifactVersion targetVersion = updatePropertyToNewestVersion(
                            pom, property, version, currentVersion, allowDowngrade, unchangedSegment);

                    if (targetVersion != null) {
                        getChangeRecorder()
                                .recordChange(DefaultPropertyChangeRecord.builder()
                                        .withProperty(property.getName())
                                        .withOldValue(currentVersion)
                                        .withNewValue(targetVersion.toString())
                                        .build());

                        for (final ArtifactAssociation association : version.getAssociations()) {
                            if ((isIncluded(association.getArtifact()))) {
                                getChangeRecorder()
                                        .recordChange(DefaultDependencyChangeRecord.builder()
                                                .withKind(DependencyChangeRecord.ChangeKind.PROPERTY)
                                                .withArtifact(association.getArtifact())
                                                .withOldVersion(currentVersion)
                                                .withNewVersion(targetVersion.toString())
                                                .build());
                            }
                        }
                    }
                } catch (InvalidSegmentException | InvalidVersionSpecificationException | MojoExecutionException e) {
                    getLog().warn(String.format(
                            "Skipping the processing of %s:%s due to: %s",
                            property.getName(), property.getVersion(), e.getMessage()));
                }
            }
        }
    }
}
