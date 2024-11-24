package org.codehaus.mojo.versions.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.DisplayDependencyUpdatesMojo;
import org.codehaus.mojo.versions.DisplayExtensionUpdatesMojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;

import static java.util.Optional.empty;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.VERSION;

/**
 * Helper class for {@link DisplayDependencyUpdatesMojo} and {@link DisplayExtensionUpdatesMojo},
 * provides a way for formatting logging dependency updates
 */
public class DependencyUpdatesLoggingHelper {

    /**
     * @return {@code true} if the version of the dependency is in the project
     */
    private static boolean dependencyVersionLocalToProject(MavenProject project, Dependency dependency) {
        return dependency
                .getLocation(VERSION.toString())
                .getSource()
                .getModelId()
                .equals(project.getGroupId() + ":" + project.getArtifactId() + ":" + project.getVersion());
    }

    /**
     * Compiles a {@link DependencyUpdatesResult} object containing dependency updates for the given dependency map
     * and the given unchanged segment.
     * @param project a {@link MavenProject} object
     * @param updates map of available versions per dependency
     * @param allowSnapshots whether snapshots should be allowed as updates
     * @param unchangedSegment the most major segment not allowed to be updated or {@code Optional.empty()} if
     *                        all segments are allowed to be updated
     * @param maxLineWith maximum line width
     * @param displayManagedBy if {@code true}, will display information on the pom managing the given dependency
     *                         for dependencies not managed by the current project
     * @return a {@link DependencyUpdatesResult} object containing the result
     */
    public static DependencyUpdatesResult getDependencyUpdates(
            MavenProject project,
            Map<Dependency, ArtifactVersions> updates,
            boolean allowSnapshots,
            Optional<Segment> unchangedSegment,
            int maxLineWith,
            boolean displayManagedBy) {
        List<String> withUpdates = new ArrayList<>();
        List<String> usingCurrent = new ArrayList<>();
        for (Map.Entry<Dependency, ArtifactVersions> entry : updates.entrySet()) {
            Dependency dep = entry.getKey();
            ArtifactVersions versions = entry.getValue();
            String left = "  " + ArtifactUtils.versionlessKey(versions.getArtifact()) + " ";
            String currentVersion;
            Optional<ArtifactVersion> latestVersion;
            if (versions.getCurrentVersion() != null) {
                currentVersion = versions.getCurrentVersion()
                        + (!displayManagedBy || dependencyVersionLocalToProject(project, dep)
                                ? ""
                                : " (managed by "
                                        + dep.getLocation(VERSION.toString())
                                                .getSource()
                                                .getModelId() + ")");
                try {
                    latestVersion = versions.getNewestVersion(currentVersion, unchangedSegment, allowSnapshots, false);
                } catch (InvalidSegmentException e) {
                    latestVersion = empty();
                }
            } else {
                currentVersion = versions.getArtifact().getVersionRange().toString();
                ArtifactVersion actualVersion =
                        versions.getNewestVersion(versions.getArtifact().getVersionRange(), allowSnapshots);
                Restriction newVersionRestriction;
                try {
                    Restriction segmentRestriction =
                            versions.restrictionForUnchangedSegment(actualVersion, unchangedSegment, false);
                    newVersionRestriction = new Restriction(
                            actualVersion,
                            false,
                            segmentRestriction.getUpperBound(),
                            segmentRestriction.isUpperBoundInclusive());
                } catch (InvalidSegmentException e) {
                    throw new RuntimeException(e);
                }
                latestVersion = Optional.of(newVersionRestriction)
                        .map(restriction -> versions.getNewestVersion(restriction, allowSnapshots));
            }
            String right =
                    " " + latestVersion.map(v -> currentVersion + " -> " + v).orElse(currentVersion);
            List<String> t = latestVersion.isPresent() ? withUpdates : usingCurrent;
            if (right.length() + left.length() + 3 > maxLineWith) {
                t.add(left + "...");
                t.add(StringUtils.leftPad(right, maxLineWith));

            } else {
                t.add(StringUtils.rightPad(left, maxLineWith - right.length(), ".") + right);
            }
        }

        return new DependencyUpdatesResult() {
            @Override
            public List<String> getUsingLatest() {
                return usingCurrent;
            }

            @Override
            public List<String> getWithUpdates() {
                return withUpdates;
            }
        };
    }

    /**
     * Defines the list of dependencies using current versions and the list of dependencies having updates
     */
    public interface DependencyUpdatesResult {

        /**
         * @return Dependencies using the latest version
         */
        List<String> getUsingLatest();

        /**
         * @return Dependencies with updates available
         */
        List<String> getWithUpdates();
    }
}
