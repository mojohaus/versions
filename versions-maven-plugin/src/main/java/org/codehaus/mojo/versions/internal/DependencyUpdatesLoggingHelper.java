package org.codehaus.mojo.versions.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.function.TriFunction;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.Restriction;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.DisplayDependencyUpdatesMojo;
import org.codehaus.mojo.versions.DisplayExtensionUpdatesMojo;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.Segment;
import org.codehaus.mojo.versions.model.VersionChange;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rule.RuleService;
import org.codehaus.mojo.versions.rule.RuleServiceUtils;
import org.codehaus.mojo.versions.utils.ArtifactFactory;

import static java.util.Optional.empty;
import static org.codehaus.mojo.versions.utils.DependencyBuilder.Location.VERSION;

/**
 * Helper class for {@link DisplayDependencyUpdatesMojo} and {@link DisplayExtensionUpdatesMojo},
 * provides a way for formatting logging dependency updates
 */
public class DependencyUpdatesLoggingHelper {

    private final RuleService ruleService;

    private final Optional<Segment> unchangedSegment;

    private final boolean allowSnapshots;

    private final int maxLineWidth;

    private final boolean displayManagedBy;

    private final MavenProject project;

    private final ArtifactFactory artifactFactory;

    private final Log log;

    /**
     * Constructs a new instance.
     *
     * @param project a {@link MavenProject} object
     * @param log a {@link Log} instance
     * @param artifactFactory {@link ArtifactFactory} instance
     * @param ruleService      a {@link RuleService} instance
     * @param unchangedSegment the most major version segment that is not to be changed or {@link Optional#empty()}
     *                         if all version segments can be changed
     * @param allowSnapshots   whether snapshots should be allowed as updates
     * @param maxLineWidth     maximum line width
     * @param displayManagedBy if {@code true}, will display information on the pom managing the given dependency
     *                         for dependencies not managed by the current project
     */
    @SuppressWarnings("checkstyle:parameterNumber")
    public DependencyUpdatesLoggingHelper(
            MavenProject project,
            Log log,
            ArtifactFactory artifactFactory,
            RuleService ruleService,
            Optional<Segment> unchangedSegment,
            boolean allowSnapshots,
            int maxLineWidth,
            boolean displayManagedBy) {
        this.project = project;
        this.log = log;
        this.artifactFactory = artifactFactory;
        this.ruleService = ruleService;
        this.unchangedSegment = unchangedSegment;
        this.allowSnapshots = allowSnapshots;
        this.maxLineWidth = maxLineWidth;
        this.displayManagedBy = displayManagedBy;
    }

    /**
     * @return {@code true} if the version of the dependency is in the project
     */
    private boolean dependencyVersionLocalToProject(Dependency dependency) {
        return dependency
                .getLocation(VERSION.toString())
                .getSource()
                .getModelId()
                .equals(project.getGroupId() + ":" + project.getArtifactId() + ":" + project.getVersion());
    }

    /**
     * <p>Compiles a {@link DependencyUpdatesResult} object containing dependency updates for the given dependency map
     * and the given unchanged segment.</p>
     * <p>The resulting dependencies are filtered using the include/exclude rules from the {@link RuleService}.</p>
     *
     * @param updates map of available versions per dependency
     * @param versionChangeProvider a producer of the {@link VersionChange} object for discovered updates per (dependency, old version and new version)
     * @return a {@link DependencyUpdatesResult} object containing the result
     */
    public DependencyUpdatesResult getDependencyUpdates(
            Map<Dependency, ArtifactVersions> updates,
            TriFunction<Dependency, String, String, ? extends VersionChange> versionChangeProvider) {
        List<String> withUpdates = new ArrayList<>();
        List<String> usingCurrent = new ArrayList<>();
        List<VersionChange> versionChanges = new ArrayList<>();
        for (Map.Entry<Dependency, ArtifactVersions> entry : updates.entrySet()) {
            Dependency dep = entry.getKey();
            ArtifactVersions versions = RuleServiceUtils.filterByRuleService(
                    dep.getGroupId(), dep.getArtifactId(), entry.getValue(), ruleService, log);

            String left = "  " + ArtifactUtils.versionlessKey(versions.getArtifact()) + " ";
            String currentVersion;
            Optional<ArtifactVersion> latestVersion;
            if (versions.getCurrentVersion() != null) {
                currentVersion = versions.getCurrentVersion()
                        + (!displayManagedBy || dependencyVersionLocalToProject(dep)
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
            List<String> t = latestVersion
                    .map(v -> {
                        versionChanges.add(versionChangeProvider.apply(dep, currentVersion, v.toString()));
                        return withUpdates;
                    })
                    .orElse(usingCurrent);
            if (right.length() + left.length() + 3 > maxLineWidth) {
                t.add(left + "...");
                t.add(StringUtils.leftPad(right, maxLineWidth));

            } else {
                t.add(StringUtils.rightPad(left, maxLineWidth - right.length(), ".") + right);
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

            @Override
            public List<VersionChange> getVersionChanges() {
                return versionChanges;
            }
        };
    }

    /**
     * Defines the list of dependencies using current versions and the list of dependencies having updates
     */
    public interface DependencyUpdatesResult {

        /**
         * Returns the list of dependencies using the latest version available (i.e. no update available)
         *
         * @return Dependencies using the latest version
         */
        List<String> getUsingLatest();

        /**
         * Returns the list of dependencies with updates available (i.e. not using the latest version)
         *
         * @return Dependencies with updates available
         */
        List<String> getWithUpdates();

        /**
         * Returns a list of {@link VersionChange} describing all registered version changes in the
         * {@link org.codehaus.mojo.versions.api.recording.VersionChangeRecorder} comprehension
         *
         * @return a list of {@link VersionChange} objects describing the changes
         */
        List<VersionChange> getVersionChanges();
    }
}
