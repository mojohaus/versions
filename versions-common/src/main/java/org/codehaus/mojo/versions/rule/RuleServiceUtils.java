package org.codehaus.mojo.versions.rule;

import java.util.Collection;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.api.IgnoreVersionHelper;
import org.codehaus.mojo.versions.model.IgnoreVersion;

/**
 * Utility class for filtering artifact versions based on rules.
 *
 * @since 2.20.0
 */
public class RuleServiceUtils {

    /**
     * Filters the given {@link ArtifactVersions} by ignoring versions as specified by the {@link RuleService} for the
     * given artifact.
     *
     * @param groupId          groupId of the artifact for which to filter versions
     * @param artifactId       artifactId of the artifact for which to filter versions
     * @param artifactVersions the {@link ArtifactVersions} to filter
     * @param ruleService      the {@link RuleService} to retrieve ignored versions from
     * @param log              the {@link Log} for debug output
     * @return a new {@link ArtifactVersions} instance with ignored versions filtered out
     * @since 2.20.0
     */
    public static ArtifactVersions filterByRuleService(
            String groupId, String artifactId, ArtifactVersions artifactVersions, RuleService ruleService, Log log) {
        Collection<IgnoreVersion> ignoredVersions;
        ignoredVersions = ruleService.getIgnoredVersions(groupId, artifactId);
        String artifactKey = groupId + ":" + artifactId;
        if (!ignoredVersions.isEmpty() && log.isDebugEnabled()) {
            log.debug("Found ignored versions: " + ignoredVersions + " for artifact " + artifactKey);
        }
        return new ArtifactVersions(artifactVersions)
                .filter(version -> ignoredVersions.stream().noneMatch(i -> {
                    if (IgnoreVersionHelper.isVersionIgnored(version, i)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Version " + version + " for artifact "
                                    + artifactKey
                                    + " found on ignore list: "
                                    + i);
                        }
                        return true;
                    }
                    return false;
                }));
    }
}
