package org.codehaus.mojo.versions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.utils.ArtifactVersionService;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import static java.util.Optional.ofNullable;

/**
 * Finds the minimal Maven version required to build a Maven project.
 * Evaluates the {@code maven-enforcer-plugin:enforce} goal and
 * its {@code requireMavenVersion} rule.
 *
 * @see <a href="https://maven.apache.org/enforcer/enforcer-rules/requireMavenVersion.html">Require Maven Version Rule</a>
 */
class MinimalMavenBuildVersionFinder {

    private MinimalMavenBuildVersionFinder() {
        // not supposed to be created, static methods only
    }

    static Optional<ArtifactVersion> getGreatestVersion(ArtifactVersion... v) {
        return Arrays.stream(v).filter(Objects::nonNull).reduce((v1, v2) -> {
            if (v1.compareTo(v2) >= 0) {
                return v1;
            }
            return v2;
        });
    }

    static ArtifactVersion find(MavenProject mavenProject, Log log) {
        return getGreatestVersion(
                        getEnforcerMavenVersion(mavenProject, log),
                        ofNullable(mavenProject.getPrerequisites())
                                .map(Prerequisites::getMaven)
                                .map(ArtifactVersionService::getArtifactVersion)
                                .orElse(null))
                .orElse(null);
    }

    private static ArtifactVersion getEnforcerMavenVersion(MavenProject mavenProject, Log log) {
        List<Plugin> buildPlugins = mavenProject.getBuildPlugins();
        if (null == buildPlugins) {
            log.debug("MinimalMavenBuildVersionFinder: No build plugins found");
            return null;
        }

        Plugin mavenEnforcerPlugin = getMavenEnforcerPlugin(buildPlugins);
        if (null == mavenEnforcerPlugin) {
            log.debug("MinimalMavenBuildVersionFinder: No maven-enforcer-plugin used");
            return null;
        }

        List<PluginExecution> pluginExecutions = mavenEnforcerPlugin.getExecutions();
        if (null == pluginExecutions) {
            log.debug("MinimalMavenBuildVersionFinder: No executions of maven-enforcer-plugin found");
            return null;
        }

        List<PluginExecution> pluginExecutionsWithEnforceGoal = getPluginExecutionsWithEnforceGoal(pluginExecutions);
        if (pluginExecutionsWithEnforceGoal.isEmpty()) {
            log.debug("MinimalMavenBuildVersionFinder: No 'enforce' execution of maven-enforcer-plugin found");
            return null;
        }

        Xpp3Dom requireMavenVersionTag = getRequireMavenVersionTag(pluginExecutionsWithEnforceGoal);
        if (null == requireMavenVersionTag) {
            log.debug("MinimalMavenBuildVersionFinder: "
                    + "No 'requireMavenVersion' rule of maven-enforcer-plugin found");
            return null;
        }

        Xpp3Dom versionTag = requireMavenVersionTag.getChild("version");
        if (null == versionTag) {
            log.debug("MinimalMavenBuildVersionFinder: "
                    + "No version specified in 'requireMavenVersion' rule of maven-enforcer-plugin");
            return null;
        }

        String versionTagValue = versionTag.getValue();
        if (null == versionTagValue || "".equals(versionTagValue)) {
            log.debug("MinimalMavenBuildVersionFinder: "
                    + "Empty version specified in 'requireMavenVersion' rule of maven-enforcer-plugin");
            return null;
        }
        ArtifactVersion minimumVersion = getMinimumVersionFromRange(versionTagValue);
        log.debug("Calculated minimum version " + minimumVersion + " from version parameter value '" + versionTagValue
                + "'");
        return minimumVersion;
    }

    private static Plugin getMavenEnforcerPlugin(List<Plugin> buildPlugins) {
        for (Plugin plugin : buildPlugins) {
            if ("maven-enforcer-plugin".equals(plugin.getArtifactId())
                    && "org.apache.maven.plugins".equals(plugin.getGroupId())) {
                return plugin;
            }
        }
        return null;
    }

    private static List<PluginExecution> getPluginExecutionsWithEnforceGoal(List<PluginExecution> executions) {
        List<PluginExecution> pluginExecutions = new ArrayList<>();
        for (PluginExecution pluginExecution : executions) {
            List<String> goals = pluginExecution.getGoals();
            if (goals != null && goals.contains("enforce")) {
                pluginExecutions.add(pluginExecution);
            }
        }
        return pluginExecutions;
    }

    private static Xpp3Dom getRequireMavenVersionTag(List<PluginExecution> executions) {
        for (PluginExecution pluginExecution : executions) {
            Xpp3Dom configurationTag = (Xpp3Dom) pluginExecution.getConfiguration();
            if (null == configurationTag) {
                continue;
            }

            Xpp3Dom rulesTag = configurationTag.getChild("rules");
            if (null == rulesTag) {
                continue;
            }

            Xpp3Dom requireMavenVersionTag = rulesTag.getChild("requireMavenVersion");
            if (null == requireMavenVersionTag) {
                continue;
            }

            return requireMavenVersionTag;
        }
        return null;
    }

    /**
     * The below method implements the specification found at
     * https://maven.apache.org/enforcer/enforcer-rules/versionRanges.html
     */
    static ArtifactVersion getMinimumVersionFromRange(String versionRange) {

        int openIndicesCount = 0;
        int closeIndicesCount = 0;

        for (int i = 0; i < versionRange.length(); i++) {
            char character = versionRange.charAt(i);

            if ('(' == character || '[' == character) {
                openIndicesCount++;
            } else if (')' == character || ']' == character) {
                closeIndicesCount++;
            }
        }

        if (openIndicesCount != closeIndicesCount) {
            return null;
        }

        if (openIndicesCount == 0) {
            return ArtifactVersionService.getArtifactVersion(versionRange);
        }

        if (!((versionRange.charAt(0) == '[' || versionRange.charAt(0) == '(')
                && (versionRange.charAt(versionRange.length() - 1) == ']'
                        || versionRange.charAt(versionRange.length() - 1) == ')'))) {
            return null;
        }

        if (openIndicesCount != 1) {
            return null;
        }

        String innerString = versionRange.substring(1, versionRange.length() - 1);

        int commaIndex = innerString.indexOf(',');

        if (commaIndex == -1) {
            if (versionRange.charAt(0) == '[' && versionRange.charAt(versionRange.length() - 1) == ']') {
                return ArtifactVersionService.getArtifactVersion(innerString);
            } else {
                return null;
            }
        }

        if (commaIndex == 0) {
            return null;
        }

        if (commaIndex == innerString.length() - 1) {
            String minimumVersion = innerString.substring(0, innerString.length() - 1);

            if (versionRange.charAt(0) == '[' && versionRange.charAt(versionRange.length() - 1) == ')') {
                return ArtifactVersionService.getArtifactVersion(minimumVersion);
            }

            if (versionRange.charAt(0) == '(' && versionRange.charAt(versionRange.length() - 1) == ')') {
                // this is actually wrong - the Maven version should be higher than this,
                // the Maven version cannot be equal to this, but the Maven Enforcer plugin should capture this
                return ArtifactVersionService.getArtifactVersion(minimumVersion);
            }

            return null;
        }

        String minimumVersion = innerString.substring(0, commaIndex);

        if (versionRange.charAt(0) == '[') {
            return ArtifactVersionService.getArtifactVersion(minimumVersion);
        }

        if (versionRange.charAt(0) == '(') {
            // this is actually wrong - the Maven version should be higher than this,
            // the Maven version cannot be equal to this, but the Maven Enforcer plugin should capture this
            return ArtifactVersionService.getArtifactVersion(minimumVersion);
        }

        return null;
    }
}
