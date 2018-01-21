package org.codehaus.mojo.versions;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import java.util.ArrayList;
import java.util.List;

/**
 * Finds the minimum Maven version required by a Maven project.
 * Checks for the existence of both the prerequisites.maven property and for maven-enforcer-plugin:enforce goal.
 * Returns null if no minimum version is found.
 * Checks project and it's parents recursively.
 *
 * Pros: works with Maven 3.5.0 which throws a warning if prerequisites.maven is set for a non Maven-Plugin project.
 * Cons: tightly coupled with the maven-enforcer-plugin and the Xpp3Dom configuration tag.
 */
class RequiredMavenVersionFinder {

    private final MavenProject mavenProject;

    RequiredMavenVersionFinder(MavenProject mavenProject) {
        this.mavenProject = mavenProject;
    }

    ArtifactVersion find() {
        ArtifactVersion childMavenVersion = getHighestArtifactVersion(getPrerequisitesMavenVersion(), getEnforcerMavenVersion());

        if (!mavenProject.hasParent()) {
            return childMavenVersion;
        }

        ArtifactVersion parentMavenVersion = new RequiredMavenVersionFinder(mavenProject.getParent()).find();

        return getHighestArtifactVersion(childMavenVersion, parentMavenVersion);
    }

    private ArtifactVersion getPrerequisitesMavenVersion() {
        Prerequisites prerequisites = mavenProject.getPrerequisites();
        if (null == prerequisites) {
            return null;
        }

        String prerequisitesMavenValue = prerequisites.getMaven();
        if (null == prerequisitesMavenValue) {
            return null;
        }

        return new DefaultArtifactVersion(prerequisitesMavenValue);
    }

    private ArtifactVersion getEnforcerMavenVersion() {
        List<Plugin> buildPlugins = mavenProject.getBuildPlugins();
        if (null == buildPlugins) {
            return null;
        }

        Plugin mavenEnforcerPlugin = getMavenEnforcerPlugin(buildPlugins);
        if (null == mavenEnforcerPlugin) {
            return null;
        }

        List<PluginExecution> pluginExecutions = mavenEnforcerPlugin.getExecutions();
        if (null == pluginExecutions) {
            return null;
        }

        List<PluginExecution> pluginExecutionsWithEnforceGoal = getPluginExecutionsWithEnforceGoal(pluginExecutions);
        if (pluginExecutionsWithEnforceGoal.isEmpty()) {
            return null;
        }
        
        Xpp3Dom requireMavenVersionTag = getRequireMavenVersionTag(pluginExecutionsWithEnforceGoal);
        if (null == requireMavenVersionTag) {
            return null;
        }

        Xpp3Dom versionTag = requireMavenVersionTag.getChild("version");
        if (null == versionTag) {
            return null;
        }

        String versionTagValue = versionTag.getValue();
        if (null == versionTagValue || "".equals(versionTagValue)) {
            return null;
        }

        return processMavenVersionRange(versionTagValue);
    }

    private Plugin getMavenEnforcerPlugin(List<Plugin> buildPlugins) {
        for (Plugin plugin : buildPlugins) {
            if ("maven-enforcer-plugin".equals(plugin.getArtifactId())) {
                return plugin;
            }
        }
        return null;
    }

    private List<PluginExecution> getPluginExecutionsWithEnforceGoal(List<PluginExecution> executions) {
        List<PluginExecution> pluginExecutions = new ArrayList<>();
        for (PluginExecution pluginExecution : executions) {
            List<String> goals = pluginExecution.getGoals();
            if (goals != null && goals.contains("enforce")) {
                pluginExecutions.add(pluginExecution);
            }
        }
        return pluginExecutions;
    }

    private Xpp3Dom getRequireMavenVersionTag(List<PluginExecution> executions) {
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

    private ArtifactVersion getHighestArtifactVersion(ArtifactVersion firstMavenVersion, ArtifactVersion secondMavenVersion) {
        if (null == firstMavenVersion && null == secondMavenVersion) {
            return null;
        }

        if (null == firstMavenVersion) {
            return secondMavenVersion;
        }

        if (null == secondMavenVersion) {
            return firstMavenVersion;
        }

        if (firstMavenVersion.compareTo(secondMavenVersion) < 0) {
            return secondMavenVersion;
        }

        return firstMavenVersion;
    }

    /**
     * The below method implements the specification found at https://maven.apache.org/enforcer/enforcer-rules/versionRanges.html
     */
    private ArtifactVersion processMavenVersionRange(String versionRange) {

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
            return new DefaultArtifactVersion(versionRange);
        }

        if (!((versionRange.charAt(0) == '[' || versionRange.charAt(0) == '(') && (versionRange.charAt(versionRange.length() - 1) == ']' || versionRange.charAt(versionRange.length() - 1) == ')'))) {
            return null;
        }

        if (openIndicesCount != 1) {
            return null;
        }

        String innerString = versionRange.substring(1, versionRange.length() - 1);

        int commaIndex = innerString.indexOf(',');

        if (commaIndex == -1) {
            if (versionRange.charAt(0) == '[' && versionRange.charAt(versionRange.length() - 1) == ']') {
                return new DefaultArtifactVersion(innerString);
            }
            else {
                return null;
            }
        }

        if (commaIndex == 0) {
            return null;
        }

        if (commaIndex == innerString.length() - 1) {
            String minimumVersion = innerString.substring(0, innerString.length() - 1);

            if (versionRange.charAt(0) == '[' && versionRange.charAt(versionRange.length() - 1) == ')') {
                return new DefaultArtifactVersion(minimumVersion);
            }

            if (versionRange.charAt(0) == '(' && versionRange.charAt(versionRange.length() - 1) == ')') {
                // this is actually wrong - the Maven version should be higher than this, the Maven version cannot be equal to this, but the Maven Enforcer plugin should capture this
                return new DefaultArtifactVersion(minimumVersion);
            }

            return null;
        }

        String minimumVersion = innerString.substring(0, commaIndex);

        if (versionRange.charAt(0) == '[') {
            return new DefaultArtifactVersion(minimumVersion);
        }

        if (versionRange.charAt(0) == '(') {
            // this is actually wrong - the Maven version should be higher than this, the Maven version cannot be equal to this, but the Maven Enforcer plugin should capture this
            return new DefaultArtifactVersion(minimumVersion);
        }

        return null;
    }
}
