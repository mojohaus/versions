package org.codehaus.mojo.versions;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

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
        ArtifactVersion childMavenVersion = getHighestNonNullArtifactVersion(getPrerequisitesMavenVersion(), getEnforcerMavenVersion());

        if (!mavenProject.hasParent()) {
            return childMavenVersion;
        }

        ArtifactVersion parentMavenVersion = new RequiredMavenVersionFinder(mavenProject.getParent()).find();

        return getHighestNonNullArtifactVersion(childMavenVersion, parentMavenVersion);
    }

    private ArtifactVersion getHighestNonNullArtifactVersion(ArtifactVersion firstMavenVersion, ArtifactVersion secondMavenVersion) {
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
        Plugin mavenEnforcerPlugin = getMavenEnforcerPlugin(buildPlugins);
        if (null == mavenEnforcerPlugin) {
            return null;
        }

        List<PluginExecution> pluginExecutions = mavenEnforcerPlugin.getExecutions();
        PluginExecution pluginExecutionWithEnforceGoal = getPluginExecutionWithEnforceGoal(pluginExecutions);
        if (null == pluginExecutionWithEnforceGoal) {
            return null;
        }

        Xpp3Dom configurationTag = (Xpp3Dom) pluginExecutionWithEnforceGoal.getConfiguration();
        if (null == configurationTag) {
            return null;
        }

        Xpp3Dom rulesTag = configurationTag.getChild("rules");
        if (null == rulesTag) {
            return null;
        }

        Xpp3Dom requireMavenVersionTag = rulesTag.getChild("requireMavenVersion");
        if (null == requireMavenVersionTag) {
            return null;
        }

        Xpp3Dom versionTag = requireMavenVersionTag.getChild("version");
        if (null == versionTag) {
            return null;
        }

        String versionTagValue = versionTag.getValue();
        if (null == versionTagValue) {
            return null;
        }

        return new DefaultArtifactVersion(versionTagValue);
    }

    private Plugin getMavenEnforcerPlugin(List<Plugin> buildPlugins) {
        for (Plugin plugin : buildPlugins) {
            if ("maven-enforcer-plugin".equals(plugin.getArtifactId())) {
                return plugin;
            }
        }
        return null;
    }

    private PluginExecution getPluginExecutionWithEnforceGoal(List<PluginExecution> executions) {
        for (PluginExecution pluginExecution : executions) {
            List<String> goals = pluginExecution.getGoals();
            if (goals.contains("enforce")) {
                return pluginExecution;
            }
        }
        return null;
    }
}
