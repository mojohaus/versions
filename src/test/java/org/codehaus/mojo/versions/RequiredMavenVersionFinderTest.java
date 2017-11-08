package org.codehaus.mojo.versions;

import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.model.Prerequisites;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;

/**
 * Unit tests for RequiredMavenVersionFinder.
 */
@RunWith(MockitoJUnitRunner.class)
public class RequiredMavenVersionFinderTest {

    @Mock
    private MavenProject mavenProject;
    @Mock
    private Prerequisites prerequisites;
    @Mock
    private Plugin nonEnforcerPlugin;
    @Mock
    private Plugin enforcerPlugin;
    @Mock
    private PluginExecution pluginExecution;
    @Mock
    private Xpp3Dom configurationTag;
    @Mock
    private Xpp3Dom rulesTag;
    @Mock
    private Xpp3Dom requireMavenVersionTag;
    @Mock
    private Xpp3Dom versionTag;
    @Mock
    private MavenProject parentMavenProject;
    @Mock
    private Prerequisites parentPrerequisites;

    @Before
    public void setup() {
        when(mavenProject.getPrerequisites()).thenReturn(null);
        when(mavenProject.getBuildPlugins()).thenReturn(null);
        when(mavenProject.hasParent()).thenReturn(false);
        when(prerequisites.getMaven()).thenReturn(null);
        when(nonEnforcerPlugin.getArtifactId()).thenReturn(null);
        when(enforcerPlugin.getArtifactId()).thenReturn("maven-enforcer-plugin");
        when(enforcerPlugin.getExecutions()).thenReturn(null);
        when(pluginExecution.getGoals()).thenReturn(null);
        when(parentMavenProject.getPrerequisites()).thenReturn(null);
        when(parentMavenProject.getBuildPlugins()).thenReturn(null);
        when(parentMavenProject.hasParent()).thenReturn(false);
        when(parentPrerequisites.getMaven()).thenReturn(null);
    }

    @Test
    public void findReturnsNullWhenPrerequisitesAreNullAndBuildPluginListIsNull() {
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenPrerequisitesMavenVersionIsNull() {
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNotNullWhenPrerequisitesMavenVersionIsNotNull() {
        String mavenVersion = "1";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersion);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(mavenVersion);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenBuildPluginsListDoesNotContainEnforcerPlugin() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(nonEnforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenEnforcerExecutionsIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenExecutionsListIsEmpty() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenExecutionGoalsListIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenPopulatedExecutionsListDoNotContainEnforcerExecution() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenEnforceGoalConfigurationIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenRulesChildIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(configurationTag.getChild("rules")).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenRequiredMavenVersionChildIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionChildIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(requireMavenVersionTag.getChild("version")).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueIsNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(requireMavenVersionTag.getChild("version")).thenReturn(versionTag);
        when(versionTag.getValue()).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueIsNotNull() {
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        goals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(requireMavenVersionTag.getChild("version")).thenReturn(versionTag);
        String mavenVersion = "1";
        when(versionTag.getValue()).thenReturn(mavenVersion);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersion);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenChildWithoutVersionAndParentWithoutVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsChildVersionWhenChildWithVersionAndParentWithoutVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        String childMavenVersion = "1";
        DefaultArtifactVersion childArtifactVersion = new DefaultArtifactVersion(childMavenVersion);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        assertEquals(childArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsParentVersionWhenChildWithoutVersionAndParentWithVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        String parentMavenVersion = "1";
        DefaultArtifactVersion parentArtifactVersion = new DefaultArtifactVersion(parentMavenVersion);
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        assertEquals(parentArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsChildVersionWhenChildWithMatchingVersionAndParentWithMatchingVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        String childMavenVersion = "1";
        DefaultArtifactVersion childArtifactVersion = new DefaultArtifactVersion(childMavenVersion);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        String parentMavenVersion = "1";
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        assertEquals(childArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsChildVersionWhenChildWithHigherVersionAndParentWithLowerVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        String childMavenVersion = "2";
        DefaultArtifactVersion childArtifactVersion = new DefaultArtifactVersion(childMavenVersion);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        String parentMavenVersion = "1";
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        assertEquals(childArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsParentVersionWhenChildWithLowerVersionAndParentWithHigherVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        String childMavenVersion = "1";
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        String parentMavenVersion = "2";
        DefaultArtifactVersion parentArtifactVersion = new DefaultArtifactVersion(parentMavenVersion);
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        assertEquals(parentArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }
}
