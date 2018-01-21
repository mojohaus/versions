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
    private PluginExecution otherPluginExecution;
    @Mock
    private Xpp3Dom configurationTag;
    @Mock
    private Xpp3Dom otherConfigurationTag;
    @Mock
    private Xpp3Dom rulesTag;
    @Mock
    private Xpp3Dom otherRulesTag;
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
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        when(prerequisites.getMaven()).thenReturn(mavenVersion);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersion);
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

    private void findReturnsValueWhenVersionTagValueIsSet(String mavenVersionRange) {
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
        when(versionTag.getValue()).thenReturn(mavenVersionRange);
    }

    @Test
    public void findReturnsNullWhenVersionTagValueIsNull() {
        findReturnsValueWhenVersionTagValueIsSet(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueIsValidSimpleRange() {
        String mavenVersionRange = "1.0";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenSecondEnforcerExecutionIsValidAndFirstEnforcerExecutionHasNoConfigurationTag() {
        String mavenVersionRange = "1.0";
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(otherPluginExecution);
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        ArrayList<String> otherGoals = new ArrayList<>();
        goals.add("enforce");
        otherGoals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(otherPluginExecution.getGoals()).thenReturn(otherGoals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(otherPluginExecution.getConfiguration()).thenReturn(null);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(requireMavenVersionTag.getChild("version")).thenReturn(versionTag);
        when(versionTag.getValue()).thenReturn(mavenVersionRange);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenSecondEnforcerExecutionIsValidAndFirstEnforcerExecutionHasNoRulesTag() {
        String mavenVersionRange = "1.0";
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(otherPluginExecution);
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        ArrayList<String> otherGoals = new ArrayList<>();
        goals.add("enforce");
        otherGoals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(otherPluginExecution.getGoals()).thenReturn(otherGoals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(otherPluginExecution.getConfiguration()).thenReturn(otherConfigurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(otherConfigurationTag.getChild("rules")).thenReturn(null);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(requireMavenVersionTag.getChild("version")).thenReturn(versionTag);
        when(versionTag.getValue()).thenReturn(mavenVersionRange);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenSecondEnforcerExecutionIsValidAndFirstEnforcerExecutionHasNoRequireMavenVersionTag() {
        String mavenVersionRange = "1.0";
        ArrayList<Plugin> buildPlugins = new ArrayList<>();
        buildPlugins.add(enforcerPlugin);
        when(mavenProject.getBuildPlugins()).thenReturn(buildPlugins);
        ArrayList<PluginExecution> pluginExecutions = new ArrayList<>();
        pluginExecutions.add(otherPluginExecution);
        pluginExecutions.add(pluginExecution);
        ArrayList<String> goals = new ArrayList<>();
        ArrayList<String> otherGoals = new ArrayList<>();
        goals.add("enforce");
        otherGoals.add("enforce");
        when(pluginExecution.getGoals()).thenReturn(goals);
        when(otherPluginExecution.getGoals()).thenReturn(otherGoals);
        when(enforcerPlugin.getExecutions()).thenReturn(pluginExecutions);
        when(pluginExecution.getConfiguration()).thenReturn(configurationTag);
        when(otherPluginExecution.getConfiguration()).thenReturn(otherConfigurationTag);
        when(configurationTag.getChild("rules")).thenReturn(rulesTag);
        when(otherConfigurationTag.getChild("rules")).thenReturn(otherRulesTag);
        when(rulesTag.getChild("requireMavenVersion")).thenReturn(requireMavenVersionTag);
        when(otherRulesTag.getChild("requireMavenVersion")).thenReturn(null);
        when(requireMavenVersionTag.getChild("version")).thenReturn(versionTag);
        when(versionTag.getValue()).thenReturn(mavenVersionRange);
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsExact() {
        String mavenVersionRange = "[1.0]";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsInclusiveMax() {
        String mavenVersionRange = "(,1.0]";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsExclusiveMax() {
        String mavenVersionRange = "(,1.0)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsInclusiveMin() {
        String mavenVersionRange = "[1.0,)";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsExclusiveMin() {
        String mavenVersionRange = "(1.0,)";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsExclusiveMinExclusiveMax() {
        String mavenVersionRange = "(1.0,2.0)";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsInclusiveMinInclusiveMax() {
        String mavenVersionRange = "[1.0,2.0]";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsInclusiveMinExclusiveMax() {
        String mavenVersionRange = "[1.0,2.0)";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsValueWhenVersionTagValueSetsExclusiveMinInclusiveMax() {
        String mavenVersionRange = "(1.0,2.0]";
        String minimumVersion = "1.0";
        DefaultArtifactVersion artifactVersion = new DefaultArtifactVersion(minimumVersion);
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertEquals(artifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsInclusiveLowerMaxInclusiveHigherMin() {
        String mavenVersionRange = "(,1.0],[1.2,)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsExclusiveLowerMaxExclusiveHigherMin() {
        String mavenVersionRange = "(,1.1),(1.1,)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsInclusiveLowerMaxExclusiveHigherMin() {
        String mavenVersionRange = "(,1.1],(1.1,)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueSetsExclusiveLowerMaxInclusiveHigherMin() {
        String mavenVersionRange = "(,1.1),[1.1,)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueOpenCharsCountGreaterThanCloseCharCount() {
        String mavenVersionRange = "(1.0";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueOpenCharsCountLessThanCloseCharCount() {
        String mavenVersionRange = "1.0)";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueDoesNotStartWithAnOpenChar() {
        String mavenVersionRange = "1.0()";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueDoesNotEndWithAnCloseChar() {
        String mavenVersionRange = "()1.0";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsNullWhenVersionTagValueOpenAndCloseTagsNotSameWhenNoComma() {
        String mavenVersionRange = "(1.0]";
        findReturnsValueWhenVersionTagValueIsSet(mavenVersionRange);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
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
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        String childMavenVersion = "1";
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        DefaultArtifactVersion childArtifactVersion = new DefaultArtifactVersion(childMavenVersion);
        assertEquals(childArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsParentVersionWhenChildWithoutVersionAndParentWithVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        String parentMavenVersion = "1";
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        DefaultArtifactVersion parentArtifactVersion = new DefaultArtifactVersion(parentMavenVersion);
        assertEquals(parentArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsChildVersionWhenChildWithMatchingVersionAndParentWithMatchingVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        String childMavenVersion = "1";
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        String parentMavenVersion = "1";
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        DefaultArtifactVersion childArtifactVersion = new DefaultArtifactVersion(childMavenVersion);
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
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        String parentMavenVersion = "1";
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        assertEquals(childArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }

    @Test
    public void findReturnsParentVersionWhenChildWithLowerVersionAndParentWithHigherVersion() {
        when(mavenProject.hasParent()).thenReturn(true);
        when(mavenProject.getParent()).thenReturn(parentMavenProject);
        when(mavenProject.getPrerequisites()).thenReturn(prerequisites);
        String childMavenVersion = "1";
        when(prerequisites.getMaven()).thenReturn(childMavenVersion);
        when(parentMavenProject.getPrerequisites()).thenReturn(parentPrerequisites);
        String parentMavenVersion = "2";
        when(parentPrerequisites.getMaven()).thenReturn(parentMavenVersion);
        DefaultArtifactVersion parentArtifactVersion = new DefaultArtifactVersion(parentMavenVersion);
        assertEquals(parentArtifactVersion, new RequiredMavenVersionFinder(mavenProject).find());
    }
}
