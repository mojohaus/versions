package org.codehaus.mojo.versions;

import org.apache.maven.model.Prerequisites;
import org.apache.maven.project.MavenProject;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

/**
 * Unit tests for RequiredMavenVersionFinder.
 */
@RunWith(MockitoJUnitRunner.class)
public class RequiredMavenVersionFinderTest {

    @Mock
    private MavenProject mavenProject;
    @Mock
    private MavenProject parentMavenProject;
    @Mock
    private Prerequisites prerequisites;

    @Before
    public void setup() {
        when(mavenProject.getPrerequisites()).thenReturn(null);
        when(mavenProject.getBuildPlugins()).thenReturn(null);
        when(mavenProject.hasParent()).thenReturn(false);
        when(mavenProject.getParent()).thenReturn(null);
        when(prerequisites.getMaven()).thenReturn(null);
    }

    @Test
    public void findReturnsNullWhenPrerequisitesAndEnforcerPluginNotSet() {
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }

    // todo: find when prerequisites is not null and prerequisites.getMaven() is null

    // todo: find when prerequisites is not null and prerequisites.getMaven() is not null

    // todo: find when buildPlugins is not null but does not contain enforcer plugin

    // todo: find when mavenEnforcerPlugin.getExecutions() is null

    // todo: find when mavenEnforcerPlugin.getExecutions() is empty list

    // todo: find when mavenEnforcerPlugin.getExecutions() is not empty but does not contain enforce goal

    // todo: find when pluginExecutionWithEnforceGoal.getConfiguration() is null

    // todo: find when configurationTag.getChild("rules") is null

    // todo: find when rulesTag.getChild("requireMavenVersion") is null

    // todo: find when requireMavenVersionTag.getChild("version") is null

    // todo: find when versionTag.getValue() is null

    // todo: find when child without version set and parent without set

    // todo: find when child without version set and parent with version set

    // todo: find when child with version set and parent without version set

    // todo: find when child and parent with equal version set

    // todo: find when child has higher version than parent

    // todo: find when child has lower version than parent
}
