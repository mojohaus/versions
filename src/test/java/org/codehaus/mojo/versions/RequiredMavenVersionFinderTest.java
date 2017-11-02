package org.codehaus.mojo.versions;

import org.apache.maven.project.MavenProject;
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

    @Test
    public void findReturnsNullWhenPerquisitesAndEnforcerPluginNotSet() {
        when(mavenProject.getPrerequisites()).thenReturn(null);
        when(mavenProject.getBuildPlugins()).thenReturn(null);
        assertNull(new RequiredMavenVersionFinder(mavenProject).find());
    }
}
