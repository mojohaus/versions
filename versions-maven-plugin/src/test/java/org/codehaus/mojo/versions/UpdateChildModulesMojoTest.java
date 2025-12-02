package org.codehaus.mojo.versions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.plugin.testing.MojoRule;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * A unit test suite for {@link UpdateChildModulesMojo}
 */
public class UpdateChildModulesMojoTest extends AbstractMojoTestCase {
    @Rule
    public MojoRule mojoRule = new MojoRule(this);

    private Path tempDir;

    @Mock
    protected Log log;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
    }

    @After
    public void tearDown() throws IOException {
        TestUtils.tearDownTempDir(tempDir);
    }

    /**
     * Tests against a case where {@link UpdateChildModulesMojo} is executed on a module project list
     * (that is, if a -pl parameter is used providing a list of modules to process).
     *
     * @throws Exception thrown if something goes not according to plan
     */
    @Test
    public void testModuleList() throws Exception {
        TestUtils.copyDir(Paths.get("src/test/resources/org/codehaus/mojo/update-child-modules/module-list"), tempDir);
        MavenSession session = TestUtils.createMavenSession(
                mojoRule.getContainer(), mojoRule::readMavenProject, tempDir, "mod1", "mod2");
        UpdateChildModulesMojo mojo = mojoRule.lookupConfiguredMojo(session, newMojoExecution("update-child-modules"));

        mojo.execute();

        String mod1 = String.join("", Files.readAllLines(tempDir.resolve("mod1/mod11/pom.xml")));
        String mod2 = String.join("", Files.readAllLines(tempDir.resolve("mod2/mod21/pom.xml")));
        assertThat(mod1, containsString("<version>2.0</version>"));
        assertThat(mod2, containsString("<version>2.0</version>"));
    }
}
