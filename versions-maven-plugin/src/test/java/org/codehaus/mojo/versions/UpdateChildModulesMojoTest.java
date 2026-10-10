package org.codehaus.mojo.versions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * A unit test suite for {@link UpdateChildModulesMojo}
 */
@ProjectMojoTest
public class UpdateChildModulesMojoTest {
    protected ProjectMojos mojos;

    private Path tempDir;

    @Mock
    protected Log log;

    @BeforeEach
    public void setUp(ProjectMojos mojos) throws Exception {
        this.mojos = mojos;
        openMocks(this);
        tempDir = TestUtils.createTempDir("set");
    }

    @AfterEach
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
        UpdateChildModulesMojo mojo = mojos.lookupInReactor("update-child-modules", tempDir, "mod1", "mod2");

        mojo.execute();

        String mod1 = String.join("", Files.readAllLines(tempDir.resolve("mod1/mod11/pom.xml")));
        String mod2 = String.join("", Files.readAllLines(tempDir.resolve("mod2/mod21/pom.xml")));
        assertTextContains(mod1, "<version>2.0</version>");
        assertTextContains(mod2, "<version>2.0</version>");
    }
}
