package org.codehaus.mojo.versions;

import java.util.List;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.plexus.component.configurator.expression.ExpressionEvaluator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import static org.codehaus.mojo.versions.utils.MockUtils.mockArtifactHandlerManager;
import static org.junit.Assert.assertEquals;
import static org.mockito.MockitoAnnotations.openMocks;

public class SeparatePatternsForIncludesAnExcludesTest {

    AbstractVersionsDependencyUpdaterMojo mojo;

    @Mock
    private Log log;

    private PomHelper pomHelper;

    private ArtifactFactory artifactFactory;

    @Mock
    private ExpressionEvaluator expressionEvaluator;

    @Before
    public void setUp() throws Exception {
        openMocks(this);
        ArtifactHandlerManager artifactHandlerManager = mockArtifactHandlerManager();
        artifactFactory = new ArtifactFactory(artifactHandlerManager);
        pomHelper = new PomHelper(artifactFactory, expressionEvaluator);

        mojo = new AbstractVersionsDependencyUpdaterMojo(artifactFactory, null, null, null) {
            @Override
            protected boolean getProcessDependencies() {
                return true;
            }

            @Override
            protected boolean getProcessDependencyManagement() {
                return true;
            }

            @Override
            public boolean getProcessParent() {
                return false;
            }

            @Override
            protected boolean getAllowSnapshots() {
                return false;
            }

            @Override
            protected void update(MutableXMLStreamReader pom) {}
        };
    }

    @Test
    public void testSeparatePatternsWithNull() {
        List<String> patterns = mojo.separatePatterns(null);
        assertEquals(0, patterns.size());
    }

    @Test
    public void testSeparatePatternsWithSinglePattern() {
        List<String> patterns = mojo.separatePatterns("group:artifact:type:version");
        assertEquals(1, patterns.size());
        assertEquals("group:artifact:type:version", patterns.get(0));
    }

    @Test
    public void testSeparatePatternWithSingleRange() {
        List<String> patterns = mojo.separatePatterns("group:artifact:type:[1.0.2,2.0.0]");
        assertEquals(1, patterns.size());
        assertEquals("group:artifact:type:[1.0.2,2.0.0]", patterns.get(0));

        patterns = mojo.separatePatterns("group:artifact:type:(1.0.2,2.0.0]");
        assertEquals(1, patterns.size());
        assertEquals("group:artifact:type:(1.0.2,2.0.0]", patterns.get(0));
    }

    @Test
    public void testSeparatePatternWithSeveralPatternsAndRanges() {
        List<String> patterns =
                mojo.separatePatterns("group:artifact:type:[1.0.2,2.0.0),group2:artifact:type:(1.0.2,2.0.0]");
        assertEquals(2, patterns.size());
        assertEquals("group:artifact:type:[1.0.2,2.0.0)", patterns.get(0));
        assertEquals("group2:artifact:type:(1.0.2,2.0.0]", patterns.get(1));
    }

    @Test
    public void testSeparatePatternsWithTwoCommaSeparatedPatterns() {
        List<String> patterns = mojo.separatePatterns("group:artifact:type:version,group:artifact:type:version2");
        assertEquals(2, patterns.size());
        assertEquals("group:artifact:type:version", patterns.get(0));
        assertEquals("group:artifact:type:version2", patterns.get(1));
    }

    @Test
    public void testSeparatePatternsWithSeveralCommaSeparatedPatterns() {
        List<String> patterns = mojo.separatePatterns("group:artifact:type:version,group:artifact:type:version2,"
                + "group:artifact:type:version3,group:artifact:type:version4");
        assertEquals(4, patterns.size());
        assertEquals("group:artifact:type:version", patterns.get(0));
        assertEquals("group:artifact:type:version2", patterns.get(1));
        assertEquals("group:artifact:type:version3", patterns.get(2));
        assertEquals("group:artifact:type:version4", patterns.get(3));
    }
}
