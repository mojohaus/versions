package org.codehaus.mojo.versions.api;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.logging.Log;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.startsWith;

@ExtendWith(MockitoExtension.class)
public class PropertyVersionsBuilderTest {

    @Mock
    private Log log;

    @Mock
    private VersionsHelper versionsHelper;

    @Mock
    private Artifact artifact;

    private PropertyVersionsBuilder builder;

    @BeforeEach
    public void beforeEach() {
        builder = new PropertyVersionsBuilder(versionsHelper, null, "property", log).withAssociation(artifact, false);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testGetVersionRangeLowerBound(boolean inclusive) {
        builder.withLowerBound("3", true).withLowerBound("1", inclusive).withLowerBound("2", false);
        String versionRange = builder.getVersionRange();
        assertThat(versionRange, startsWith((inclusive ? '[' : '(') + "1,"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testGetVersionRangeUpperBound(boolean inclusive) {
        builder.withUpperBound("1", true).withUpperBound("3", inclusive).withUpperBound("2", false);
        String versionRange = builder.getVersionRange();
        assertThat(versionRange, endsWith(",3" + (inclusive ? ']' : ')')));
    }
}
