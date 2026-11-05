package org.codehaus.mojo.versions.rule;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertSameElements;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class RuleServiceTest {
    @Mock
    private Log log;

    @Mock
    private MavenSession mavenSession;

    @BeforeEach
    public void beforeEach() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testIgnoredVersionsShouldExtendTheRuleSet() throws MojoExecutionException, IllegalAccessException {
        RuleService service = new RulesServiceBuilder()
                .withMavenSession(mavenSession)
                .withLog(log)
                .withRuleSet(new RuleSet() {
                    {
                        setIgnoreVersions(new ArrayList<>(singletonList(new IgnoreVersion() {
                            {
                                setVersion("1.0.0");
                            }
                        })));
                        setRules(singletonList(new Rule() {
                            {
                                setGroupId("org.slf4j");
                                setArtifactId("slf4j-api");
                                setIgnoreVersions(singletonList(new IgnoreVersion() {
                                    {
                                        setType("regex");
                                        setVersion("^[^1]\\.*");
                                    }
                                }));
                            }
                        }));
                    }
                })
                .withIgnoredVersions(Arrays.asList(".*-M.", ".*-SNAPSHOT"))
                .build();
        RuleSet ruleSet = service.getRuleSet();
        assertEquals(3, ruleSet.getIgnoreVersions().size(), () -> String.valueOf(ruleSet.getIgnoreVersions()));
        assertSameElements(
                Arrays.asList(".*-M.", ".*-SNAPSHOT", "1.0.0"),
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()));
    }

    @Test
    void testIgnoredVersionsShouldBeTheOnlyPresentInAnEmptyRuleSet() throws MojoExecutionException {
        RuleService service = new RulesServiceBuilder()
                .withMavenSession(mavenSession)
                .withLog(log)
                .withIgnoredVersions(Arrays.asList(".*-M.", ".*-SNAPSHOT"))
                .build();
        RuleSet ruleSet = service.getRuleSet();
        assertEquals(2, ruleSet.getIgnoreVersions().size(), () -> String.valueOf(ruleSet.getIgnoreVersions()));
        assertSameElements(
                Arrays.asList(".*-M.", ".*-SNAPSHOT"),
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()));
    }
}
