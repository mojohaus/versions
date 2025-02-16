package org.codehaus.mojo.versions.rule;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.wagon.ConnectionException;
import org.apache.maven.wagon.ResourceDoesNotExistException;
import org.apache.maven.wagon.TransferFailedException;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationException;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.authorization.AuthorizationException;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.singletonList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;

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
        assertThat(ruleSet.getIgnoreVersions(), hasSize(3));
        assertThat(
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()),
                containsInAnyOrder(".*-M.", ".*-SNAPSHOT", "1.0.0"));
    }

    @Test
    void testIgnoredVersionsShouldBeTheOnlyPresentInAnEmptyRuleSet() throws MojoExecutionException {
        RuleService service = new RulesServiceBuilder()
                .withMavenSession(mavenSession)
                .withLog(log)
                .withIgnoredVersions(Arrays.asList(".*-M.", ".*-SNAPSHOT"))
                .build();
        RuleSet ruleSet = service.getRuleSet();
        assertThat(ruleSet.getIgnoreVersions(), hasSize(2));
        assertThat(
                ruleSet.getIgnoreVersions().stream()
                        .map(IgnoreVersion::getVersion)
                        .collect(Collectors.toList()),
                containsInAnyOrder(".*-M.", ".*-SNAPSHOT"));
    }

    private static Wagon mockFileWagon(URI rulesUri)
            throws AuthenticationException, ConnectionException, AuthorizationException, TransferFailedException,
                    ResourceDoesNotExistException {
        Wagon wagon = mock(Wagon.class);
        doNothing()
                .when(wagon)
                .connect(
                        any(org.apache.maven.wagon.repository.Repository.class),
                        any(AuthenticationInfo.class),
                        any(ProxyInfo.class));
        doAnswer(i -> {
                    File tempFile = i.getArgument(1);
                    Files.copy(Paths.get(rulesUri), tempFile.toPath(), REPLACE_EXISTING);
                    return null;
                })
                .when(wagon)
                .get(anyString(), any(File.class));
        return wagon;
    }
}
