package org.codehaus.mojo.versions.rule;

import javax.xml.stream.XMLStreamException;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.wagon.Wagon;
import org.apache.maven.wagon.authentication.AuthenticationInfo;
import org.apache.maven.wagon.observers.Debug;
import org.apache.maven.wagon.proxy.ProxyInfo;
import org.codehaus.mojo.versions.api.DefaultVersionsHelper;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.model.io.stax.RuleStaxReader;
import org.eclipse.aether.repository.AuthenticationContext;
import org.eclipse.aether.repository.RemoteRepository;

import static java.util.Collections.singletonList;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.isBlank;

public class RulesServiceBuilder {
    private static final String CLASSPATH_PROTOCOL = "classpath";

    private Collection<String> ignoredVersions;
    private RuleSet ruleSet;
    private String serverId;
    private String rulesUri;
    private Log log;
    private MavenSession mavenSession;

    private Map<String, Wagon> wagonMap;

    public RulesServiceBuilder withIgnoredVersions(Collection<String> ignoredVersions) {
        this.ignoredVersions = ignoredVersions;
        return this;
    }

    public RulesServiceBuilder withRuleSet(RuleSet ruleSet) {
        this.ruleSet = ruleSet;
        return this;
    }

    public RulesServiceBuilder withServerId(String serverId) {
        this.serverId = serverId;
        return this;
    }

    public RulesServiceBuilder withRulesUri(String rulesUri) {
        this.rulesUri = rulesUri;
        return this;
    }

    public RulesServiceBuilder withLog(Log log) {
        this.log = log;
        return this;
    }

    public RulesServiceBuilder withMavenSession(MavenSession mavenSession) {
        this.mavenSession = mavenSession;
        return this;
    }

    public RulesServiceBuilder withWagonMap(Map<String, Wagon> wagonMap) {
        this.wagonMap = wagonMap;
        return this;
    }

    public RuleService build() throws MojoExecutionException {
        assert mavenSession != null;
        assert log != null;

        RuleSet ruleSet;
        if (this.ruleSet != null) {
            if (!isBlank(rulesUri)) {
                log.warn("rulesUri is ignored if rules are specified in pom or as parameters");
            }
            ruleSet = this.ruleSet;
        } else {
            ruleSet = isBlank(rulesUri)
                    ? new RuleSet()
                    : rulesUri.startsWith(CLASSPATH_PROTOCOL + ":")
                            ? getRulesFromClasspath(rulesUri, log)
                            : getRulesUsingWagon(rulesUri);
        }
        if (ignoredVersions != null && !ignoredVersions.isEmpty()) {
            ruleSet = enrichRuleSet(ignoredVersions, ruleSet);
        }
        return new RuleService(log, ruleSet);
    }

    private static class RulesUri {
        String basePath;
        String resource;

        private RulesUri(String basePath, String resource) {
            this.basePath = basePath;
            this.resource = resource;
        }

        static RulesUri build(String rulesUri) throws URISyntaxException {
            int split = rulesUri.lastIndexOf('/');
            return split == -1
                    ? new RulesUri(rulesUri, "")
                    : new RulesUri(
                            rulesUri.substring(0, split) + '/',
                            split + 1 < rulesUri.length() ? rulesUri.substring(split + 1) : "");
        }
    }

    private static RuleSet getRulesFromClasspath(String uri, Log logger) throws MojoExecutionException {
        logger.debug("Going to load rules from \"" + uri + "\"");
        String choppedUrl = uri.substring(CLASSPATH_PROTOCOL.length() + 3);
        URL url = DefaultVersionsHelper.class.getResource(choppedUrl);
        if (url == null) {
            throw new MojoExecutionException("Resource \"" + uri + "\" not found in classpath.");
        }

        try (BufferedInputStream bis = new BufferedInputStream(url.openStream())) {
            RuleSet result = new RuleStaxReader().read(bis);
            logger.debug("Loaded rules from \"" + uri + "\" successfully");
            return result;
        } catch (IOException | XMLStreamException e) {
            throw new MojoExecutionException("Could not load specified rules from " + uri, e);
        }
    }

    private RuleSet getRulesUsingWagon(String rulesUri) throws MojoExecutionException {
        RulesUri uri;
        try {
            uri = RulesUri.build(rulesUri);
        } catch (URISyntaxException e) {
            log.warn("Invalid rulesUri protocol: " + e.getMessage());
            return null;
        }

        RemoteRepository repository = remoteRepository(uri);
        return ofNullable(wagonMap.get(repository.getProtocol()))
                .map(wagon -> {
                    if (log.isDebugEnabled()) {
                        Debug debug = new Debug();
                        wagon.addSessionListener(debug);
                        wagon.addTransferListener(debug);
                    }

                    try {
                        Optional<ProxyInfo> proxyInfo = getProxyInfo(repository);
                        Optional<AuthenticationInfo> authenticationInfo = getAuthenticationInfo(repository);
                        if (log.isDebugEnabled()) {
                            log.debug("Connecting to remote repository \"" + repository.getId() + "\""
                                    + proxyInfo
                                            .map(pi -> " using proxy " + pi.getHost() + ":" + pi.getPort())
                                            .orElse("")
                                    + authenticationInfo
                                            .map(ai -> " as " + ai.getUserName())
                                            .orElse(""));
                        }
                        wagon.connect(
                                wagonRepository(repository),
                                getAuthenticationInfo(repository).orElse(null),
                                getProxyInfo(repository).orElse(null));
                        try {
                            Path tempFile = Files.createTempFile("rules-", ".xml");
                            wagon.get(uri.resource, tempFile.toFile());
                            try (InputStream is = Files.newInputStream(tempFile)) {
                                return new RuleStaxReader().read(is);
                            } finally {
                                Files.deleteIfExists(tempFile);
                            }

                        } finally {
                            wagon.disconnect();
                        }
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                })
                .orElseThrow(() -> new MojoExecutionException("Could not load specified rules from " + rulesUri));
    }

    private Optional<AuthenticationInfo> getAuthenticationInfo(RemoteRepository repository) {
        return ofNullable(repository.getAuthentication()).map(authentication -> new AuthenticationInfo() {
            {
                try (AuthenticationContext authCtx =
                        AuthenticationContext.forRepository(mavenSession.getRepositorySession(), repository)) {
                    ofNullable(authCtx.get(AuthenticationContext.USERNAME)).ifPresent(this::setUserName);
                    ofNullable(authCtx.get(AuthenticationContext.PASSWORD)).ifPresent(this::setPassword);
                    ofNullable(authCtx.get(AuthenticationContext.PRIVATE_KEY_PASSPHRASE))
                            .ifPresent(this::setPassphrase);
                    ofNullable(authCtx.get(AuthenticationContext.PRIVATE_KEY_PATH))
                            .ifPresent(this::setPrivateKey);
                }
            }
        });
    }

    private org.apache.maven.wagon.repository.Repository wagonRepository(RemoteRepository repository) {
        return new org.apache.maven.wagon.repository.Repository(repository.getId(), repository.getUrl());
    }

    private Optional<ProxyInfo> getProxyInfo(RemoteRepository repository) {
        return ofNullable(repository.getProxy()).map(proxy -> new ProxyInfo() {
            {
                setHost(proxy.getHost());
                setPort(proxy.getPort());
                setType(proxy.getType());
                ofNullable(proxy.getAuthentication()).ifPresent(auth -> {
                    try (AuthenticationContext authCtx =
                            AuthenticationContext.forProxy(mavenSession.getRepositorySession(), repository)) {
                        ofNullable(authCtx.get(AuthenticationContext.USERNAME)).ifPresent(this::setUserName);
                        ofNullable(authCtx.get(AuthenticationContext.PASSWORD)).ifPresent(this::setPassword);
                        ofNullable(authCtx.get(AuthenticationContext.NTLM_DOMAIN))
                                .ifPresent(this::setNtlmDomain);
                        ofNullable(authCtx.get(AuthenticationContext.NTLM_WORKSTATION))
                                .ifPresent(this::setNtlmHost);
                    }
                });
            }
        });
    }

    private RemoteRepository remoteRepository(RulesUri uri) {
        RemoteRepository prototype = new RemoteRepository.Builder(serverId, null, uri.basePath).build();
        RemoteRepository.Builder builder = new RemoteRepository.Builder(prototype);
        ofNullable(mavenSession.getRepositorySession().getProxySelector().getProxy(prototype))
                .ifPresent(builder::setProxy);
        ofNullable(mavenSession
                        .getRepositorySession()
                        .getAuthenticationSelector()
                        .getAuthentication(prototype))
                .ifPresent(builder::setAuthentication);
        ofNullable(mavenSession.getRepositorySession().getMirrorSelector().getMirror(prototype))
                .ifPresent(mirror -> builder.setMirroredRepositories(singletonList(mirror)));
        return builder.build();
    }

    /**
     * <p>Creates the enriched version of the ruleSet given as parameter; the ruleSet will contain the
     * set of ignored versions passed on top of its own (if defined).</p>
     *
     * <p>If the {@code originalRuleSet} is {@code null}, a new {@linkplain RuleSet} will be created as
     * a result.</p>
     *
     * <p><em>The method does not change the {@code originalRuleSet} object.</em></p>
     *
     * @param ignoredVersions collection of ignored version to enrich the clone of the original rule set
     * @param originalRuleSet original rule set
     * @return new RuleSet object containing the (if passed) cloned version of the rule set, enriched with
     *         the given set of ignored versions
     */
    @SuppressWarnings("checkstyle:AvoidNestedBlocks")
    private static RuleSet enrichRuleSet(Collection<String> ignoredVersions, RuleSet originalRuleSet) {
        RuleSet ruleSet = new RuleSet();
        if (originalRuleSet != null) {
            if (originalRuleSet.getRules() != null) {
                ruleSet.setRules(new ArrayList<>(originalRuleSet.getRules()));
            }
            if (originalRuleSet.getIgnoreVersions() != null) {
                ruleSet.setIgnoreVersions(new ArrayList<>(originalRuleSet.getIgnoreVersions()));
            }
        }

        if (ruleSet.getIgnoreVersions() == null) {
            ruleSet.setIgnoreVersions(new ArrayList<>());
        }
        ruleSet.getIgnoreVersions()
                .addAll(ignoredVersions.stream()
                        .map(v -> {
                            IgnoreVersion ignoreVersion = new IgnoreVersion();
                            ignoreVersion.setType(IgnoreVersion.TYPE_REGEX);
                            ignoreVersion.setVersion(v);
                            return ignoreVersion;
                        })
                        .collect(Collectors.toList()));

        return ruleSet;
    }
}
