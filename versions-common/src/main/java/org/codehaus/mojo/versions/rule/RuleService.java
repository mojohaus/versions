package org.codehaus.mojo.versions.rule;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.IgnoreVersionHelper;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.mojo.versions.utils.RegexUtils;

public class RuleService {
    Log log;

    private final Map<String, Rule> bestFitRuleCache = new ConcurrentHashMap<>();

    private final RuleSet ruleSet;

    public RuleService(Log log, RuleSet ruleSet) {
        this.log = log;
        this.ruleSet = ruleSet;
    }

    /**
     * Find the rule, if any, which best fits the artifact details given.
     *
     * @param groupId    Group id of the artifact
     * @param artifactId Artifact id of the artifact
     * @return Rule which best describes the given artifact
     */
    public Rule getBestFitRule(String groupId, String artifactId) {
        String groupArtifactId = groupId + ':' + artifactId;
        if (bestFitRuleCache.containsKey(groupArtifactId)) {
            return bestFitRuleCache.get(groupArtifactId);
        }

        Rule bestFit = null;
        final List<Rule> rules = getRuleSet().getRules();
        int bestGroupIdScore = Integer.MAX_VALUE;
        int bestArtifactIdScore = Integer.MAX_VALUE;
        boolean exactGroupId = false;
        boolean exactArtifactId = false;
        for (Rule rule : rules) {
            int groupIdScore = RegexUtils.getWildcardScore(rule.getGroupId());
            if (groupIdScore > bestGroupIdScore) {
                continue;
            }
            boolean exactMatch = exactMatch(rule.getGroupId(), groupId);
            boolean match = exactMatch || match(rule.getGroupId(), groupId);
            if (!match || (exactGroupId && !exactMatch)) {
                continue;
            }
            if (bestGroupIdScore > groupIdScore) {
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            bestGroupIdScore = groupIdScore;
            if (exactMatch && !exactGroupId) {
                exactGroupId = true;
                bestArtifactIdScore = Integer.MAX_VALUE;
                exactArtifactId = false;
            }
            int artifactIdScore = RegexUtils.getWildcardScore(rule.getArtifactId());
            if (artifactIdScore > bestArtifactIdScore) {
                continue;
            }
            exactMatch = exactMatch(rule.getArtifactId(), artifactId);
            match = exactMatch || match(rule.getArtifactId(), artifactId);
            if (!match || (exactArtifactId && !exactMatch)) {
                continue;
            }
            bestArtifactIdScore = artifactIdScore;
            if (exactMatch && !exactArtifactId) {
                exactArtifactId = true;
            }
            bestFit = rule;
        }

        if (bestFit != null) {
            bestFitRuleCache.put(groupArtifactId, bestFit);
        }
        return bestFit;
    }

    /**
     * Returns a list of versions which should not be considered when looking for updates.
     *
     * @param artifact The artifact
     * @return List of ignored version
     */
    public List<IgnoreVersion> getIgnoredVersions(Artifact artifact) {
        Rule bestFitRule = getBestFitRule(artifact.getGroupId(), artifact.getArtifactId());
        return Stream.concat(
                        ruleSet.getIgnoreVersions().stream(),
                        Optional.ofNullable(bestFitRule)
                                .map(Rule::getIgnoreVersions)
                                .map(Collection::stream)
                                .orElse(Stream.empty()))
                .filter(v -> {
                    if (!IgnoreVersionHelper.isValidType(v)) {
                        log.warn("The type attribute '" + v.getType() + "' for global ignoreVersion["
                                + v + "] is not valid. Please use one of '" + IgnoreVersionHelper.VALID_TYPES
                                + "'.");
                        return false;
                    }
                    return true;
                })
                .collect(Collectors.toList());
    }

    /**
     * @return rule set
     */
    public RuleSet getRuleSet() {
        return ruleSet;
    }

    static boolean exactMatch(String wildcardRule, String value) {
        Pattern p = Pattern.compile(RegexUtils.convertWildcardsToRegex(wildcardRule, true));
        return p.matcher(value).matches();
    }

    static boolean match(String wildcardRule, String value) {
        Pattern p = Pattern.compile(RegexUtils.convertWildcardsToRegex(wildcardRule, false));
        return p.matcher(value).matches();
    }
}
