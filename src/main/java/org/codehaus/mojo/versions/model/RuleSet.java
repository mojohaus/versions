package org.codehaus.mojo.versions.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class RuleSet implements Serializable {
	
	private List<IgnoreVersion> ignoreVersions;
	private List<Rule> rules;
	private String comparisonMethod;
	private String modelEncoding = "UTF-8";

	public void addIgnoreVersion(IgnoreVersion ignoreVersion) {
		getIgnoreVersions().add(ignoreVersion);
	}

	public void addRule(Rule rule) {
		getRules().add(rule);
	}

	public String getComparisonMethod() {
		return this.comparisonMethod;
	}

	public List<IgnoreVersion> getIgnoreVersions() {
		if (this.ignoreVersions == null) {
			this.ignoreVersions = new ArrayList();
		}

		return this.ignoreVersions;
	}

	public String getModelEncoding() {
		return this.modelEncoding;
	}

	public List<Rule> getRules() {
		if (this.rules == null) {
			this.rules = new ArrayList();
		}

		return this.rules;
	}

	public void removeIgnoreVersion(IgnoreVersion ignoreVersion) {
		getIgnoreVersions().remove(ignoreVersion);
	}

	public void removeRule(Rule rule) {
		getRules().remove(rule);
	}

	public void setComparisonMethod(String comparisonMethod) {
		this.comparisonMethod = comparisonMethod;
	}

	public void setIgnoreVersions(List<IgnoreVersion> ignoreVersions) {
		this.ignoreVersions = ignoreVersions;
	}

	public void setModelEncoding(String modelEncoding) {
		this.modelEncoding = modelEncoding;
	}

	public void setRules(List<Rule> rules) {
		this.rules = rules;
	}

	public RuleSet() {
		this.comparisonMethod = "maven";
	}

	public String toString() {
		StringBuilder buf = new StringBuilder(128);
		buf.append("RuleSet[rules = ");
		buf.append(this.rules);
		buf.append(", comparisonMethod = \"");
		buf.append(this.comparisonMethod);
		buf.append("\"]");
		return buf.toString();
	}
}