package org.codehaus.mojo.versions.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Rule implements Serializable {
	private List<IgnoreVersion> ignoreVersions;
	private String groupId;
	private String artifactId;
	private String comparisonMethod;

	public void addIgnoreVersion(IgnoreVersion ignoreVersion) {
		getIgnoreVersions().add(ignoreVersion);
	}

	public String getArtifactId() {
		return this.artifactId;
	}

	public String getComparisonMethod() {
		return this.comparisonMethod;
	}

	public String getGroupId() {
		return this.groupId;
	}

	public List<IgnoreVersion> getIgnoreVersions() {
		if (this.ignoreVersions == null) {
			this.ignoreVersions = new ArrayList();
		}

		return this.ignoreVersions;
	}

	public void removeIgnoreVersion(IgnoreVersion ignoreVersion) {
		getIgnoreVersions().remove(ignoreVersion);
	}

	public void setArtifactId(String artifactId) {
		this.artifactId = artifactId;
	}

	public void setComparisonMethod(String comparisonMethod) {
		this.comparisonMethod = comparisonMethod;
	}

	public void setGroupId(String groupId) {
		this.groupId = groupId;
	}

	public void setIgnoreVersions(List<IgnoreVersion> ignoreVersions) {
		this.ignoreVersions = ignoreVersions;
	}

	public Rule() {
		this.artifactId = "*";
		this.comparisonMethod = "maven";
	}

	public String toString() {
		StringBuilder buf = new StringBuilder(128);
		buf.append("Rule[groupId = \"");
		buf.append(this.groupId);
		buf.append("\", artifactId = \"");
		buf.append(this.artifactId);
		buf.append("\", comparisonMethod = \"");
		buf.append(this.comparisonMethod);
		buf.append("\", ignoreVersions = \"");
		buf.append(this.ignoreVersions);
		buf.append("\"]");
		return buf.toString();
	}
}