package org.codehaus.mojo.versions.model;

import java.io.Serializable;

public class IgnoreVersion implements Serializable {
	private String version;
	private String type = "exact";

	public String getType() {
		return this.type;
	}

	public String getVersion() {
		return this.version;
	}

	public void setType(String type) {
		this.type = type;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String toString() {
		StringBuilder buf = new StringBuilder(128);
		buf.append(this.version);
		buf.append(" (");
		buf.append(this.type);
		buf.append(")");
		return buf.toString();
	}
}