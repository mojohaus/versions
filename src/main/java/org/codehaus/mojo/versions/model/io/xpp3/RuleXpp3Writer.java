package org.codehaus.mojo.versions.model.io.xpp3;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.plexus.util.xml.pull.MXSerializer;
import org.codehaus.plexus.util.xml.pull.XmlSerializer;

public class RuleXpp3Writer {
	private static final String NAMESPACE = null;

	public void write(Writer writer, RuleSet ruleSet) throws IOException {
		XmlSerializer serializer = new MXSerializer();
		serializer.setProperty("http://xmlpull.org/v1/doc/properties.html#serializer-indentation", "  ");
		serializer.setProperty("http://xmlpull.org/v1/doc/properties.html#serializer-line-separator", "\n");
		serializer.setOutput(writer);
		serializer.startDocument(ruleSet.getModelEncoding(), null);
		writeRuleSet(ruleSet, "ruleset", serializer);
		serializer.endDocument();
	}

	public void write(OutputStream stream, RuleSet ruleSet) throws IOException {
		XmlSerializer serializer = new MXSerializer();
		serializer.setProperty("http://xmlpull.org/v1/doc/properties.html#serializer-indentation", "  ");
		serializer.setProperty("http://xmlpull.org/v1/doc/properties.html#serializer-line-separator", "\n");
		serializer.setOutput(stream, ruleSet.getModelEncoding());
		serializer.startDocument(ruleSet.getModelEncoding(), null);
		writeRuleSet(ruleSet, "ruleset", serializer);
		serializer.endDocument();
	}

	private void writeIgnoreVersion(IgnoreVersion ignoreVersion, String tagName, XmlSerializer serializer)
			throws IOException {
		serializer.startTag(NAMESPACE, tagName);
		if ((ignoreVersion.getType() != null) && (!ignoreVersion.getType().equals("exact"))) {
			serializer.attribute(NAMESPACE, "type", ignoreVersion.getType());
		}
		serializer.text(ignoreVersion.getVersion());
		serializer.endTag(NAMESPACE, tagName);
	}

	private void writeRule(Rule rule, String tagName, XmlSerializer serializer) throws IOException {
		serializer.startTag(NAMESPACE, tagName);
		if (rule.getGroupId() != null) {
			serializer.attribute(NAMESPACE, "groupId", rule.getGroupId());
		}
		if (rule.getArtifactId() != null) {
			serializer.attribute(NAMESPACE, "artifactId", rule.getArtifactId());
		}
		if (rule.getComparisonMethod() != null) {
			serializer.attribute(NAMESPACE, "comparisonMethod", rule.getComparisonMethod());
		}
		if ((rule.getIgnoreVersions() != null) && (rule.getIgnoreVersions().size() > 0)) {
			serializer.startTag(NAMESPACE, "ignoreVersions");
			for (Iterator iter = rule.getIgnoreVersions().iterator(); iter.hasNext();) {
				IgnoreVersion o = (IgnoreVersion) iter.next();
				writeIgnoreVersion(o, "ignoreVersion", serializer);
			}
			serializer.endTag(NAMESPACE, "ignoreVersions");
		}
		serializer.endTag(NAMESPACE, tagName);
	}

	private void writeRuleSet(RuleSet ruleSet, String tagName, XmlSerializer serializer) throws IOException {
		serializer.startTag(NAMESPACE, tagName);
		if (ruleSet.getComparisonMethod() != null) {
			serializer.attribute(NAMESPACE, "comparisonMethod", ruleSet.getComparisonMethod());
		}
		if ((ruleSet.getIgnoreVersions() != null) && (ruleSet.getIgnoreVersions().size() > 0)) {
			serializer.startTag(NAMESPACE, "ignoreVersions");
			for (Iterator iter = ruleSet.getIgnoreVersions().iterator(); iter.hasNext();) {
				IgnoreVersion o = (IgnoreVersion) iter.next();
				writeIgnoreVersion(o, "ignoreVersion", serializer);
			}
			serializer.endTag(NAMESPACE, "ignoreVersions");
		}
		if ((ruleSet.getRules() != null) && (ruleSet.getRules().size() > 0)) {
			serializer.startTag(NAMESPACE, "rules");
			for (Iterator iter = ruleSet.getRules().iterator(); iter.hasNext();) {
				Rule o = (Rule) iter.next();
				writeRule(o, "rule", serializer);
			}
			serializer.endTag(NAMESPACE, "rules");
		}
		serializer.endTag(NAMESPACE, tagName);
	}
}