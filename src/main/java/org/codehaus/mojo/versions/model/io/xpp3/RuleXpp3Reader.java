package org.codehaus.mojo.versions.model.io.xpp3;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import org.codehaus.mojo.versions.model.IgnoreVersion;
import org.codehaus.mojo.versions.model.Rule;
import org.codehaus.mojo.versions.model.RuleSet;
import org.codehaus.plexus.util.ReaderFactory;
import org.codehaus.plexus.util.xml.pull.EntityReplacementMap;
import org.codehaus.plexus.util.xml.pull.MXParser;
import org.codehaus.plexus.util.xml.pull.XmlPullParser;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

public class RuleXpp3Reader {
	private boolean addDefaultEntities = true;

	private boolean checkFieldWithDuplicate(XmlPullParser parser, String tagName, String alias, Set parsed)
			throws XmlPullParserException {
		if ((!parser.getName().equals(tagName)) && (!parser.getName().equals(alias))) {
			return false;
		}
		if (!parsed.add(tagName)) {
			throw new XmlPullParserException("Duplicated tag: '" + tagName + "'", parser, null);
		}
		return true;
	}

	private void checkUnknownAttribute(XmlPullParser parser, String attribute, String tagName, boolean strict)
			throws XmlPullParserException, IOException {
		if (strict) {
			throw new XmlPullParserException("Unknown attribute '" + attribute + "' for tag '" + tagName + "'", parser,
					null);
		}
	}

	private void checkUnknownElement(XmlPullParser parser, boolean strict) throws XmlPullParserException, IOException {
		if (strict) {
			throw new XmlPullParserException("Unrecognised tag: '" + parser.getName() + "'", parser, null);
		}

		for (int unrecognizedTagCount = 1; unrecognizedTagCount > 0;) {
			int eventType = parser.next();
			if (eventType == 2) {
				unrecognizedTagCount++;
			} else if (eventType == 3) {
				unrecognizedTagCount--;
			}
		}
	}

	public boolean getAddDefaultEntities() {
		return this.addDefaultEntities;
	}

	private boolean getBooleanValue(String s, String attribute, XmlPullParser parser) throws XmlPullParserException {
		return getBooleanValue(s, attribute, parser, null);
	}

	private boolean getBooleanValue(String s, String attribute, XmlPullParser parser, String defaultValue)
			throws XmlPullParserException {
		if ((s != null) && (s.length() != 0)) {
			return Boolean.valueOf(s).booleanValue();
		}
		if (defaultValue != null) {
			return Boolean.valueOf(defaultValue).booleanValue();
		}
		return false;
	}

	private byte getByteValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Byte.valueOf(s).byteValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException("Unable to parse element '" + attribute + "', must be a byte",
							parser, nfe);
				}
			}
		}
		return 0;
	}

	private char getCharacterValue(String s, String attribute, XmlPullParser parser) throws XmlPullParserException {
		if (s != null) {
			return s.charAt(0);
		}
		return '\000';
	}

	private Date getDateValue(String s, String attribute, XmlPullParser parser) throws XmlPullParserException {
		return getDateValue(s, attribute, null, parser);
	}

	private Date getDateValue(String s, String attribute, String dateFormat, XmlPullParser parser)
			throws XmlPullParserException {
		if (s != null) {
			String effectiveDateFormat = dateFormat;
			if (dateFormat == null) {
				effectiveDateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSS";
			}
			if ("long".equals(effectiveDateFormat)) {
				try {
					return new Date(Long.parseLong(s));
				} catch (NumberFormatException e) {
					throw new XmlPullParserException(e.getMessage(), parser, e);
				}

			}

			try {
				DateFormat dateParser = new SimpleDateFormat(effectiveDateFormat, Locale.US);
				return dateParser.parse(s);
			} catch (ParseException e) {
				throw new XmlPullParserException(e.getMessage(), parser, e);
			}
		}

		return null;
	}

	private double getDoubleValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Double.valueOf(s).doubleValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException(
							"Unable to parse element '" + attribute + "', must be a floating point number", parser,
							nfe);
				}
			}
		}
		return 0.0D;
	}

	private float getFloatValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Float.valueOf(s).floatValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException(
							"Unable to parse element '" + attribute + "', must be a floating point number", parser,
							nfe);
				}
			}
		}
		return 0.0F;
	}

	private int getIntegerValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Integer.valueOf(s).intValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException("Unable to parse element '" + attribute + "', must be an integer",
							parser, nfe);
				}
			}
		}
		return 0;
	}

	private long getLongValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Long.valueOf(s).longValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException(
							"Unable to parse element '" + attribute + "', must be a long integer", parser, nfe);
				}
			}
		}
		return 0L;
	}

	private String getRequiredAttributeValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s == null) {
			if (strict) {
				throw new XmlPullParserException("Missing required value for attribute '" + attribute + "'", parser,
						null);
			}
		}
		return s;
	}

	private short getShortValue(String s, String attribute, XmlPullParser parser, boolean strict)
			throws XmlPullParserException {
		if (s != null) {
			try {
				return Short.valueOf(s).shortValue();
			} catch (NumberFormatException nfe) {
				if (strict) {
					throw new XmlPullParserException(
							"Unable to parse element '" + attribute + "', must be a short integer", parser, nfe);
				}
			}
		}
		return 0;
	}

	private String getTrimmedValue(String s) {
		if (s != null) {
			s = s.trim();
		}
		return s;
	}

	private int nextTag(XmlPullParser parser) throws IOException, XmlPullParserException {
		int eventType = parser.next();
		if (eventType == 4) {
			eventType = parser.next();
		}
		if ((eventType != 2) && (eventType != 3)) {
			throw new XmlPullParserException("expected START_TAG or END_TAG not " + XmlPullParser.TYPES[eventType],
					parser, null);
		}
		return eventType;
	}

	public RuleSet read(Reader reader, boolean strict) throws IOException, XmlPullParserException {
		XmlPullParser parser = this.addDefaultEntities ? new MXParser(EntityReplacementMap.defaultEntityReplacementMap)
				: new MXParser();

		parser.setInput(reader);

		return read(parser, strict);
	}

	public RuleSet read(Reader reader) throws IOException, XmlPullParserException {
		return read(reader, true);
	}

	public RuleSet read(InputStream in, boolean strict) throws IOException, XmlPullParserException {
		return read(ReaderFactory.newXmlReader(in), strict);
	}

	public RuleSet read(InputStream in) throws IOException, XmlPullParserException {
		return read(ReaderFactory.newXmlReader(in));
	}

	private IgnoreVersion parseIgnoreVersion(XmlPullParser parser, boolean strict)
			throws IOException, XmlPullParserException {
		String tagName = parser.getName();
		IgnoreVersion ignoreVersion = new IgnoreVersion();
		for (int i = parser.getAttributeCount() - 1; i >= 0; i--) {
			String name = parser.getAttributeName(i);
			String value = parser.getAttributeValue(i);

			if (name.indexOf(':') < 0) {
				if ("type".equals(name)) {
					ignoreVersion.setType(getTrimmedValue(value));
				} else {
					checkUnknownAttribute(parser, name, tagName, strict);
				}
			}
		}
		ignoreVersion.setVersion(getTrimmedValue(parser.nextText()));
		return ignoreVersion;
	}

	private Rule parseRule(XmlPullParser parser, boolean strict) throws IOException, XmlPullParserException {
		String tagName = parser.getName();
		Rule rule = new Rule();
		for (int i = parser.getAttributeCount() - 1; i >= 0; i--) {
			String name = parser.getAttributeName(i);
			String value = parser.getAttributeValue(i);

			if (name.indexOf(':') < 0) {
				if ("groupId".equals(name)) {
					rule.setGroupId(getTrimmedValue(value));
				} else if ("artifactId".equals(name)) {
					rule.setArtifactId(getTrimmedValue(value));
				} else if ("comparisonMethod".equals(name)) {
					rule.setComparisonMethod(getTrimmedValue(value));
				} else {
					checkUnknownAttribute(parser, name, tagName, strict);
				}
			}
		}
		Set parsed = new HashSet();
		while ((strict ? parser.nextTag() : nextTag(parser)) == 2) {
			if (checkFieldWithDuplicate(parser, "ignoreVersions", null, parsed)) {
				List ignoreVersions = new ArrayList();
				rule.setIgnoreVersions(ignoreVersions);
				while (parser.nextTag() == 2) {
					if ("ignoreVersion".equals(parser.getName())) {
						ignoreVersions.add(parseIgnoreVersion(parser, strict));
					} else {
						checkUnknownElement(parser, strict);
					}
				}
			} else {
				checkUnknownElement(parser, strict);
			}
		}
		return rule;
	}

	private RuleSet parseRuleSet(XmlPullParser parser, boolean strict) throws IOException, XmlPullParserException {
		String tagName = parser.getName();
		RuleSet ruleSet = new RuleSet();
		for (int i = parser.getAttributeCount() - 1; i >= 0; i--) {
			String name = parser.getAttributeName(i);
			String value = parser.getAttributeValue(i);

			if (name.indexOf(':') < 0) {
				if (!"xmlns".equals(name)) {
					if ("comparisonMethod".equals(name)) {
						ruleSet.setComparisonMethod(getTrimmedValue(value));
					} else {
						checkUnknownAttribute(parser, name, tagName, strict);
					}
				}
			}
		}
		Set parsed = new HashSet();
		while ((strict ? parser.nextTag() : nextTag(parser)) == 2) {
			if (checkFieldWithDuplicate(parser, "ignoreVersions", null, parsed)) {
				List ignoreVersions = new ArrayList();
				ruleSet.setIgnoreVersions(ignoreVersions);
				while (parser.nextTag() == 2) {
					if ("ignoreVersion".equals(parser.getName())) {
						ignoreVersions.add(parseIgnoreVersion(parser, strict));
					} else {
						checkUnknownElement(parser, strict);
					}
				}
			} else if (checkFieldWithDuplicate(parser, "rules", null, parsed)) {
				List rules = new ArrayList();
				ruleSet.setRules(rules);
				while (parser.nextTag() == 2) {
					if ("rule".equals(parser.getName())) {
						rules.add(parseRule(parser, strict));
					} else {
						checkUnknownElement(parser, strict);
					}
				}
			} else {
				checkUnknownElement(parser, strict);
			}
		}
		return ruleSet;
	}

	private RuleSet read(XmlPullParser parser, boolean strict) throws IOException, XmlPullParserException {
		int eventType = parser.getEventType();
		while (eventType != 1) {
			if (eventType == 2) {
				if ((strict) && (!"ruleset".equals(parser.getName()))) {
					throw new XmlPullParserException(
							"Expected root element 'ruleset' but found '" + parser.getName() + "'", parser, null);
				}
				RuleSet ruleSet = parseRuleSet(parser, strict);
				ruleSet.setModelEncoding(parser.getInputEncoding());
				return ruleSet;
			}
			eventType = parser.next();
		}
		throw new XmlPullParserException(
				"Expected root element 'ruleset' but found no element at all: invalid XML document", parser, null);
	}

	public void setAddDefaultEntities(boolean addDefaultEntities) {
		this.addDefaultEntities = addDefaultEntities;
	}
}