package org.codehaus.mojo.versions;

import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import junit.framework.TestCase;

public class SeparatePatternsForIncludesAnExcludesTest extends TestCase {

	AbstractVersionsDependencyUpdaterMojo mojo;
	
	protected void setUp() throws Exception {
		mojo = new AbstractVersionsDependencyUpdaterMojo() {
			protected void update(ModifiedPomXMLEventReader pom) throws MojoExecutionException, MojoFailureException, XMLStreamException {
			}
		};
	}

	public void testSeparatePatternsWithNull() {
		List patterns = mojo.separatePatterns(null);
		assertEquals(0, patterns.size());
	}

	public void testSeparatePatternsWithSinglePattern() {
		List patterns = mojo.separatePatterns("group:artifact:type:version");
		assertEquals(1, patterns.size());
		assertEquals("group:artifact:type:version", patterns.get(0));
	}

	public void testSeparatePatternWithSingleRange() {
		List patterns = mojo.separatePatterns("group:artifact:type:[1.0.2,2.0.0]");
		assertEquals(1, patterns.size());
		assertEquals("group:artifact:type:[1.0.2,2.0.0]", patterns.get(0));

		patterns = mojo.separatePatterns("group:artifact:type:(1.0.2,2.0.0]");
		assertEquals(1, patterns.size());
		assertEquals("group:artifact:type:(1.0.2,2.0.0]", patterns.get(0));
	}

	public void testSeparatePatternWithSeveralPatternsAndRanges() {
		List patterns = mojo.separatePatterns("group:artifact:type:[1.0.2,2.0.0),group2:artifact:type:(1.0.2,2.0.0]");
		assertEquals(2, patterns.size());
		assertEquals("group:artifact:type:[1.0.2,2.0.0)", patterns.get(0));
		assertEquals("group2:artifact:type:(1.0.2,2.0.0]", patterns.get(1));
	}

	
	public void testSeparatePatternsWithTwoCommaSeparatedPatterns() {
		List patterns = mojo.separatePatterns("group:artifact:type:version,group:artifact:type:version2");
		assertEquals(2, patterns.size());
		assertEquals("group:artifact:type:version", patterns.get(0));
		assertEquals("group:artifact:type:version2", patterns.get(1));
	}

	public void testSeparatePatternsWithSeveralCommaSeparatedPatterns() {
		List patterns = mojo.separatePatterns("group:artifact:type:version,group:artifact:type:version2,group:artifact:type:version3,group:artifact:type:version4");
		assertEquals(4, patterns.size());
		assertEquals("group:artifact:type:version", patterns.get(0));
		assertEquals("group:artifact:type:version2", patterns.get(1));
		assertEquals("group:artifact:type:version3", patterns.get(2));
		assertEquals("group:artifact:type:version4", patterns.get(3));
	}
	
}
