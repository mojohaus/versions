package org.codehaus.mojo.versions.api;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import javax.xml.stream.XMLStreamException;
import javax.xml.transform.TransformerException;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.DefaultModelWriter;
import org.apache.maven.model.io.ModelWriter;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.MavenProject;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ModelNode;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static java.nio.charset.Charset.defaultCharset;
import static org.apache.commons.io.IOUtils.toInputStream;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.containsStringIgnoringCase;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Tests the methods of {@link PomHelper}.
 */
@ExtendWith(MockitoExtension.class)
class PomHelperTest {
    private static final int NUMBER_OF_CHILD_PROJECTS = 30;

    private static final Path PATH = Paths.get("dummy-file");

    @Mock
    private Log log;

    /**
     * Tests what happens when changing a long property substitution pattern, e.g.
     * <a href="http://jira.codehaus.org/browse/MVERSIONS-44">MVERSIONS-44</a>
     *
     * @throws Exception if the test fails.
     */
    @Test
    void testLongProperties() throws Exception {
        URL url = getClass().getResource("PomHelperTest.testLongProperties.pom.xml");
        assert url != null;
        File file = new File(url.getPath());

        MutableXMLStreamReader pom = new MutableXMLStreamReader(file.toPath());

        String oldVersion = PomHelper.getProjectVersion(pom);

        String newVersion = "1";

        assertTrue(PomHelper.setProjectVersion(pom, newVersion), "The pom has been modified");

        assertEquals(newVersion, PomHelper.getProjectVersion(pom));

        assertNotSame(newVersion, oldVersion);
    }

    @Test
    void testSetPropertyVersionNoProfile() throws Exception {
        URL url = getClass().getResource("PomHelperTest.profiles.pom.xml");
        assert url != null;
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(Paths.get(url.toURI()))) {
            assertThat(PomHelper.setPropertyVersion(pom, null, "propertyA", "newValue"), is(true));
            String pomString = pom.getSource();
            assertThat(pomString, containsString("<propertyA>newValue</propertyA>"));
            assertThat(
                    pomString,
                    allOf(
                            containsString("<propertyB>propertyBValue</propertyB>"),
                            containsString("<profileAPropertyA>profileAPropertyAValue</profileAPropertyA>"),
                            containsString("<profileAPropertyB>profileAPropertyBValue</profileAPropertyB>"),
                            containsString("<profileBPropertyA>profileBPropertyAValue</profileBPropertyA>"),
                            containsString("<profileBPropertyB>profileBPropertyBValue</profileBPropertyB>")));
        }
    }

    @Test
    void testSetPropertyVersionProfile() throws Exception {
        URL url = getClass().getResource("PomHelperTest.profiles.pom.xml");
        assert url != null;
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(Paths.get(url.toURI()))) {
            assertThat(PomHelper.setPropertyVersion(pom, "profileA", "profileAPropertyA", "newValue"), is(true));
            String pomString = pom.getSource();
            assertThat(pomString, containsString("<profileAPropertyA>newValue</profileAPropertyA>"));
            assertThat(
                    pomString,
                    allOf(
                            containsString("<propertyA>propertyAValue</propertyA>"),
                            containsString("<propertyB>propertyBValue</propertyB>"),
                            containsString("<profileAPropertyB>profileAPropertyBValue</profileAPropertyB>"),
                            containsString("<profileBPropertyA>profileBPropertyAValue</profileBPropertyA>"),
                            containsString("<profileBPropertyB>profileBPropertyBValue</profileBPropertyB>")));
        }
    }

    @Test
    void testImplicitProperties() throws Exception {
        URL url = getClass().getResource("PomHelperTest.implicitProperties.pom.xml");
        assert url != null;
        File file = new File(url.getPath());
        String input = PomHelper.readXml(file).getLeft();
        Model model = new MavenXpp3Reader().read(new StringReader(input));
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(file.toPath())) {
            Map<String, String> implicitProperties = PomHelper.getImplicitProperties(pom, model);
            assertThat(implicitProperties, hasEntry(is("projectVersion"), is("doNotOverride")));
            assertThat(implicitProperties, hasEntry(is("propertyA"), is("propertyAValue")));
            assertThat(implicitProperties, hasEntry(is("project.parent.groupId"), is("parentGroupId")));
            assertThat(implicitProperties, hasEntry(is("project.parent.artifactId"), is("parentArtifactId")));
            assertThat(implicitProperties, hasEntry(is("project.parent.version"), is("parentVersion")));
            assertThat(implicitProperties, hasEntry(is("project.groupId"), is("parentGroupId")));
            assertThat(implicitProperties, hasEntry(is("project.artifactId"), is("parentArtifactId")));
            assertThat(implicitProperties, hasEntry(is("project.version"), is("parentVersion")));
        }
    }

    static Stream<Arguments> testGetProjectVersionArguments() {
        return Stream.of(
                Arguments.of(
                        "<project><modelVersion>4.0.0</modelVersion><parent/><version>projectVersion</version></project>",
                        "projectVersion"),
                Arguments.of("<project><modelVersion>4.0.0</modelVersion><parent/><version/></project>", ""));
    }

    @ParameterizedTest
    @MethodSource("testGetProjectVersionArguments")
    void testGetProjectVersion(String pom, String expectedValue)
            throws XMLStreamException, IOException, TransformerException {
        try (MutableXMLStreamReader reader = new MutableXMLStreamReader(toInputStream(pom, defaultCharset()), PATH)) {
            assertThat(PomHelper.getProjectVersion(reader), is(expectedValue));
        }
    }

    static Stream<Arguments> testSetProjectParentVersionArguments() {
        return Stream.of(
                Arguments.of(
                        "<project><modelVersion>4.0.0</modelVersion><parent><version>parentVersion</version></parent><version>projectVersion</version></project>",
                        true),
                Arguments.of("<project><modelVersion>4.0.0</modelVersion><parent/><version/></project>", false));
    }

    @ParameterizedTest
    @MethodSource("testSetProjectParentVersionArguments")
    void testSetProjectParentVersion(String pom, boolean expectedValue)
            throws XMLStreamException, IOException, TransformerException {
        try (MutableXMLStreamReader reader = new MutableXMLStreamReader(toInputStream(pom, defaultCharset()), PATH)) {
            assertThat(PomHelper.setProjectParentVersion(reader, "newVersion"), is(expectedValue));
        }
    }

    @Test
    void testSetDependencyVersion()
            throws XMLStreamException, IOException, TransformerException, XmlPullParserException {
        URL url = getClass().getResource("PomHelperTest.implicitProperties.pom.xml");
        assert url != null;
        File file = new File(url.getPath());
        String input = PomHelper.readXml(file).getLeft();
        Model model = new MavenXpp3Reader().read(new StringReader(input));
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(file.toPath())) {
            assertThat(
                    PomHelper.setDependencyVersion(
                            pom, "propertyAValue", "artifactA", "version", "newVersion", model, log),
                    is(true));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertThat(
                    newModel.getDependencies(),
                    hasItem(allOf(
                            hasProperty("groupId", is("${propertyA}")),
                            hasProperty("artifactId", is("artifactA")),
                            hasProperty("version", is("newVersion")))));
        }
    }

    @Test
    void testSetDependencyVersionDepMan()
            throws XMLStreamException, IOException, TransformerException, XmlPullParserException {
        URL url = getClass().getResource("PomHelperTest.implicitProperties.pom.xml");
        assert url != null;
        File file = new File(url.getPath());
        String input = PomHelper.readXml(file).getLeft();
        Model model = new MavenXpp3Reader().read(new StringReader(input));
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(file.toPath())) {
            assertThat(
                    PomHelper.setDependencyVersion(pom, "groupB", "artifactB", "versionB", "newVersion", model, log),
                    is(true));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertThat(
                    newModel.getDependencyManagement().getDependencies(),
                    hasItem(allOf(
                            hasProperty("groupId", is("groupB")),
                            hasProperty("artifactId", is("artifactB")),
                            hasProperty("version", is("newVersion")))));
        }
    }

    @Test
    void testSetDependencyVersionProfile()
            throws XMLStreamException, IOException, TransformerException, XmlPullParserException {
        URL url = getClass().getResource("PomHelperTest.implicitProperties.pom.xml");
        assert url != null;
        File file = new File(url.getPath());
        String input = PomHelper.readXml(file).getLeft();
        Model model = new MavenXpp3Reader().read(new StringReader(input));
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(file.toPath())) {
            assertThat(
                    PomHelper.setDependencyVersion(pom, "groupC", "artifactC", "versionC", "newVersion", model, log),
                    is(true));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertThat(
                    newModel.getProfiles().get(0).getDependencies(),
                    hasItem(allOf(
                            hasProperty("groupId", is("groupC")),
                            hasProperty("artifactId", is("artifactC")),
                            hasProperty("version", is("newVersion")))));
        }
    }

    @Test
    void testGroupIdNotOnChildPom() throws Exception {
        URL url = getClass().getResource("PomHelperTest.noGroupIdOnChild.pom.xml");
        assert url != null;
        String input = PomHelper.readXml(new File(url.getPath())).getLeft();
        MavenXpp3Reader reader = new MavenXpp3Reader();
        Model model = reader.read(new StringReader(input));

        assertEquals("org.myorg", PomHelper.getGroupId(model));
    }

    @Test
    void testVersionVersionEqual() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("1.0.8", "1.0.8"));
    }

    @Test
    void testVersionVersionDiffer() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("1.0.8", "1.0.0"));
    }

    @Test
    void testVersionRangeIntersect() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("1.0.8", "[1.0.3,1.1.0]"));
    }

    @Test
    void testVersionRangeDisjoint() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("1.0.8", "[0.0.1,1.0.0]"));
    }

    @Test
    void testVersionLeftOpenRangeDisjoint() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("1.0.8", "[,1.0.0]"));
    }

    @Test
    void testVersionRightOpenRangeDisjoint() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("1.0.8", "[1.1.0,)"));
    }

    @Test
    void testEmptyRange() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("1.0.8", ""));
    }

    @Test
    void testRangeEmpty() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("[1.0.5,1.0.8]", ""));
    }

    @Test
    void testRangeRangeIntersect() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("[1.0.5,1.0.8]", "[1.0.7,1.1.0]"));
    }

    @Test
    void testRangeRangeDisjoint() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("[1.0.5,1.0.6]", "[1.0.7,1.1.0]"));
    }

    @Test
    void testRangeVersionDisjoint() throws Exception {
        assertFalse(PomHelper.isVersionOverlap("[1.0.5,1.0.6]", "1.0.8"));
    }

    @Test
    void testRangeVersionIntersect() throws Exception {
        assertTrue(PomHelper.isVersionOverlap("[1.0.0,2.0.0]", "1.0.8"));
    }

    @Test
    void testSetElementValueExistingValue() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child>test</child></parent></super-parent>", defaultCharset()),
                PATH);

        assertThat(PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"), is(true));
        MatcherAssert.assertThat(
                xmlEventReader.getSource().replaceAll("\\s", ""),
                is("<super-parent><parent><child>value</child></parent></super-parent>"));
    }

    @Test
    void testSetElementValueEmptyChild() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child/></parent></super-parent>", defaultCharset()), PATH);

        assertThat(PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"), is(true));
        MatcherAssert.assertThat(
                xmlEventReader.getSource().replaceAll("\\s", ""),
                is("<super-parent><parent><child>value</child></parent></super-parent>"));
    }

    @Test
    void testSetElementValueNewValueEmptyParent() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent/></super-parent>", defaultCharset()), PATH);

        assertThat(PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"), is(true));
        MatcherAssert.assertThat(
                xmlEventReader.getSource().replaceAll("\\s", ""),
                is("<super-parent><parent><child>value</child></parent></super-parent>"));
    }

    @Test
    void testSetElementValueNewValueNoChild() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child2/></parent></super-parent>", defaultCharset()), PATH);

        assertThat(PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"), is(true));
        MatcherAssert.assertThat(
                xmlEventReader.getSource().replaceAll("\\s", ""),
                is("<super-parent><parent><child2/><child>value</child></parent></super-parent>"));
    }

    @Test
    void testSetProjectValueNewValueNonEmptyParent() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child>test</child></parent></super-parent>", defaultCharset()),
                PATH);

        assertThat(PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"), is(true));
        MatcherAssert.assertThat(
                xmlEventReader.getSource().replaceAll("\\s", ""),
                is("<super-parent><parent><child>value</child></parent></super-parent>"));
    }

    @Test
    void testIssue505ChildModules() throws Exception {
        MavenProject project = new MavenProject();
        project.setFile(new File("src/test/resources/org/codehaus/mojo/versions/api/issue-505/pom.xml"));
        assertThat(PomHelper.getChildModels(project, new SystemStreamLog()).entrySet(), hasSize(3));
    }

    @Test
    void testChildModelsForMultiLevelProject() throws Exception {
        Path tempDirectory = Files.createTempDirectory("testChildModelsForLargeNumberOfModules");
        ModelWriter modelWriter = new DefaultModelWriter();
        Map<Path, Model> createdModels = new LinkedHashMap<>();

        try {
            Model rootProject = createSimpleModel("root");
            createdModels.put(tempDirectory, rootProject);
            for (int levelOne = 0; levelOne < NUMBER_OF_CHILD_PROJECTS; levelOne++) {
                Model levelOneProject = createSimpleModel("child-" + levelOne);
                Path levelOneProjectDirectory = tempDirectory.resolve(levelOneProject.getArtifactId());
                rootProject.addModule(levelOneProject.getArtifactId());
                createdModels.put(levelOneProjectDirectory, levelOneProject);

                for (int levelTwo = 0; levelTwo < NUMBER_OF_CHILD_PROJECTS; levelTwo++) {
                    Model levelTwoProject = createSimpleModel("child-" + levelOne + "-" + levelTwo);
                    Path levelTwoProjectDirectory = levelOneProjectDirectory.resolve(levelTwoProject.getArtifactId());
                    levelOneProject.addModule(levelTwoProject.getArtifactId());
                    createdModels.put(levelTwoProjectDirectory, levelTwoProject);
                }
            }

            for (Map.Entry<Path, Model> entry : createdModels.entrySet()) {
                modelWriter.write(entry.getKey().resolve("pom.xml").toFile(), Collections.emptyMap(), entry.getValue());
            }

            MavenProject project = new MavenProject();
            project.setFile(tempDirectory.resolve("pom.xml").toFile());

            assertThat(
                    PomHelper.getChildModels(project, new SystemStreamLog()).entrySet(), hasSize(createdModels.size()));
        } finally {
            FileUtils.deleteDirectory(tempDirectory.toFile());
        }
    }

    private Model createSimpleModel(String artifactId) {
        Model module = new Model();
        module.setGroupId("child.test");
        module.setArtifactId(artifactId);
        module.setVersion("1.0.0-SNAPSHOT");
        module.setPackaging("pom");
        module.setModelVersion("4.0.0");
        return module;
    }

    @Test
    void testGetRawModelTree() throws Exception {
        Log log = mock(Log.class);
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/api/getRawModelTree/pom.xml");
        MutableXMLStreamReader pomReader = new MutableXMLStreamReader(path);
        List<ModelNode> rawModelTree = PomHelper.getRawModelTree(
                new ModelNode(PomHelper.getRawModel(pomReader.getSource(), path.toFile()), pomReader), log);
        assertThat(
                rawModelTree.stream()
                        .map(ModelNode::getModel)
                        .map(Model::getArtifactId)
                        .collect(Collectors.joining(" ")),
                is("grandparent childA grandchild childB"));
    }

    @Test
    void testFindProperty() throws Exception {
        Log log = mock(Log.class);
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/api/findProperty/pom.xml");
        MutableXMLStreamReader pomReader = new MutableXMLStreamReader(path);
        List<ModelNode> rawModelTree = PomHelper.getRawModelTree(
                new ModelNode(PomHelper.getRawModel(pomReader.getSource(), path.toFile()), pomReader), log);

        ModelNode grandparent = rawModelTree.get(0);
        assertThat(grandparent.getModel().getArtifactId(), is("grandparent"));
        ModelNode childA = rawModelTree.get(1);
        assertThat(childA.getModel().getArtifactId(), is("childA"));
        ModelNode grandchild = rawModelTree.get(2);
        assertThat(grandchild.getModel().getArtifactId(), is("grandchild"));
        ModelNode childB = rawModelTree.get(3);
        assertThat(childB.getModel().getArtifactId(), is("childB"));
        assertThat(PomHelper.findProperty("a", grandchild).get(), is(childA));
        assertThat(PomHelper.findProperty("a", childA).get(), is(childA));
        assertThat(PomHelper.findProperty("a", grandparent).get(), is(grandparent));
        assertThat(PomHelper.findProperty("a", childB).get(), is(grandparent));
        assertThat(PomHelper.findProperty("b", childB).get(), is(childB));
    }

    @Test
    // The reason for this is that XMLStreamReader overrides the preamble with its own detected encoding,
    // which is especially visible for files with a BOM
    void testReadXmlFileUtf16() throws IOException, XMLStreamException, TransformerException {
        String string = PomHelper.readXml(
                        new File("src/test/resources/org/codehaus/mojo/versions/api/PomHelperTest.utf16.xml"))
                .toString();
        assertThat(string, containsStringIgnoringCase("\"utf-16\""));
        assertThat(string, containsString("ąęśćńźĄŚĘŻĆ"));
    }

    @Test
    void testGetRawModelWithParents() throws IOException {
        MavenProject grandfather = new MavenProject();
        grandfather.getModel().setArtifactId("grandfather");

        MavenProject father = new MavenProject();
        father.setParent(grandfather);
        grandfather.getModel().setArtifactId("father");

        MavenProject child = new MavenProject();
        child.setParent(father);
        child.getModel().setArtifactId("child");

        MavenProject grandchild = new MavenProject();
        grandchild.setParent(child);
        child.getModel().setArtifactId("grandchild");

        Map<MavenProject, ?> map = PomHelper.getRawModelWithParents(grandchild);
        assertThat(map.keySet(), contains(grandchild, child, father, grandfather));
    }
}
