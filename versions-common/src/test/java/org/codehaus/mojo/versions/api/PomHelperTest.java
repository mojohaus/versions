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
import java.util.ArrayList;
import java.util.Arrays;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static java.nio.charset.Charset.defaultCharset;
import static org.apache.commons.io.IOUtils.toInputStream;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
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
            assertEquals(true, PomHelper.setPropertyVersion(pom, null, "propertyA", "newValue"));
            String pomString = pom.getSource();
            assertTextContains(pomString, "<propertyA>newValue</propertyA>");
            assertTextContains(
                    pomString,
                    "<propertyB>propertyBValue</propertyB>",
                    "<profileAPropertyA>profileAPropertyAValue</profileAPropertyA>",
                    "<profileAPropertyB>profileAPropertyBValue</profileAPropertyB>",
                    "<profileBPropertyA>profileBPropertyAValue</profileBPropertyA>",
                    "<profileBPropertyB>profileBPropertyBValue</profileBPropertyB>");
        }
    }

    @Test
    void testSetPropertyVersionProfile() throws Exception {
        URL url = getClass().getResource("PomHelperTest.profiles.pom.xml");
        assert url != null;
        try (MutableXMLStreamReader pom = new MutableXMLStreamReader(Paths.get(url.toURI()))) {
            assertEquals(true, PomHelper.setPropertyVersion(pom, "profileA", "profileAPropertyA", "newValue"));
            String pomString = pom.getSource();
            assertTextContains(pomString, "<profileAPropertyA>newValue</profileAPropertyA>");
            assertTextContains(
                    pomString,
                    "<propertyA>propertyAValue</propertyA>",
                    "<propertyB>propertyBValue</propertyB>",
                    "<profileAPropertyB>profileAPropertyBValue</profileAPropertyB>",
                    "<profileBPropertyA>profileBPropertyAValue</profileBPropertyA>",
                    "<profileBPropertyB>profileBPropertyBValue</profileBPropertyB>");
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
            assertEquals("doNotOverride", implicitProperties.get("projectVersion"));
            assertEquals("propertyAValue", implicitProperties.get("propertyA"));
            assertEquals("parentGroupId", implicitProperties.get("project.parent.groupId"));
            assertEquals("parentArtifactId", implicitProperties.get("project.parent.artifactId"));
            assertEquals("parentVersion", implicitProperties.get("project.parent.version"));
            assertEquals("parentGroupId", implicitProperties.get("project.groupId"));
            assertEquals("parentArtifactId", implicitProperties.get("project.artifactId"));
            assertEquals("parentVersion", implicitProperties.get("project.version"));
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
            assertEquals(expectedValue, PomHelper.getProjectVersion(reader));
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
            assertEquals(expectedValue, PomHelper.setProjectParentVersion(reader, "newVersion"));
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
            assertEquals(
                    true,
                    PomHelper.setDependencyVersion(
                            pom, "propertyAValue", "artifactA", "version", "newVersion", model, log));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertTrue(
                    newModel.getDependencies().stream()
                            .anyMatch(d -> "${propertyA}".equals(d.getGroupId())
                                    && "artifactA".equals(d.getArtifactId())
                                    && "newVersion".equals(d.getVersion())),
                    () -> String.valueOf(newModel.getDependencies()));
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
            assertEquals(
                    true,
                    PomHelper.setDependencyVersion(pom, "groupB", "artifactB", "versionB", "newVersion", model, log));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertTrue(
                    newModel.getDependencyManagement().getDependencies().stream()
                            .anyMatch(d -> "groupB".equals(d.getGroupId())
                                    && "artifactB".equals(d.getArtifactId())
                                    && "newVersion".equals(d.getVersion())),
                    () -> String.valueOf(newModel.getDependencyManagement().getDependencies()));
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
            assertEquals(
                    true,
                    PomHelper.setDependencyVersion(pom, "groupC", "artifactC", "versionC", "newVersion", model, log));
            Model newModel = new MavenXpp3Reader().read(new StringReader(pom.getSource()));
            assertTrue(
                    newModel.getProfiles().get(0).getDependencies().stream()
                            .anyMatch(d -> "groupC".equals(d.getGroupId())
                                    && "artifactC".equals(d.getArtifactId())
                                    && "newVersion".equals(d.getVersion())),
                    () -> String.valueOf(newModel.getProfiles().get(0).getDependencies()));
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

        assertEquals(true, PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"));
        assertEquals(
                "<super-parent><parent><child>value</child></parent></super-parent>",
                xmlEventReader.getSource().replaceAll("\\s", ""));
    }

    @Test
    void testSetElementValueEmptyChild() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child/></parent></super-parent>", defaultCharset()), PATH);

        assertEquals(true, PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"));
        assertEquals(
                "<super-parent><parent><child>value</child></parent></super-parent>",
                xmlEventReader.getSource().replaceAll("\\s", ""));
    }

    @Test
    void testSetElementValueNewValueEmptyParent() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent/></super-parent>", defaultCharset()), PATH);

        assertEquals(true, PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"));
        assertEquals(
                "<super-parent><parent><child>value</child></parent></super-parent>",
                xmlEventReader.getSource().replaceAll("\\s", ""));
    }

    @Test
    void testSetElementValueNewValueNoChild() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child2/></parent></super-parent>", defaultCharset()), PATH);

        assertEquals(true, PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"));
        assertEquals(
                "<super-parent><parent><child2/><child>value</child></parent></super-parent>",
                xmlEventReader.getSource().replaceAll("\\s", ""));
    }

    @Test
    void testSetProjectValueNewValueNonEmptyParent() throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader xmlEventReader = new MutableXMLStreamReader(
                toInputStream("<super-parent><parent><child>test</child></parent></super-parent>", defaultCharset()),
                PATH);

        assertEquals(true, PomHelper.setElementValue(xmlEventReader, "/super-parent/parent", "child", "value"));
        assertEquals(
                "<super-parent><parent><child>value</child></parent></super-parent>",
                xmlEventReader.getSource().replaceAll("\\s", ""));
    }

    @Test
    void testIssue505ChildModules() throws Exception {
        MavenProject project = new MavenProject();
        project.setFile(new File("src/test/resources/org/codehaus/mojo/versions/api/issue-505/pom.xml"));
        assertEquals(
                3,
                PomHelper.getChildModels(project, new SystemStreamLog())
                        .entrySet()
                        .size());
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

            assertEquals(
                    createdModels.size(),
                    PomHelper.getChildModels(project, new SystemStreamLog())
                            .entrySet()
                            .size());
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
        assertEquals(
                "grandparent childA grandchild childB",
                rawModelTree.stream()
                        .map(ModelNode::getModel)
                        .map(Model::getArtifactId)
                        .collect(Collectors.joining(" ")));
    }

    @Test
    void testFindProperty() throws Exception {
        Log log = mock(Log.class);
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/api/findProperty/pom.xml");
        MutableXMLStreamReader pomReader = new MutableXMLStreamReader(path);
        List<ModelNode> rawModelTree = PomHelper.getRawModelTree(
                new ModelNode(PomHelper.getRawModel(pomReader.getSource(), path.toFile()), pomReader), log);

        ModelNode grandparent = rawModelTree.get(0);
        assertEquals("grandparent", grandparent.getModel().getArtifactId());
        ModelNode childA = rawModelTree.get(1);
        assertEquals("childA", childA.getModel().getArtifactId());
        ModelNode grandchild = rawModelTree.get(2);
        assertEquals("grandchild", grandchild.getModel().getArtifactId());
        ModelNode childB = rawModelTree.get(3);
        assertEquals("childB", childB.getModel().getArtifactId());
        assertEquals(childA, PomHelper.findProperty("a", grandchild).get());
        assertEquals(childA, PomHelper.findProperty("a", childA).get());
        assertEquals(grandparent, PomHelper.findProperty("a", grandparent).get());
        assertEquals(grandparent, PomHelper.findProperty("a", childB).get());
        assertEquals(childB, PomHelper.findProperty("b", childB).get());
    }

    @Test
    // The reason for this is that XMLStreamReader overrides the preamble with its own detected encoding,
    // which is especially visible for files with a BOM
    void testReadXmlFileUtf16() throws IOException, XMLStreamException, TransformerException {
        String string = PomHelper.readXml(
                        new File("src/test/resources/org/codehaus/mojo/versions/api/PomHelperTest.utf16.xml"))
                .toString();
        assertTrue(string.toLowerCase().contains("\"utf-16\"".toLowerCase()), () -> string);
        assertTextContains(string, "ąęśćńźĄŚĘŻĆ");
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
        assertEquals(Arrays.asList(grandchild, child, father, grandfather), new ArrayList<>(map.keySet()));
    }
}
