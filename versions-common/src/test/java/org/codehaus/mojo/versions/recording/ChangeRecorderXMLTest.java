package org.codehaus.mojo.versions.recording;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public final class ChangeRecorderXMLTest {
    private static void copyResource(final String name, final Path output) throws IOException {
        try (InputStream inputStream = ChangeRecorderXMLTest.class.getResourceAsStream(name)) {
            Files.copy(inputStream, output, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static Document parseXML(final Path path) throws ParserConfigurationException, IOException, SAXException {
        final DocumentBuilderFactory documentBuilders = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = documentBuilders.newDocumentBuilder();
        return documentBuilder.parse(path.toFile());
    }

    @Test
    public void testChanges() throws Exception {
        final Path path0 = Files.createTempFile("ChangeRecorderTest", ".xml");
        final Path path1 = Files.createTempDirectory("ChangeRecorderTest")
                .resolve("subDirectory")
                .resolve("ChangeRecorderTest.xml");

        copyResource("expectedFile.xml", path0);

        final ChangeRecorder recorder = new ChangeRecorderXML();
        recorder.recordChange(DefaultDependencyChangeRecord.builder()
                .withKind(DependencyChangeRecord.ChangeKind.DEPENDENCY)
                .withGroupId("org.codehaus")
                .withArtifactId("example0")
                .withOldVersion("0.0.1")
                .withNewVersion("0.0.2")
                .build());

        recorder.recordChange(DefaultDependencyChangeRecord.builder()
                .withKind(DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT)
                .withGroupId("org.codehaus")
                .withArtifactId("example1")
                .withOldVersion("1.0.0")
                .withNewVersion("2.0.0")
                .build());

        recorder.writeReport(path1);

        final Document document0 = parseXML(path0);
        final Document document1 = parseXML(path1);

        final NodeList elements0 = document0.getElementsByTagNameNS(ChangeRecorderXML.CHANGES_NAMESPACE, "updated");
        final NodeList elements1 = document1.getElementsByTagNameNS(ChangeRecorderXML.CHANGES_NAMESPACE, "updated");

        Assert.assertEquals("Correct number of updates", elements0.getLength(), elements1.getLength());

        for (int index = 0; index < elements0.getLength(); ++index) {
            final Element element0 = (Element) elements0.item(index);
            final Element element1 = (Element) elements1.item(index);

            Assert.assertEquals(
                    element0.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "artifactId"),
                    element1.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "artifactId"));
            Assert.assertEquals(
                    element0.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "groupId"),
                    element1.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "groupId"));
            Assert.assertEquals(
                    element0.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "oldVersion"),
                    element1.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "oldVersion"));
            Assert.assertEquals(
                    element0.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "newVersion"),
                    element1.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "newVersion"));

            // FIXME - looks like assertions not working
            Assert.assertEquals(
                    element0.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "kind"),
                    element1.getAttributeNS(ChangeRecorderXML.CHANGES_NAMESPACE, "kind"));
        }
    }

    @Test
    public void emptyResultShouldNotGenerateReports() throws Exception {
        Path path = Files.createTempDirectory("ChangeRecorderTest").resolve("ChangeRecorderTest.xml");

        ChangeRecorder recorder = new ChangeRecorderXML();
        recorder.writeReport(path);

        Assert.assertFalse("File should not be created", Files.isRegularFile(path));
    }
}
