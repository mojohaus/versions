package org.codehaus.mojo.versions.rewriting;

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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.codehaus.stax2.XMLStreamReader2;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static javax.xml.stream.XMLStreamConstants.END_DOCUMENT;
import static javax.xml.stream.XMLStreamConstants.END_ELEMENT;
import static javax.xml.stream.XMLStreamConstants.START_DOCUMENT;
import static javax.xml.stream.XMLStreamConstants.START_ELEMENT;
import static org.codehaus.mojo.versions.utils.TextAssertions.assertTextContains;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for {@link MutableXMLStreamReaderTest}
 */
class MutableXMLStreamReaderTest {
    private static final Path PATH = Paths.get("path");

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testCreateReaderUsingFile(String value) throws XMLStreamException, IOException, TransformerException {
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/rewriting/" + value + ".xml");
        try (MutableXMLStreamReader reader = new MutableXMLStreamReader(path)) {
            assertEquals(path, reader.getFileName());
            assertTrue(
                    reader.getSource().toLowerCase().contains("encoding=\"" + value + "\"".toLowerCase()),
                    () -> reader.getSource());
            assertTextContains(reader.getSource(), "ąęóśłźćńĄŻĘĆ");
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testCreateReaderUsingInputStream(String value) throws XMLStreamException, IOException, TransformerException {
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/rewriting/" + value + ".xml");
        try (MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), path)) {
            assertEquals(path, reader.getFileName());
            assertTrue(
                    reader.getSource().toLowerCase().contains("encoding=\"" + value + "\"".toLowerCase()),
                    () -> reader.getSource());
            assertTextContains(reader.getSource(), "ąęóśłźćńĄŻĘĆ");
        }
    }

    private static boolean goToElement(XMLStreamReader2 reader, int eventType, String elementName)
            throws XMLStreamException {
        for (int event = reader.getEventType(); event != END_DOCUMENT && reader.hasNext(); event = reader.next()) {
            if (event == eventType && elementName.equals(reader.getLocalName())) {
                return true;
            }
        }
        return false;
    }

    private static boolean goToStartElement(XMLStreamReader2 reader, String elementName) throws XMLStreamException {
        return goToElement(reader, START_ELEMENT, elementName);
    }

    private static boolean goToEndElement(XMLStreamReader2 reader, String elementName) throws XMLStreamException {
        return goToElement(reader, END_ELEMENT, elementName);
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testReplace(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertEquals(START_DOCUMENT, reader.getEventType());
        assertEquals(true, reader.hasNext());

        // move inside the <modelVersion> element
        assertEquals(true, goToStartElement(reader, "modelVersion"));
        reader.next();
        assertEquals(true, reader.isCharacters());

        assertEquals("4.0.0", reader.getSource().substring((int) reader.getCurrentStartingCharOffset(), (int)
                reader.getCurrentEndingCharOffset()));

        reader.replace("5");
        assertEquals(
                "5",
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()));

        // now move inside the <groupId> element
        assertEquals(true, goToStartElement(reader, "groupId"));
        reader.next();
        assertEquals(true, reader.isCharacters());

        assertEquals(
                "localhost",
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testBetween(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertEquals(START_DOCUMENT, reader.getEventType());
        assertEquals(true, reader.hasNext());

        // move the cursor to <modelVersion>
        assertEquals(true, goToStartElement(reader, "modelVersion"));
        reader.mark("<modelVersion>");

        // move the cursor to </modelVersion>
        assertEquals(true, goToEndElement(reader, "modelVersion"));
        reader.mark("</modelVersion>");

        assertEquals("4.0.0", reader.getBetween("<modelVersion>", "</modelVersion>"));

        // to check if the replacement has not shifted offsets, move inside the <groupId> element
        assertEquals(true, goToStartElement(reader, "groupId"));
        reader.next();
        assertEquals(true, reader.isCharacters());

        assertEquals(
                "localhost",
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testReplaceBetween(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertEquals(START_DOCUMENT, reader.getEventType());
        assertEquals(true, reader.hasNext());

        // move the cursor to <version>
        assertEquals(true, goToStartElement(reader, "version"));
        reader.mark("<version>");

        // move the cursor to </version>
        assertEquals(true, goToEndElement(reader, "version"));
        reader.mark("</version>");
        assertEquals("1.0", reader.getBetween("<version>", "</version>"));

        // move the cursor to <properties>
        assertEquals(true, goToStartElement(reader, "properties"));
        reader.mark("<properties>");

        // move the cursor to <empty/>
        assertEquals(true, goToStartElement(reader, "empty"));
        reader.mark("<empty/>");

        // move the cursor to </properties>
        assertEquals(true, goToEndElement(reader, "properties"));
        reader.mark("</properties>");

        assertTextContains(
                reader.getBetween("<properties>", "</properties>"), "<api>2.0</api>", "<impl>1.0</impl>", "<empty/>");

        reader.replaceBetween("<version>", "</version>", "1.1.0-SNAPSHOT");
        assertTextContains(reader.getSource(), "<version>1.1.0-SNAPSHOT</version>");
        assertEquals("1.1.0-SNAPSHOT", reader.getBetween("<version>", "</version>"));

        // after the replacement, offsets between marks should have been adjusted
        assertTextContains(
                reader.getBetween("<properties>", "</properties>"), "<api>2.0</api>", "<impl>1.0</impl>", "<empty/>");

        reader.replaceBetween("<empty/>", "<empty/>", "1.0.0");

        // after the replacement, offsets between marks should have been adjusted
        assertTextContains(
                reader.getBetween("<properties>", "</properties>"),
                "<api>2.0</api>",
                "<impl>1.0</impl>",
                "<empty>1.0.0</empty>");

        // offsets outside marks should also be correct
        assertEquals(true, goToStartElement(reader, "groupId"));
        reader.next();
        assertEquals(true, reader.isCharacters());

        assertEquals(
                "localhost",
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testReplaceMark(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertEquals(START_DOCUMENT, reader.getEventType());
        assertEquals(true, reader.hasNext());

        // create a set of marks in and around <version>
        assertEquals(true, goToStartElement(reader, "version"));
        reader.mark("<version>");
        reader.next();
        assertEquals(true, reader.isCharacters());
        reader.mark("version");
        assertEquals(true, goToEndElement(reader, "version"));
        reader.mark("</version>");

        // create another set of marks in and around <api>
        assertEquals(true, goToStartElement(reader, "api"));
        reader.mark("<api>");
        reader.next();
        assertEquals(true, reader.isCharacters());
        reader.mark("api");
        assertEquals(true, goToEndElement(reader, "api"));
        reader.mark("</api>");

        assertEquals("1.0", reader.getBetween("<version>", "</version>"));
        assertEquals("2.0", reader.getBetween("<api>", "</api>"));

        reader.replaceMark("version", "1.0.1-SNAPSHOT");
        assertEquals("1.0.1-SNAPSHOT", reader.getBetween("<version>", "</version>"));
        assertEquals("2.0", reader.getBetween("<api>", "</api>"));

        reader.replaceMark("version", "2");
        assertEquals("2", reader.getBetween("<version>", "</version>"));
        assertEquals("2.0", reader.getBetween("<api>", "</api>"));
    }
}
