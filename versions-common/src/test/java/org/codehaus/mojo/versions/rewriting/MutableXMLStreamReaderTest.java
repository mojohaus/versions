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
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.containsStringIgnoringCase;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.Is.is;

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
            assertThat(reader.getFileName(), is(path));
            assertThat(reader.getSource(), containsStringIgnoringCase("encoding=\"" + value + "\""));
            assertThat(reader.getSource(), containsString("ąęóśłźćńĄŻĘĆ"));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testCreateReaderUsingInputStream(String value) throws XMLStreamException, IOException, TransformerException {
        Path path = Paths.get("src/test/resources/org/codehaus/mojo/versions/rewriting/" + value + ".xml");
        try (MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), path)) {
            assertThat(reader.getFileName(), is(path));
            assertThat(reader.getSource(), containsStringIgnoringCase("encoding=\"" + value + "\""));
            assertThat(reader.getSource(), containsString("ąęóśłźćńĄŻĘĆ"));
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

        assertThat(reader.getEventType(), is(START_DOCUMENT));
        assertThat(reader.hasNext(), is(true));

        // move inside the <modelVersion> element
        assertThat(goToStartElement(reader, "modelVersion"), is(true));
        reader.next();
        assertThat(reader.isCharacters(), is(true));

        assertThat(
                reader.getSource().substring((int) reader.getCurrentStartingCharOffset(), (int)
                        reader.getCurrentEndingCharOffset()),
                equalTo("4.0.0"));

        reader.replace("5");
        assertThat(
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()),
                equalTo("5"));

        // now move inside the <groupId> element
        assertThat(goToStartElement(reader, "groupId"), is(true));
        reader.next();
        assertThat(reader.isCharacters(), is(true));

        assertThat(
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()),
                equalTo("localhost"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testBetween(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertThat(reader.getEventType(), is(START_DOCUMENT));
        assertThat(reader.hasNext(), is(true));

        // move the cursor to <modelVersion>
        assertThat(goToStartElement(reader, "modelVersion"), is(true));
        reader.mark("<modelVersion>");

        // move the cursor to </modelVersion>
        assertThat(goToEndElement(reader, "modelVersion"), is(true));
        reader.mark("</modelVersion>");

        assertThat(reader.getBetween("<modelVersion>", "</modelVersion>"), is("4.0.0"));

        // to check if the replacement has not shifted offsets, move inside the <groupId> element
        assertThat(goToStartElement(reader, "groupId"), is(true));
        reader.next();
        assertThat(reader.isCharacters(), is(true));

        assertThat(
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()),
                equalTo("localhost"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testReplaceBetween(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertThat(reader.getEventType(), is(START_DOCUMENT));
        assertThat(reader.hasNext(), is(true));

        // move the cursor to <version>
        assertThat(goToStartElement(reader, "version"), is(true));
        reader.mark("<version>");

        // move the cursor to </version>
        assertThat(goToEndElement(reader, "version"), is(true));
        reader.mark("</version>");
        assertThat(reader.getBetween("<version>", "</version>"), is("1.0"));

        // move the cursor to <properties>
        assertThat(goToStartElement(reader, "properties"), is(true));
        reader.mark("<properties>");

        // move the cursor to </properties>
        assertThat(goToEndElement(reader, "properties"), is(true));
        reader.mark("</properties>");

        assertThat(
                reader.getBetween("<properties>", "</properties>"),
                allOf(containsString("<api>2.0</api>"), containsString("<impl>1.0</impl>")));

        reader.replaceBetween("<version>", "</version>", "1.1.0-SNAPSHOT");
        assertThat(reader.getSource(), containsString("<version>1.1.0-SNAPSHOT</version>"));
        assertThat(reader.getBetween("<version>", "</version>"), is("1.1.0-SNAPSHOT"));

        // after the replacement, offsets between marks should have been adjusted
        assertThat(
                reader.getBetween("<properties>", "</properties>"),
                allOf(containsString("<api>2.0</api>"), containsString("<impl>1.0</impl>")));

        // offsets outside marks should also be correct
        assertThat(goToStartElement(reader, "groupId"), is(true));
        reader.next();
        assertThat(reader.isCharacters(), is(true));

        assertThat(
                reader.getSource()
                        .substring(reader.getCurrentStartingCharOffset(), reader.getCurrentEndingCharOffset()),
                equalTo("localhost"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"iso-8859-2", "utf-8", "utf-16"})
    void testReplaceMark(String value) throws XMLStreamException, IOException, TransformerException {
        MutableXMLStreamReader reader =
                new MutableXMLStreamReader(getClass().getResourceAsStream(value + ".xml"), PATH);

        assertThat(reader.getEventType(), is(START_DOCUMENT));
        assertThat(reader.hasNext(), is(true));

        // create a set of marks in and around <version>
        assertThat(goToStartElement(reader, "version"), is(true));
        reader.mark("<version>");
        reader.next();
        assertThat(reader.isCharacters(), is(true));
        reader.mark("version");
        assertThat(goToEndElement(reader, "version"), is(true));
        reader.mark("</version>");

        // create another set of marks in and around <api>
        assertThat(goToStartElement(reader, "api"), is(true));
        reader.mark("<api>");
        reader.next();
        assertThat(reader.isCharacters(), is(true));
        reader.mark("api");
        assertThat(goToEndElement(reader, "api"), is(true));
        reader.mark("</api>");

        assertThat(reader.getBetween("<version>", "</version>"), is("1.0"));
        assertThat(reader.getBetween("<api>", "</api>"), is("2.0"));

        reader.replaceMark("version", "1.0.1-SNAPSHOT");
        assertThat(reader.getBetween("<version>", "</version>"), is("1.0.1-SNAPSHOT"));
        assertThat(reader.getBetween("<api>", "</api>"), is("2.0"));

        reader.replaceMark("version", "2");
        assertThat(reader.getBetween("<version>", "</version>"), is("2"));
        assertThat(reader.getBetween("<api>", "</api>"), is("2.0"));
    }
}
