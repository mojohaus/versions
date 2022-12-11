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

import javax.xml.stream.Location;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import java.io.StringReader;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link ModifiedPomXMLEventReaderTest}
 *
 * @author Andrzej Jarmoniuk
 */
@RunWith(MockitoJUnitRunner.class)
public class ModifiedPomXMLEventReaderTest extends AbstractMojoTestCase {
    private static final String[] STR = {"xyz", "0123456789abcdef"};
    private static final String REPLACEMENT = "abcdef";

    @Mock
    private Location location;

    @Mock
    private XMLEvent xmlEvent;

    @Mock
    private XMLEventReader xmlEventReader;

    @Mock
    private XMLInputFactory xmlInputFactory;

    private ModifiedPomXMLEventReader pomXMLEventReader;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        when(location.getCharacterOffset()).thenReturn(STR[0].length()).thenReturn(STR[0].length() + STR[1].length());

        when(xmlEvent.isCharacters()).thenReturn(true);
        when(xmlEvent.getLocation()).thenReturn(location);

        when(xmlEventReader.hasNext())
                .thenReturn(true)
                .thenReturn(true) // str[0]
                .thenReturn(true)
                .thenReturn(true) // str[1]
                .thenReturn(false); // ∅
        when(xmlEventReader.nextEvent()).thenReturn(xmlEvent).thenReturn(xmlEvent);
        when(xmlEventReader.peek()).thenReturn(xmlEvent);

        when(xmlInputFactory.createXMLEventReader(any(StringReader.class))).thenReturn(xmlEventReader);

        pomXMLEventReader =
                new ModifiedPomXMLEventReader(new StringBuilder(STR[0]).append(STR[1]), xmlInputFactory, "");
    }

    @Test
    public void testReplace() throws XMLStreamException, IllegalAccessException {
        assertThat(pomXMLEventReader.hasNext(), is(true));
        assertThat(pomXMLEventReader.nextEvent(), is(xmlEvent));

        assertThat(pomXMLEventReader.hasNext(), is(true));
        assertThat(pomXMLEventReader.nextEvent(), is(xmlEvent));

        pomXMLEventReader.replace(REPLACEMENT);
        assertThat(pomXMLEventReader.asStringBuilder().toString(), is(STR[0] + REPLACEMENT));

        pomXMLEventReader.mark(0);
        assertThat(pomXMLEventReader.getMarkVerbatim(0), is(REPLACEMENT));

        // more dangerous test since this touches the implementation
        assertThat(getVariableValueFromObject(pomXMLEventReader, "lastEnd"), is((STR[0] + REPLACEMENT).length()));
    }

    @Test
    public void testReplaceMark() throws XMLStreamException, IllegalAccessException {
        assertThat(pomXMLEventReader.hasNext(), is(true));
        assertThat(pomXMLEventReader.nextEvent(), is(xmlEvent));

        assertThat(pomXMLEventReader.hasNext(), is(true));
        assertThat(pomXMLEventReader.nextEvent(), is(xmlEvent));

        pomXMLEventReader.mark(0);

        pomXMLEventReader.replaceMark(0, REPLACEMENT);
        assertThat(pomXMLEventReader.asStringBuilder().toString(), is(STR[0] + REPLACEMENT));

        pomXMLEventReader.mark(0);
        assertThat(pomXMLEventReader.getMarkVerbatim(0), is(REPLACEMENT));

        // more dangerous test since this touches the implementation
        assertThat(getVariableValueFromObject(pomXMLEventReader, "lastEnd"), is((STR[0] + REPLACEMENT).length()));
    }
}
