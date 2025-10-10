package org.codehaus.mojo.versions.rewriting;

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

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.maven.shared.utils.io.IOUtil;
import org.codehaus.stax2.LocationInfo;
import org.codehaus.stax2.XMLInputFactory2;
import org.codehaus.stax2.XMLStreamReader2;
import org.codehaus.stax2.util.StreamReader2Delegate;

import static java.util.Optional.ofNullable;

/**
 * A mutable {@link XMLStreamReader2}, allowing simple string manipulation (replacement) of the underlying document
 */
public class MutableXMLStreamReader extends StreamReader2Delegate implements AutoCloseable {
    private static final XMLInputFactory FACTORY = XMLInputFactory2.newInstance();

    private StringBuilder source;

    private final Path fileName;

    private boolean modified;

    private final Map<Object, MarkInfo> marks = new HashMap<>();

    /**
     * Current (start and end) offset (caused by mutations) against the {@link #getLocationInfo()} as reported by
     * the delegate
     */
    private final int[] delta = new int[2];

    static {
        FACTORY.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false);
    }

    private Charset sourceEncoding;

    /**
     * Constructs a new object using the contents of the given file as the initial contents of the document.
     *
     * @param path file containing the initial document contents
     * @throws IOException        thrown in case of an I/O problems
     * @throws XMLStreamException thrown if the file cannot be parsed
     */
    public MutableXMLStreamReader(Path path) throws IOException, XMLStreamException {
        this(Files.newInputStream(path), path);
    }

    /**
     * Constructs a new object using the contents of the given input stream as the initial contents of the document.
     *
     * @param inputStream stream containing the initial document contents
     * @param fileName    name of the file
     * @throws IOException        thrown in case of an I/O problems
     * @throws XMLStreamException thrown if the file cannot be parsed
     */
    public MutableXMLStreamReader(InputStream inputStream, Path fileName) throws IOException, XMLStreamException {
        super(null);
        this.fileName = fileName;
        init(inputStream);
        rewind();
    }

    /**
     * Returns the name of the file associated with the document
     *
     * @return name of the file associated with the document
     */
    public Path getFileName() {
        return fileName;
    }

    /**
     * Returns the current state of the document, in string format
     *
     * @return current state of the document, in string format
     */
    public String getSource() {
        return source.toString();
    }

    /**
     * Whether the document has been modified
     *
     * @return {@code true} if the document has been modified
     */
    public boolean isModified() {
        return modified;
    }

    /**
     * Returns current (adjusted by {@link #delta} starting char offset
     *
     * @return current (adjusted by {@link #delta} starting char offset
     */
    public int getCurrentStartingCharOffset() {
        return delta[0] + (int) getLocationInfo().getStartingCharOffset();
    }

    /**
     * Returns current (adjusted by {@link #delta} ending char offset
     *
     * @return current (adjusted by {@link #delta} ending char offset
     */
    public int getCurrentEndingCharOffset() {
        try {
            return delta[1] + (int) getLocationInfo().getEndingCharOffset();
        } catch (XMLStreamException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Replaces the current element with the replacement text
     *
     * @param replacement string replacing the current element
     */
    public void replace(String replacement) {
        if (getEventType() == START_DOCUMENT || getEventType() == END_DOCUMENT) {
            throw new IllegalStateException("Attempt at replacement outside of any element");
        }

        int start = getCurrentStartingCharOffset(), end = getCurrentEndingCharOffset();
        if (source.substring(start, end).equals(replacement)) {
            return;
        }

        source.replace(start, end, replacement);
        int delta = replacement.length() - (end - start);

        // marks can only be added as the cursor traverses the document onwards,
        // so the only marks affected by the replacement are marks set on the current element
        marks.values().stream().filter(mi -> mi.getEnd() == end).forEach(mi -> mi.setEnd(mi.getEnd() + delta));

        this.delta[1] += delta;
        modified = true;
    }

    private void validateMarks(Object... marks) {
        for (Object mark : marks) {
            if (!hasMark(mark)) {
                throw new IllegalStateException("Mark " + mark + " does not exist");
            }
        }
    }

    private void validateMarkOffsets(Object mark1, Object mark2) {
        if (marks.get(mark1).getStart() > marks.get(mark2).getStart()) {
            throw new IllegalStateException("Start offset of " + mark1 + "("
                    + marks.get(mark1).getEnd() + ") > start offset of " + mark2
                    + "(" + marks.get(mark2).getStart() + ")");
        }
    }

    /**
     * Returns the substring of the document between the end of the first mark and the start of the second marked element
     *
     * @param mark1 starting mark of the substring
     * @param mark2 ending mark of the substring
     * @return substring of the document between two marks
     */
    public String getBetween(Object mark1, Object mark2) {
        validateMarks(mark1, mark2);
        validateMarkOffsets(mark1, mark2);

        return Objects.equals(marks.get(mark1), marks.get(mark2))
                ? ""
                : source.substring(marks.get(mark1).getEnd(), marks.get(mark2).getStart());
    }

    /**
     * Replaces the document between two marks with the given replacement.
     *
     * @param mark1       starting mark of the string to be replaced
     * @param mark2       ending mark of the string to be replaced
     * @param replacement replacement string
     */
    public void replaceBetween(Object mark1, Object mark2, String replacement) {
        validateMarks(mark1, mark2);
        validateMarkOffsets(mark1, mark2);

        MarkInfo startMark = marks.get(mark1), endMark = marks.get(mark2);
        int start, end;
        if (!startMark.equals(endMark)) {
            start = startMark.getEnd();
            end = endMark.getStart();
            if (source.substring(start, end).equals(replacement)) {
                return;
            }
        } else {
            // Special case: property has no value (self-closing tag)
            String elementWithTags = source.substring(startMark.getStart(), startMark.getEnd());
            int closingTagIndex = elementWithTags.lastIndexOf("/>");
            String elementName = source.substring(
                    startMark.getStart() + elementWithTags.indexOf('<') + 1, startMark.getStart() + closingTagIndex);
            start = startMark.getStart() + closingTagIndex;
            end = startMark.getEnd();
            replacement = ">" + replacement + "</" + elementName + ">";
        }

        source.replace(start, end, replacement);
        int delta = replacement.length() - (end - start);

        marks.values().stream().filter(mi -> mi.getStart() >= end).forEach(mi -> mi.setStart(mi.getStart() + delta));
        marks.values().stream().filter(mi -> mi.getEnd() >= end).forEach(mi -> mi.setEnd(mi.getEnd() + delta));

        this.delta[1] += delta;
        modified = true;
    }

    /**
     * Replaces the document between the starting and ending character offset of the given mark
     *
     * @param mark        mark to be replaced between its starting and ending character offset
     * @param replacement replacement string
     */
    public void replaceMark(Object mark, String replacement) {
        validateMarks(mark);

        int start = marks.get(mark).getStart(), end = marks.get(mark).getEnd();
        if (source.substring(start, end).equals(replacement)) {
            return;
        }

        source.replace(start, end, replacement);
        int delta = replacement.length() - (end - start);

        marks.values().stream().filter(mi -> mi.getStart() >= end).forEach(mi -> mi.setStart(mi.getStart() + delta));
        marks.values().stream().filter(mi -> mi.getEnd() >= end).forEach(mi -> mi.setEnd(mi.getEnd() + delta));

        if (start < getCurrentStartingCharOffset()) {
            this.delta[0] += delta;
        }
        this.delta[1] += delta;
        modified = true;
    }

    /**
     * Recreates the underlying delegate {@link XMLStreamReader2} based on the current state of the document
     *
     * @throws XMLStreamException thrown if the document cannot be parsed
     */
    public void rewind() throws XMLStreamException {
        if (getParent() != null) {
            getParent().close();
        }

        marks.clear();
        delta[0] = 0;
        delta[1] = 0;

        XMLStreamReader2 reader = (XMLStreamReader2) FACTORY.createXMLStreamReader(
                new ByteArrayInputStream(source.toString().getBytes(sourceEncoding)), sourceEncoding.toString());
        setParent(reader);
    }

    @Override
    public int next() throws XMLStreamException {
        delta[0] = delta[1];
        return super.next();
    }

    /**
     * If the mark with the given {@code markNr} has been recorded
     *
     * @param markNr number of the mark to check
     * @return {@code true} if the given mark exists
     */
    public boolean hasMark(Object markNr) {
        return marks.containsKey(markNr);
    }

    /**
     * Records the current {@link LocationInfo} and delta under the given {@code markNr}
     *
     * @param markNr number of the mark to record to
     */
    public void mark(Object markNr) {
        marks.put(markNr, new MarkInfo(getCurrentStartingCharOffset(), getCurrentEndingCharOffset()));
    }

    /**
     * Removes the mark under the given {@code markNr}
     *
     * @param markNr number of the mark to remove
     */
    public void clearMark(Object markNr) {
        marks.remove(markNr);
    }

    private void init(InputStream inputStream) throws IOException, XMLStreamException {
        try (BufferedInputStream buf = new BufferedInputStream(inputStream)) {
            buf.mark(0x4000);

            // detect the encoding
            XMLStreamReader reader = FACTORY.createXMLStreamReader(buf);
            sourceEncoding =
                    ofNullable(reader.getEncoding()).map(Charset::forName).orElse(Charset.defaultCharset());
            reader.close();

            // offload the entire contents to this.source
            buf.reset();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            IOUtil.copy(buf, baos);
            source = new StringBuilder(baos.toString(sourceEncoding.toString()));
        }
    }

    /**
     * Contains startingCharOffset and endingCharOffset information for a mark
     */
    private static class MarkInfo {
        private int start;
        private int end;

        MarkInfo(int start, int end) {
            this.start = start;
            this.end = end;
        }

        int getStart() {
            return start;
        }

        void setStart(int value) {
            start = value;
        }

        int getEnd() {
            return end;
        }

        void setEnd(int value) {
            end = value;
        }

        @Override
        public int hashCode() {
            return Objects.hash(start, end);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof MarkInfo)) {
                return false;
            }
            MarkInfo other = (MarkInfo) obj;
            return start == other.start && end == other.end;
        }

        @Override
        public String toString() {
            return "MarkInfo[" + getStart() + ":" + getEnd() + "]";
        }
    }
}
