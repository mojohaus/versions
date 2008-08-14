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

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.XMLEvent;
import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

/**
 * Represents the modified pom file. Note: implementations of the StAX API (JSR-173) are not good round-trip rewriting
 * <b>while</b> keeping all unchanged bytes in the file as is.  For example, the StAX API specifies that <code>CR</code>
 * characters will be stripped.  Current implementations do not keep &quot; and &apos; characters consistent.
 *
 * @author <a href="mailto:stephen.alan.connolly@gmail.com">Stephen Connolly</a>
 */
public class ModifiedPomXMLEventReader
    implements XMLEventReader
{
    private final StringBuffer pom;

    private boolean modified = false;

    private final XMLInputFactory factory;

    private int nextStart = 0;

    private int nextEnd = 0;

    private int[] markStart = new int[MAX_MARKS];

    private int[] markEnd = new int[MAX_MARKS];

    private int[] markDelta = new int[MAX_MARKS];

    private int lastStart = -1;

    private int lastEnd = -1;

    private int lastDelta = 0;

    private XMLEvent next = null;

    private int nextDelta = 0;

    private XMLEventReader backing;

    private static final int MAX_MARKS = 3;

    public ModifiedPomXMLEventReader( StringBuffer pom, XMLInputFactory factory )
        throws XMLStreamException
    {
        this.pom = pom;
        this.factory = factory;
        rewind();
    }

    public void rewind()
        throws XMLStreamException
    {
        try
        {
            backing = factory.createXMLEventReader( new ByteArrayInputStream( pom.toString().getBytes( "utf-8" ) ) );
            nextEnd = 0;
            nextDelta = 0;
            for ( int i = 0; i < MAX_MARKS; i++ )
            {
                markStart[i] = -1;
                markEnd[i] = -1;
                markDelta[i] = 0;
            }
            lastStart = -1;
            lastEnd = -1;
            lastDelta = 0;
        }
        catch ( UnsupportedEncodingException e )
        {
            throw new XMLStreamException( e );
        }
    }

    public void clearMark( int index )
    {
        markStart[index] = -1;
    }

    public void mark( int index )
    {
        markStart[index] = lastStart;
        markEnd[index] = lastEnd;
        markDelta[index] = lastDelta;
    }

    public boolean hasMark( int index )
    {
        return markStart[index] != -1;
    }

    public void replace( String replacement )
    {
        if ( lastStart < 0 || lastEnd < lastStart )
        {
            throw new IllegalStateException();
        }
        if ( replacement.equals( pom.substring( lastDelta + lastStart, lastDelta + lastEnd ) ) )
        {
            return;
        }
        pom.replace( lastDelta + lastStart, lastDelta + lastEnd, replacement );
        int delta = replacement.length() - lastEnd - lastStart;
        nextDelta += delta;
        for ( int i = 0; i < MAX_MARKS; i++ )
        {
            if ( hasMark( i ) && lastStart == markStart[i] && markEnd[i] == lastEnd )
            {
                markEnd[i] += delta;
            }
        }
        lastEnd += delta;
        modified = true;
    }

    public void replaceMark( int index, String replacement )
    {
        if ( !hasMark( index ) )
        {
            throw new IllegalStateException();
        }
        if ( replacement.equals(
            pom.substring( markDelta[index] + markStart[index], markDelta[index] + markEnd[index] ) ) )
        {
            return;
        }
        pom.replace( markDelta[index] + markStart[index], markDelta[index] + markEnd[index], replacement );
        int delta = replacement.length() - markEnd[index] - markStart[index];
        nextDelta += delta;
        if ( lastStart == markStart[index] && lastEnd == markEnd[index] )
        {
            lastEnd += delta;
        }
        else if ( lastStart > markStart[index] )
        {
            lastDelta += delta;
        }
        for ( int i = 0; i < MAX_MARKS; i++ )
        {
            if ( i == index || markStart[i] == -1 )
            {
                continue;
            }
            if ( markStart[i] > markStart[index] )
            {
                markDelta[i] += delta;
            }
            else if ( markStart[i] == markStart[index] && markEnd[i] == markEnd[index] )
            {
                markDelta[i] += delta;
            }
        }
        markEnd[index] += delta;
        modified = true;
    }

    public void replaceBetween( int index1, int index2, String replacement )
    {
        if ( !hasMark( index1 ) || !hasMark( index2 ) || markStart[index1] > markStart[index2] )
        {
            throw new IllegalStateException();
        }
        if ( replacement.equals(
            pom.substring( markDelta[index1] + markEnd[index1], markDelta[index2] + markStart[index2] ) ) )
        {
            return;
        }
        pom.replace( markDelta[index1] + markEnd[index1], markDelta[index2] + markStart[index2], replacement );
        int delta = replacement.length() -
            ( ( markDelta[index2] + markStart[index2] ) - ( markDelta[index1] + markEnd[index1] ) );
        nextDelta += delta;

        for ( int i = 0; i < MAX_MARKS; i++ )
        {
            if ( i == index1 || i == index2 || markStart[i] == -1 )
            {
                continue;
            }
            if ( markStart[i] > markStart[index2] )
            {
                markDelta[i] += delta;
            }
            else if ( markStart[i] == markEnd[index1] && markEnd[i] == markStart[index1] )
            {
                markDelta[i] += delta;
            }
            else if ( markStart[i] > markEnd[index1] || markEnd[i] < markStart[index2] )
            {
                markStart[i] = -1;
            }
        }

        modified = true;
    }

    /**
     * Getter for property 'modified'.
     *
     * @return Value for property 'modified'.
     */
    public boolean isModified()
    {
        return modified;
    }

    /**
     * {@inheritDoc}
     */
    public XMLEvent nextEvent()
        throws XMLStreamException
    {
        try
        {
            return next;
        }
        finally
        {
            next = null;
            lastStart = nextStart;
            lastEnd = nextEnd;
            lastDelta = nextDelta;
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean hasNext()
    {
        if ( next != null )
        {
            // fast path
            return true;
        }
        if ( !backing.hasNext() )
        {
            // fast path
            return false;
        }
        try
        {
            next = backing.nextEvent();
            nextStart = nextEnd;
            nextEnd = next.getLocation().getCharacterOffset();
            if ( nextEnd != -1 )
            {
                if ( !next.isCharacters() )
                {
                    while ( nextStart < nextEnd && ( c( nextStart ) == '\n' || c( nextStart ) == '\r' ) )
                    {
                        nextStart++;
                    }
                }
                else
                {
                    while ( nextEnd > nextStart + 1 && ( c( nextEnd - 1 ) == '<' || c( nextEnd - 1 ) == '&' ||
                        ( nextEnd > nextStart + 2 && c( nextEnd - 2 ) == '<' ) ) )
                    {
                        nextEnd--;
                    }
                }
            }
            return true;
        }
        catch ( XMLStreamException e )
        {
            return false;
        }
    }

    private char c( int index )
    {
        return pom.charAt( nextDelta + index );
    }

    /**
     * Getter for property 'peekVerbatim'.
     *
     * @return Value for property 'peekVerbatim'.
     */
    public String getPeekVerbatim()
    {
        if ( hasNext() )
        {
            return pom.substring( nextDelta + nextStart, nextDelta + nextEnd );
        }
        return "";
    }

    public String getMarkVerbatim( int index )
    {
        if ( hasMark( index ) )
        {
            return pom.substring( markDelta[index] + markStart[index], markDelta[index] + markEnd[index] );
        }
        return "";
    }

    /**
     * Getter for property 'verbatim'.
     *
     * @return Value for property 'verbatim'.
     */
    public String getVerbatim()
    {
        if ( lastStart >= 0 && lastEnd >= lastStart )
        {
            return pom.substring( lastDelta + lastStart, lastDelta + lastEnd );
        }
        return "";
    }

    /**
     * {@inheritDoc}
     */
    public Object next()
    {
        try
        {
            return nextEvent();
        }
        catch ( XMLStreamException e )
        {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     */
    public void remove()
    {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    public XMLEvent peek()
        throws XMLStreamException
    {
        return backing.peek();
    }

    /**
     * {@inheritDoc}
     */
    public String getElementText()
        throws XMLStreamException
    {
        return backing.getElementText();
    }

    /**
     * {@inheritDoc}
     */
    public XMLEvent nextTag()
        throws XMLStreamException
    {
        while ( hasNext() )
        {
            XMLEvent e = nextEvent();
            if ( e.isCharacters() && !( (Characters) e ).isWhiteSpace() )
            {
                throw new XMLStreamException( "Unexpected text" );
            }
            if ( e.isStartElement() || e.isEndElement() )
            {
                return e;
            }
        }
        throw new XMLStreamException( "Unexpected end of Document" );
    }

    /**
     * {@inheritDoc}
     */
    public Object getProperty( String name )
        throws IllegalArgumentException
    {
        return backing.getProperty( name );
    }

    /**
     * {@inheritDoc}
     */
    public void close()
        throws XMLStreamException
    {
        backing.close();
        next = null;
        backing = null;
    }

    public StringBuffer asStringBuffer()
    {
        return pom;
    }
}
