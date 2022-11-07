package org.codehaus.mojo.versions.recording;

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

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Objects;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A recorder of version updates.
 */

public class ChangeRecorderXML implements ChangeRecorder
{
    /**
     * The XML namespace used for serialized changes.
     */

    public static final String CHANGES_NAMESPACE = "http://www.mojohaus.org/versions-maven-plugin/schema/updates/1.0";

    private final Document document;
    private final Element root;

    /**
     * Creates a new instance
     */
    public ChangeRecorderXML()
    {
        try
        {
            final DocumentBuilderFactory documentBuilders = DocumentBuilderFactory.newInstance();
            final DocumentBuilder documentBuilder = documentBuilders.newDocumentBuilder();
            document = documentBuilder.newDocument();
            root = document.createElementNS( CHANGES_NAMESPACE, "updates" );
            document.appendChild( root );
        }
        catch ( final ParserConfigurationException | DOMException e )
        {
            throw new IllegalStateException( e );
        }
    }

    @Override
    public final void recordUpdate( final String kind, final String groupId, final String artifactId,
                                    final String oldVersion, final String newVersion )
    {
        Objects.requireNonNull( kind, "kind" );
        Objects.requireNonNull( groupId, "groupId" );
        Objects.requireNonNull( artifactId, "artifactId" );
        Objects.requireNonNull( oldVersion, "oldVersion" );
        Objects.requireNonNull( newVersion, "newVersion" );

        final Element update = this.document.createElementNS( CHANGES_NAMESPACE, "update" );
        update.setAttribute( "kind", kind );
        update.setAttribute( "groupId", groupId );
        update.setAttribute( "artifactId", artifactId );
        update.setAttribute( "oldVersion", oldVersion );
        update.setAttribute( "newVersion", newVersion );
        this.root.appendChild( update );
    }

    @Override
    public final void serialize( final OutputStream outputStream ) throws IOException
    {
        try
        {
            final Transformer transformer = TransformerFactory.newInstance().newTransformer();
            final Source source = new DOMSource( this.document );
            transformer.transform( source, new StreamResult( outputStream ) );
            outputStream.flush();
        }
        catch ( final TransformerException ex )
        {
            throw new IOException( ex );
        }
    }
}
