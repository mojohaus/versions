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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public final class ChangeRecorderXMLTest
{
    private static void copyResource( final String name, final File output ) throws IOException
    {
        try ( FileOutputStream outputStream = new FileOutputStream( output ) )
        {
            try ( InputStream inputStream = ChangeRecorderXMLTest.class.getResourceAsStream( name ) )
            {
                IOUtils.copy( inputStream, outputStream );
            }
        }
    }

    private static Document parseXML( final File file ) throws ParserConfigurationException, IOException, SAXException
    {
        final DocumentBuilderFactory documentBuilders = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = documentBuilders.newDocumentBuilder();
        return documentBuilder.parse( file );
    }

    @Test
    public void testChanges() throws Exception
    {
        final File file0 = File.createTempFile( "ChangeRecorderTest", ".xml" );
        final File file1 = File.createTempFile( "ChangeRecorderTest", ".xml" );

        copyResource( "expectedFile.xml", file0 );

        final ChangeRecorder recorder = new ChangeRecorderXML();
        recorder.recordUpdate( "exampleKind", "org.codehaus", "example0", "0.0.1", "0.0.2" );
        recorder.recordUpdate( "exampleKind", "org.codehaus", "example1", "1.0.0", "2.0.0" );

        try ( FileOutputStream outputStream = new FileOutputStream( file1 ) )
        {
            recorder.serialize( outputStream );
        }

        final Document document0 = parseXML( file0 );
        final Document document1 = parseXML( file1 );

        final NodeList elements0 = document0.getElementsByTagNameNS( ChangeRecorderXML.CHANGES_NAMESPACE, "updated" );
        final NodeList elements1 = document1.getElementsByTagNameNS( ChangeRecorderXML.CHANGES_NAMESPACE, "updated" );

        Assert.assertEquals( "Correct number of updates", elements0.getLength(), elements1.getLength() );

        for ( int index = 0; index < elements0.getLength(); ++index )
        {
            final Element element0 = (Element) elements0.item( index );
            final Element element1 = (Element) elements1.item( index );

            Assert.assertEquals( element0.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "artifactId" ),
                    element1.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "artifactId" ) );
            Assert.assertEquals( element0.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "groupId" ),
                    element1.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "groupId" ) );
            Assert.assertEquals( element0.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "oldVersion" ),
                    element1.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "oldVersion" ) );
            Assert.assertEquals( element0.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "newVersion" ),
                    element1.getAttributeNS( ChangeRecorderXML.CHANGES_NAMESPACE, "newVersion" ) );
        }
    }
}
