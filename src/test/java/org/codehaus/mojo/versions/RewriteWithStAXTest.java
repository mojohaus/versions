package org.codehaus.mojo.versions;

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


import junit.framework.TestCase;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.codehaus.stax2.XMLInputFactory2;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLEventWriter;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Stack;

/**
 * Basic tests for rewriting XML with a StAX (JSR-173) implementation.
 *
 * @author Stephen Connolly
 */
public class RewriteWithStAXTest
    extends TestCase
{
    public void testBasic()
        throws Exception
    {
        String input = "<?xml version='1.0' encoding='utf-8'?>\n" + "<project>\n\r\n\r\n\r\n\r" + "  <parent>\r\n" +
            "    <groupId xmlns='foo'>org.codehaus.mojo</groupId>\n" +
            "    <artifactId>mojo-&amp;sandbox-parent</artifactId>\n" + "    <version>5-SNAPSHOT</version>\r" +
            "  </parent>\r" + "<build/></project>";

        byte[] rawInput = input.getBytes( "utf-8" );
        ByteArrayInputStream source = new ByteArrayInputStream( rawInput );
        ByteArrayOutputStream dest = new ByteArrayOutputStream();
        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
        XMLOutputFactory outputFactory = XMLOutputFactory.newInstance();
        XMLEventReader eventReader = inputFactory.createXMLEventReader( source );
        XMLEventWriter eventWriter = outputFactory.createXMLEventWriter( dest, "utf-8" );
        while ( eventReader.hasNext() )
        {
            eventWriter.add( eventReader.nextEvent() );
        }

        String output = new String( dest.toByteArray(), "utf-8" );

        assertFalse( "StAX implementation is not good enough", input.equals( output ) );
    }

    public void testReplace()
        throws Exception
    {
        String input = "<?xml version='1.0' encoding='utf-8'?>\n" + "<project>\n\r\n\r\n\r\n\r" + "  <parent>\r\n" +
            "    <groupId xmlns='foo'>org.codehaus.mojo</groupId>\n" +
            "    <artifactId>mojo-&amp;sandbox-parent</artifactId>\n" + "    <version>5-SNAPSHOT</version>\r" +
            "  </parent>\r" + "<build/></project>";
        String expected = "<?xml version='1.0' encoding='utf-8'?>\n" + "<project>\n\r\n\r\n\r\n\r" + "  <parent>\r\n" +
            "    <groupId xmlns='foo'>org.codehaus.mojo</groupId>\n" + "    <artifactId>my-artifact</artifactId>\n" +
            "    <version>5-SNAPSHOT</version>\r" + "  </parent>\r" + "<build/></project>";

        StringBuffer output = new StringBuffer( input );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
        ModifiedPomXMLEventReader eventReader = new ModifiedPomXMLEventReader( output, inputFactory );
        while ( eventReader.hasNext() )
        {
            XMLEvent event = eventReader.nextEvent();
            if ( event instanceof StartElement &&
                event.asStartElement().getName().getLocalPart().equals( "artifactId" ) )
            {
                eventReader.mark( 0 );
            }
            if ( event instanceof EndElement && event.asEndElement().getName().getLocalPart().equals( "artifactId" ) )
            {
                eventReader.mark( 1 );
                if ( eventReader.hasMark( 0 ) )
                {
                    eventReader.replaceBetween( 0, 1, "my-artifact" );
                }
            }
        }

        assertEquals( expected, output.toString() );
    }

    public void testReplaceFancy()
        throws Exception
    {
        String input =
            "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">\n" +
                "  <modelVersion>4.0.0</modelVersion>\n" + "\n" + "  <parent>\n" +
                "    <groupId>org.codehaus.mojo</groupId>\n" + "    <artifactId>mojo-sandbox-parent</artifactId>\n" +
                "    <version>5-SNAPSHOT</version>\n" + "  </parent>\n" + "\n" +
                "  <groupId>org.codehaus.mojo</groupId>\n" + "  <artifactId>versions-maven-plugin</artifactId>\n" +
                "  <version>1.0.0-alpha-1-SNAPSHOT</version>\n" + "  <packaging>maven-plugin</packaging>\n" + "\n" +
                "  <name>Versions Maven Plugin</name>\n" + "  <description>\n" +
                "    Versions plugin for Maven 2. The versions plugin updates the versions of components in the pom.\n" +
                "  </description>\n" + "  <inceptionYear>2008</inceptionYear>\n" + "  <licenses>\n" +
                "    <license>\n" + "      <name>The Apache Software License, Version 2.0</name>\n" +
                "      <url>http://www.apache.org/licenses/LICENSE-2.0</url>\n" +
                "      <distribution>repo</distribution>\n" + "    </license>\n" + "  </licenses>\n" + "\n" +
                "  <scm>\n" +
                "    <connection>scm:svn:http://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</connection>\n" +
                "    <developerConnection>scm:svn:https://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</developerConnection>\n" +
                "    <url>http://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</url>\n" + "  </scm>\n" +
                "\n" + "  <developers>\n" + "    <developer>\n" + "      <name>Stephen Connolly</name>\n" +
                "      <email>stephen.alan.connolly@gmail.com</email>\n" + "      <roles>\n" +
                "        <role>Java Developer</role>\n" + "      </roles>\n" + "      <timezone>0</timezone>\n" +
                "    </developer>\n" + "  </developers>\n" + "\n" + "  <prerequisites>\n" +
                "    <maven>2.0.6</maven>\n" + "  </prerequisites>\n" + "\n" + "  <dependencies>\n" +
                "    <dependency>\n" + "      <groupId>junit</groupId>\n" + "      <artifactId>junit</artifactId>\n" +
                "      <version>3.8.1</version>\n" + "      <scope>test</scope>\n" + "    </dependency>\n" +
                "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-project</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-settings</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-plugin-api</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.codehaus.plexus</groupId>\n" +
                "      <artifactId>plexus-utils</artifactId>\n" + "      <version>1.3</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.codehaus.plexus</groupId>\n" +
                "      <artifactId>plexus-interactivity-api</artifactId>\n" + "      <version>1.0-alpha-6</version>\n" +
                "      <exclusions>\n" + "        <exclusion>\n" + "          <artifactId>plexus-utils</artifactId>\n" +
                "          <groupId>plexus</groupId>\n" + "        </exclusion>\n" + "      </exclusions>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>javax.xml.stream</groupId>\n" +
                "      <artifactId>stax-api</artifactId>\n" + "      <version>1.0-2</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>stax</groupId>\n" +
                "      <artifactId>stax</artifactId>\n" + "      <version>1.1.1-dev</version>\n" +
                "    </dependency>\n" + "  </dependencies>\n" + "\n" + "  <build>\n" + "    <plugins>\n" +
                "      <plugin>\n" + "        <artifactId>maven-plugin-plugin</artifactId>\n" +
                "        <version>2.3</version>\n" + "        <configuration>\n" +
                "          <goalPrefix>versions</goalPrefix>\n" + "        </configuration>\n" + "      </plugin>\n" +
                "    </plugins>\n" + "  </build>\n" + "\n" + "</project>";
        String expected =
            "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">\n" +
                "  <modelVersion>4.0.0</modelVersion>\n" + "\n" + "  <parent>\n" +
                "    <groupId>org.codehaus.mojo</groupId>\n" + "    <artifactId>mojo-sandbox-parent</artifactId>\n" +
                "    <version>4</version>\n" + "  </parent>\n" + "\n" + "  <groupId>org.codehaus.mojo</groupId>\n" +
                "  <artifactId>versions-maven-plugin</artifactId>\n" + "  <version>1.0.0-alpha-1-SNAPSHOT</version>\n" +
                "  <packaging>maven-plugin</packaging>\n" + "\n" + "  <name>Versions Maven Plugin</name>\n" +
                "  <description>\n" +
                "    Versions plugin for Maven 2. The versions plugin updates the versions of components in the pom.\n" +
                "  </description>\n" + "  <inceptionYear>2008</inceptionYear>\n" + "  <licenses>\n" +
                "    <license>\n" + "      <name>The Apache Software License, Version 2.0</name>\n" +
                "      <url>http://www.apache.org/licenses/LICENSE-2.0</url>\n" +
                "      <distribution>repo</distribution>\n" + "    </license>\n" + "  </licenses>\n" + "\n" +
                "  <scm>\n" +
                "    <connection>scm:svn:http://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</connection>\n" +
                "    <developerConnection>scm:svn:https://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</developerConnection>\n" +
                "    <url>http://svn.codehaus.org/mojo/trunk/sandbox/versions-maven-plugin</url>\n" + "  </scm>\n" +
                "\n" + "  <developers>\n" + "    <developer>\n" + "      <name>Stephen Connolly</name>\n" +
                "      <email>stephen.alan.connolly@gmail.com</email>\n" + "      <roles>\n" +
                "        <role>Java Developer</role>\n" + "      </roles>\n" + "      <timezone>0</timezone>\n" +
                "    </developer>\n" + "  </developers>\n" + "\n" + "  <prerequisites>\n" +
                "    <maven>2.0.6</maven>\n" + "  </prerequisites>\n" + "\n" + "  <dependencies>\n" +
                "    <dependency>\n" + "      <groupId>junit</groupId>\n" + "      <artifactId>junit</artifactId>\n" +
                "      <version>3.8.2</version>\n" + "      <scope>test</scope>\n" + "    </dependency>\n" +
                "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-project</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-settings</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.apache.maven</groupId>\n" +
                "      <artifactId>maven-plugin-api</artifactId>\n" + "      <version>2.0</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.codehaus.plexus</groupId>\n" +
                "      <artifactId>plexus-utils</artifactId>\n" + "      <version>1.3</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>org.codehaus.plexus</groupId>\n" +
                "      <artifactId>plexus-interactivity-api</artifactId>\n" + "      <version>1.0-alpha-6</version>\n" +
                "      <exclusions>\n" + "        <exclusion>\n" + "          <artifactId>plexus-utils</artifactId>\n" +
                "          <groupId>plexus</groupId>\n" + "        </exclusion>\n" + "      </exclusions>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>javax.xml.stream</groupId>\n" +
                "      <artifactId>stax-api</artifactId>\n" + "      <version>1.0-2</version>\n" +
                "    </dependency>\n" + "    <dependency>\n" + "      <groupId>stax</groupId>\n" +
                "      <artifactId>stax</artifactId>\n" + "      <version>1.1.1-dev</version>\n" +
                "    </dependency>\n" + "  </dependencies>\n" + "\n" + "  <build>\n" + "    <plugins>\n" +
                "      <plugin>\n" + "        <artifactId>maven-plugin-plugin</artifactId>\n" +
                "        <version>2.3</version>\n" + "        <configuration>\n" +
                "          <goalPrefix>versions</goalPrefix>\n" + "        </configuration>\n" + "      </plugin>\n" +
                "    </plugins>\n" + "  </build>\n" + "\n" + "</project>";

        StringBuffer output = new StringBuffer( input );

        XMLInputFactory inputFactory = XMLInputFactory2.newInstance();
        inputFactory.setProperty( XMLInputFactory2.P_PRESERVE_LOCATION, Boolean.TRUE );
        ModifiedPomXMLEventReader eventReader = new ModifiedPomXMLEventReader( output, inputFactory );

        Stack stack = new Stack();
        String path = "";

        while ( eventReader.hasNext() )
        {
            XMLEvent event = eventReader.nextEvent();
            if ( event.isStartElement() )
            {
                stack.push( path );
                path += "/" + event.asStartElement().getName().getLocalPart();

                if ( "/project/parent/version".equals( path ) )
                {
                    eventReader.mark( 0 );
                }
            }
            if ( event.isEndElement() )
            {
                if ( "/project/parent/version".equals( path ) )
                {
                    eventReader.mark( 1 );
                    if ( eventReader.hasMark( 0 ) )
                    {
                        eventReader.replaceBetween( 0, 1, "4" );
                    }
                }
                path = (String) stack.pop();
            }
        }

        boolean inDependency = false;
        boolean groupIdMatches = false;
        boolean artifactIdMatches = false;
        eventReader.rewind();

        while ( eventReader.hasNext() )
        {
            XMLEvent event = eventReader.nextEvent();
            if ( event.isStartElement() )
            {
                String name = event.asStartElement().getName().getLocalPart();
                if ( inDependency )
                {
                    if ( "groupId".equals( name ) )
                    {
                        groupIdMatches = "junit".equals( eventReader.getElementText() );
                    }
                    else if ( "artifactId".equals( name ) )
                    {
                        artifactIdMatches = "junit".equals( eventReader.getElementText() );
                    }
                    else if ( "version".equals( name ) )
                    {
                        eventReader.mark( 1 );
                    }
                }
                else if ( "dependency".equals( name ) )
                {
                    inDependency = true;
                    groupIdMatches = false;
                    artifactIdMatches = false;
                }
            }
            if ( event.isEndElement() )
            {
                String name = event.asEndElement().getName().getLocalPart();
                if ( inDependency )
                {
                    if ( "version".equals( name ) )
                    {
                        eventReader.mark( 2 );

                    }
                    else if ( "dependency".equals( name ) )
                    {
                        if ( groupIdMatches && artifactIdMatches && eventReader.hasMark( 1 ) &&
                            eventReader.hasMark( 2 ) )
                        {
                            eventReader.replaceBetween( 1, 2, "3.8.2" );
                        }
                        inDependency = false;
                    }
                }
            }
        }

        assertEquals( expected, output.toString() );
    }

}
