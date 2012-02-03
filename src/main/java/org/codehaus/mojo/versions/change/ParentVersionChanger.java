package org.codehaus.mojo.versions.change;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

import javax.xml.stream.XMLStreamException;

/**
 *
 */
public class ParentVersionChanger
    extends AbstractVersionChanger
{

    public ParentVersionChanger( Model model, ModifiedPomXMLEventReader pom, Log reporter )
    {
        super( model, pom, reporter );
    }

    public void apply( VersionChange versionChange )
        throws XMLStreamException
    {
        if ( getModel().getParent() != null && versionChange.getGroupId().equals( getModel().getParent().getGroupId() )
            && versionChange.getArtifactId().equals( getModel().getParent().getArtifactId() ) )
        {
            if ( PomHelper.setProjectParentVersion( getPom(), versionChange.getNewVersion() ) )
            {
                info( "    Updating parent " + versionChange.getGroupId() + ":" + versionChange.getArtifactId() );
                info(
                    "        from version " + versionChange.getOldVersion() + " to " + versionChange.getNewVersion() );
            }
        }
    }
}
