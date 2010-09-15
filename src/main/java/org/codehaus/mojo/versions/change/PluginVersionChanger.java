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

import javax.xml.stream.XMLStreamException;

import org.apache.maven.model.Model;
import org.apache.maven.plugin.logging.Log;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @since 15-Sep-2010 16:00:54
 */
public class PluginVersionChanger
    extends AbstractVersionChanger
{

    public PluginVersionChanger( Model model, ModifiedPomXMLEventReader pom, Log reporter )
    {
        super( model, pom, reporter );
    }

    public void apply( VersionChange versionChange )
        throws XMLStreamException
    {
        if ( PomHelper.setPluginVersion( getPom(), versionChange.getGroupId(), versionChange.getArtifactId(),
                                         versionChange.getOldVersion(), versionChange.getNewVersion() ) )
        {
            info( "    Updating plugin " + versionChange.getGroupId() + ":" + versionChange.getArtifactId() );
            info( "        from version " + versionChange.getOldVersion() + " to " + versionChange.getNewVersion() );
        }
    }
}
