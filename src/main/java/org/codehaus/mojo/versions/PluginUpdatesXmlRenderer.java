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

import static org.codehaus.mojo.versions.DependencyUpdatesXmlRenderer.getVersionsBlocks;
import static org.codehaus.mojo.versions.DependencyUpdatesXmlRenderer.wrapElement;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.maven.model.Plugin;
import org.apache.maven.reporting.MavenReportException;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.utils.PluginComparator;

/**
 * XML renderer for PluginUpdatesReport creates an xml file in target directory and writes report about available
 * plugin/plugin management updates.
 * 
 * @author Illia Dubinin
 * @since 2.4
 */
public class PluginUpdatesXmlRenderer
{

    private static final String GROUP_ID = "groupId";

    private static final String ARTIFACT_ID = "artifactId";

    private static final String OPEN_TAG = "<";

    private static final String CLOSE_TAG = ">";

    private static final String OPEN_CLOSING_TAG = "</";

    private static final String NL = "\n";

    private static final String TAB = "\t";

    private Map<Plugin, PluginUpdatesDetails> pluginUpdates;

    private Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates;

    private String outputFileName;

    public PluginUpdatesXmlRenderer( Map<Plugin, PluginUpdatesDetails> pluginUpdates,
                                     Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates, String outputFileName )
    {
        this.pluginUpdates = pluginUpdates;
        this.pluginManagementUpdates = pluginManagementUpdates;
        this.outputFileName = outputFileName;
    }

    /**
     * Makes report file with given name in target directory.
     * 
     * @throws MavenReportException if something went wrong
     */
    public void render()
        throws MavenReportException
    {
        StringBuilder sb = new StringBuilder();
        sb.append( "<PluginUpdatesReport>" ).append( NL );
        Map<Plugin, PluginUpdatesDetails> allUpdates =
            new TreeMap<Plugin, PluginUpdatesDetails>( new PluginComparator() );
        allUpdates.putAll( pluginManagementUpdates );
        allUpdates.putAll( pluginUpdates );
        sb.append( getSummaryBlock( allUpdates ) );
        sb.append( getPluginsInfoBlock( pluginManagementUpdates, "pluginManagements", "pluginManagement" ) );
        sb.append( getPluginsInfoBlock( pluginUpdates, "plugins", "plugin" ) );
        sb.append( "</PluginUpdatesReport>" ).append( NL );
        PrintWriter pw;
        try
        {
            pw = new PrintWriter( outputFileName, "UTF8" );
            pw.print( sb.toString() );
            pw.close();
        }
        catch ( IOException e )
        {
            throw new MavenReportException( "Cannot create xml report.", e );
        }
    }

    private static String getSummaryBlock( Map<Plugin, PluginUpdatesDetails> allUpdates )
    {
        Collection<ArtifactVersions> allVersions = new ArrayList<ArtifactVersions>();
        for ( PluginUpdatesDetails details : allUpdates.values() )
        {
            allVersions.add( details.getArtifactVersions() );
        }
        return DependencyUpdatesXmlRenderer.getSummaryBlock( allVersions );
    }

    private static String getPluginsInfoBlock( Map<Plugin, PluginUpdatesDetails> pluginUpdates, String blockName,
                                               String subblockName )
    {
        StringBuilder sBuilder = new StringBuilder();
        sBuilder.append( TAB ).append( OPEN_TAG ).append( blockName ).append( CLOSE_TAG ).append( NL );
        for ( Entry<Plugin, PluginUpdatesDetails> entry : pluginUpdates.entrySet() )
        {
            sBuilder.append( TAB ).append( TAB ).append( OPEN_TAG ).append( subblockName ).append( CLOSE_TAG ).append( NL );

            Plugin plugin = entry.getKey();
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( plugin.getGroupId(),
                                                                                    GROUP_ID ) ).append( NL );
            sBuilder.append( TAB ).append( TAB ).append( TAB ).append( wrapElement( plugin.getArtifactId(),
                                                                                    ARTIFACT_ID ) ).append( NL );

            sBuilder.append( getVersionsBlocks( entry.getValue().getArtifactVersions() ) );

            sBuilder.append( TAB ).append( TAB ).append( OPEN_CLOSING_TAG ).append( subblockName ).append( CLOSE_TAG ).append( NL );
        }
        sBuilder.append( TAB ).append( OPEN_CLOSING_TAG ).append( blockName ).append( CLOSE_TAG ).append( NL );
        return sBuilder.toString();
    }

}
