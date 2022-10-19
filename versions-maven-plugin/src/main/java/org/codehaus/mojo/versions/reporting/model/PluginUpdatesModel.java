package org.codehaus.mojo.versions.reporting.model;

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

import java.util.Map;

import org.apache.maven.model.Plugin;
import org.codehaus.mojo.versions.api.PluginUpdatesDetails;
import org.codehaus.mojo.versions.utils.DependencyBuilder;

/**
 * Model class for using with the {@linkplain org.codehaus.mojo.versions.api.ReportRenderer} API
 */
public class PluginUpdatesModel extends AbstractUpdatesModel<PluginUpdatesDetails>
{
    public PluginUpdatesModel( Map<Plugin, PluginUpdatesDetails> pluginUpdates,
                               Map<Plugin, PluginUpdatesDetails> pluginManagementUpdates )
    {
        super( pluginUpdates, pluginManagementUpdates, p -> DependencyBuilder.newBuilder()
                .withGroupId( p.getGroupId() )
                .withArtifactId( p.getArtifactId() )
                .withVersion( p.getVersion() )
                .build() );
    }
}
