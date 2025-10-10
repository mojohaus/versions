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

import org.apache.maven.model.Dependency;
import org.codehaus.mojo.versions.api.ArtifactVersions;
import org.codehaus.mojo.versions.reporting.util.ReportRenderer;

/**
 * Model class for using with the {@linkplain ReportRenderer} API
 */
public class DependencyUpdatesModel extends AbstractUpdatesModel<ArtifactVersions> {
    /**
     * Creates a new model instance with the given maps of dependency updates.
     *
     * @param dependencyUpdates           the map of dependencies to their updates
     * @param dependencyManagementUpdates the map of dependency management entries to their updates
     */
    public DependencyUpdatesModel(
            Map<Dependency, ArtifactVersions> dependencyUpdates,
            Map<Dependency, ArtifactVersions> dependencyManagementUpdates) {
        super(dependencyUpdates, dependencyManagementUpdates, id -> id);
    }
}
