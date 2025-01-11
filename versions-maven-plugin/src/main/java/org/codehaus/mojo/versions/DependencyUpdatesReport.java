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

import javax.inject.Inject;

import java.util.Map;
import java.util.Set;

import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.reporting.ReportRendererFactory;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates a report of available updates for the dependencies of a project.
 *
 * @author Stephen Connolly
 * @since 1.0-beta-1
 */
@Mojo(name = "dependency-updates-report", requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true)
public class DependencyUpdatesReport extends AbstractDependencyUpdatesReport {

    @Inject
    protected DependencyUpdatesReport(
            I18N i18n,
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            ReportRendererFactory rendererFactory) {
        super(i18n, artifactFactory, repositorySystem, wagonMap, rendererFactory);
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populateDependencies(Set<Dependency> dependenciesCollector) {
        getLog().debug(String.format(
                "Collecting dependencies for project %s", getProject().getName()));
        dependenciesCollector.addAll(getProject().getDependencies());
    }

    /**
     * {@inheritDoc}
     * */
    @Override
    protected void populateDependencyManagement(Set<Dependency> dependencyManagementCollector) {
        if (hasDependencyManagement(getProject())) {
            getLog().debug(String.format(
                    "Collecting managed dependencies for project %s",
                    getProject().getName()));
            handleDependencyManagementTransitive(getProject(), dependencyManagementCollector);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBundleName() {
        return "dependency-updates-report";
    }
}
