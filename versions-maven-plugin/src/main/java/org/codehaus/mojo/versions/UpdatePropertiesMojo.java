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
import javax.xml.stream.XMLStreamException;

import java.util.List;
import java.util.Map;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

/**
 * Sets properties to the latest versions of specific artifacts.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-1
 */
@Mojo(name = "update-properties", threadSafe = true)
public class UpdatePropertiesMojo extends UpdatePropertiesMojoBase {
    /**
     * Any restrictions that apply to specific properties.
     *
     * @since 1.0-alpha-3
     */
    @Parameter
    private List<Property> properties;

    /**
     * A comma separated list of properties to update.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "includeProperties")
    private String includeProperties = null;

    /**
     * A comma separated list of properties to not update.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "excludeProperties")
    private String excludeProperties = null;

    /**
     * Whether to process the dependencies section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencies", defaultValue = "true")
    private boolean processDependencies = true;

    /**
     * Whether to process the dependencyManagement section of the project.
     *
     * @since 1.0-alpha-3
     */
    @Parameter(property = "processDependencyManagement", defaultValue = "true")
    private boolean processDependencyManagement = true;

    /**
     * Whether to process the parent section of the project. If not set will default to false.
     *
     * @since 2.3
     */
    @Parameter(property = "processParent", defaultValue = "false")
    private boolean processParent = false;

    /**
     * Creates a new instance
     * @param artifactFactory an {@link ArtifactFactory} instance
     * @param repositorySystem a {@link RepositorySystem} instance
     * @param wagonMap a map of wagon providers per protocol
     * @param changeRecorderFactories a map of change recorder factories
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public UpdatePropertiesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected boolean getProcessDependencies() {
        return processDependencies;
    }

    @Override
    protected boolean getProcessDependencyManagement() {
        return processDependencyManagement;
    }

    @Override
    public boolean getProcessParent() {
        return processParent;
    }

    /**
     * @param pom the pom to update.
     * @throws MojoExecutionException when things go wrong
     * @throws MojoFailureException   when things go wrong in a very bad way
     * @throws XMLStreamException     when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(MutableXMLStreamReader)
     * @since 1.0-alpha-1
     */
    protected void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        update(
                pom,
                getHelper()
                        .getVersionPropertiesMap(VersionsHelper.VersionPropertiesMapRequest.builder()
                                .withMavenProject(getProject())
                                .withPropertyDefinitions(properties)
                                .withIncludeProperties(includeProperties)
                                .withExcludeProperties(excludeProperties)
                                .withAutoLinkItems(autoLinkItems)
                                .withIncludeParent(includeParent)
                                .withIncludeFilter(this::isIncluded)
                                .build()));
    }
}
