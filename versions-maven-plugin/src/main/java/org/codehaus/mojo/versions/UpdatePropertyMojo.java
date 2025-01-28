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

import java.util.Map;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.Property;
import org.codehaus.mojo.versions.api.VersionsHelper;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

/**
 * Sets a property to the latest version in a given range of associated artifacts.
 *
 * @author Eric Pabst
 * @since 1.3
 */
@Mojo(name = "update-property", threadSafe = true)
public class UpdatePropertyMojo extends UpdatePropertiesMojoBase {
    /**
     * A property to update.
     *
     * @since 1.3
     */
    @Parameter(property = "property")
    protected String property = null;

    /**
     * The new version to set the property to (can be a version range to find a version within).
     * <ul>
     * <li><code>1.0</code>x >= 1.0. The default Maven meaning for 1.0 is everything (,) but with 1.0 recommended.</li>
     * <li><code>[1.0,2.0)</code> Versions 1.0 (included) to 2.0 (not included)</li>
     * <li><code>[1.0,2.0]</code> Versions 1.0 to 2.0 (both included)</li>
     * <li><code>[1.5,)</code> Versions 1.5 and higher</li>
     * <li><code>(,1.0],[1.2,)</code> Versions up to 1.0 (included) and 1.2 or higher</li>
     * </ul>
     * If you like to define the version to be used exactly you have to use it like this:
     * <code>-DnewVersion=[19.0]</code> otherwise a newer existing version will be used. If you need to downgrade a
     * version you have to define <code>-DallowDowngrade=true</code> as well otherwise
     * the version will be kept.
     *
     * @since 1.3
     */
    @Parameter(property = "newVersion")
    private String newVersion = null;

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

    @Inject
    public UpdatePropertyMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorders);
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
                                .withPropertyDefinitions(new Property[] {
                                    new Property(property) {
                                        {
                                            setVersion(newVersion);
                                        }
                                    }
                                })
                                .withIncludeProperties(property)
                                .withAutoLinkItems(autoLinkItems)
                                .withIncludeParent(includeParent)
                                .build()));
    }
}
