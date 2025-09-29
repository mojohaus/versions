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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.model.Model;
import org.apache.maven.model.Parent;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.recording.VersionChangeRecorderFactory;
import org.codehaus.mojo.versions.rewriting.MutableXMLStreamReader;
import org.codehaus.mojo.versions.utils.ArtifactFactory;
import org.eclipse.aether.RepositorySystem;

/**
 * Scans the current projects child modules, updating the versions of any which use the current project to the version
 * of the current project.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-2
 */
@Mojo(name = "update-child-modules", aggregator = true, threadSafe = true)
public class UpdateChildModulesMojo extends AbstractVersionsUpdaterMojo {
    /**
     * The groupId that we are updating. Guarded by this.
     */
    private transient String sourceGroupId = null;

    /**
     * The artifactId that we are updating. Guarded by this.
     */
    private transient String sourceArtifactId = null;

    /**
     * The version that we are updating to. Guarded by this.
     */
    private transient String sourceVersion = null;

    /**
     * Whether to allow snapshots when searching for the latest version of an artifact.
     *
     * @since 1.0-alpha-1
     */
    @Parameter(property = "allowSnapshots", defaultValue = "false")
    protected boolean allowSnapshots;

    /**
     * Creates a new instance.
     *
     * @param artifactFactory  the artifact factory
     * @param repositorySystem the repository system
     * @param wagonMap         the map of wagon implementations
     * @param changeRecorders  the change recorders
     * @throws MojoExecutionException when things go wrong
     */
    @Inject
    public UpdateChildModulesMojo(
            ArtifactFactory artifactFactory,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, VersionChangeRecorderFactory> changeRecorderFactories)
            throws MojoExecutionException {
        super(artifactFactory, repositorySystem, wagonMap, changeRecorderFactories);
    }

    @Override
    protected boolean getAllowSnapshots() {
        return allowSnapshots;
    }

    /**
     * Called when this mojo is executed.
     *
     * @throws MojoExecutionException when things go wrong.
     * @throws MojoFailureException   when things go wrong.
     */
    public void execute() throws MojoExecutionException, MojoFailureException {

        boolean didSomething = false;

        try {
            final Map<File, Model> reactor = PomHelper.getChildModels(getProject(), getLog());
            List<File> order = new ArrayList<>(reactor.keySet());
            order.sort((o1, o2) -> {
                Model m1 = reactor.get(o1);
                Model m2 = reactor.get(o2);
                int d1 = PomHelper.getReactorParentCount(reactor, m1);
                int d2 = PomHelper.getReactorParentCount(reactor, m2);
                if (d1 < d2) {
                    return -1;
                } else if (d1 > d2) {
                    return 1;
                }
                return 0;
            });

            for (File sourcePath : order) {
                Model sourceModel = reactor.get(sourcePath);

                getLog().debug(
                                sourcePath.length() == 0
                                        ? "Processing root module as parent"
                                        : "Processing " + sourcePath + " as a parent.");

                synchronized (this) {
                    sourceGroupId = PomHelper.getGroupId(sourceModel);
                    if (sourceGroupId == null) {
                        getLog().warn("Module " + sourcePath + " is missing a groupId.");
                        continue;
                    }
                    sourceArtifactId = PomHelper.getArtifactId(sourceModel);
                    if (sourceArtifactId == null) {
                        getLog().warn("Module " + sourcePath + " is missing an artifactId.");
                        continue;
                    }
                    sourceVersion = PomHelper.getVersion(sourceModel);
                    if (sourceVersion == null) {
                        getLog().warn("Module " + sourcePath + " is missing a version.");
                        continue;
                    }

                    getLog().debug("Looking for modules which use "
                            + ArtifactUtils.versionlessKey(sourceGroupId, sourceArtifactId)
                            + " as their parent to update it to " + sourceVersion);

                    for (Map.Entry<File, Model> target : PomHelper.getChildModels(
                                    reactor, sourceGroupId, sourceArtifactId)
                            .entrySet()) {
                        File moduleProjectFile = target.getKey();
                        String moduleName = moduleProjectFile.getParent();

                        Model targetModel = target.getValue();
                        final Parent parent = targetModel.getParent();
                        if (sourceVersion.equals(parent.getVersion())) {
                            getLog().debug("Module: " + moduleName + " parent is "
                                    + ArtifactUtils.versionlessKey(sourceGroupId, sourceArtifactId) + ":"
                                    + sourceVersion);
                        } else {
                            getLog().info("Module: " + moduleName);
                            getLog().info("    parent was "
                                    + ArtifactUtils.versionlessKey(sourceGroupId, sourceArtifactId) + ":"
                                    + parent.getVersion());
                            getLog().info("    updated to "
                                    + ArtifactUtils.versionlessKey(sourceGroupId, sourceArtifactId) + ":"
                                    + sourceVersion);
                            process(moduleProjectFile);
                            didSomething = true;
                        }
                    }
                }
            }

        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
        if (!didSomething) {
            getLog().info("All child modules are up to date.");
        }
    }

    /**
     * Updates the pom file.
     *
     * @param pom The pom file to update.
     * @throws MojoExecutionException when things go wrong.
     * @throws MojoFailureException   when things go wrong.
     * @throws XMLStreamException     when things go wrong.
     */
    protected synchronized void update(MutableXMLStreamReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException {
        getLog().debug("Updating parent to " + sourceVersion);

        if (PomHelper.setProjectParentVersion(pom, sourceVersion)) {
            getLog().debug("Made an update to " + sourceVersion);
        }
    }
}
