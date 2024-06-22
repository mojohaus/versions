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

import java.io.IOException;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.project.MavenProject;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.eclipse.aether.RepositorySystem;

/**
 * Replaces any versions with the corresponding version from the reactor.
 *
 * @author Stephen Connolly
 * @since 2.2
 */
@Mojo(name = "use-reactor", threadSafe = true)
public class UseReactorMojo extends AbstractVersionsDependencyUpdaterMojo {

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseReactorMojo(
            ArtifactHandlerManager artifactHandlerManager,
            RepositorySystem repositorySystem,
            Map<String, Wagon> wagonMap,
            Map<String, ChangeRecorder> changeRecorders) {
        super(artifactHandlerManager, repositorySystem, wagonMap, changeRecorders);
    }

    /**
     * @param pom the pom to update.
     * @throws org.apache.maven.plugin.MojoExecutionException when things go wrong
     * @throws org.apache.maven.plugin.MojoFailureException   when things go wrong in a very bad way
     * @throws javax.xml.stream.XMLStreamException            when things go wrong with XML streaming
     * @see AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        try {
            if (isProcessingParent() && getProject().hasParent()) {
                useReactor(pom, getProject().getParent());
            }
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    useReactor(pom, dependencyManagement.getDependencies());
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                useReactor(pom, getProject().getDependencies());
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void useReactor(ModifiedPomXMLEventReader pom, Collection<Dependency> dependencies)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {

        for (Dependency dep : dependencies) {
            Artifact artifact = this.toArtifact(dep);
            if (!isIncluded(artifact)) {
                continue;
            }

            for (MavenProject reactorProject : reactorProjects) {
                MavenProject project = reactorProject;
                if (StringUtils.equals(project.getGroupId(), dep.getGroupId())
                        && StringUtils.equals(project.getArtifactId(), dep.getArtifactId())
                        && !StringUtils.equals(project.getVersion(), dep.getVersion())) {
                    if (PomHelper.setDependencyVersion(
                            pom,
                            dep.getGroupId(),
                            dep.getArtifactId(),
                            dep.getVersion(),
                            project.getVersion(),
                            getProject().getModel())) {
                        getLog().info("Updated " + toString(dep) + " to version " + project.getVersion());
                    }
                    break;
                }
            }
        }
    }

    private void useReactor(ModifiedPomXMLEventReader pom, MavenProject parent)
            throws XMLStreamException, VersionRetrievalException {
        for (MavenProject project : reactorProjects) {
            if (StringUtils.equals(project.getGroupId(), parent.getGroupId())
                    && StringUtils.equals(project.getArtifactId(), parent.getArtifactId())
                    && !StringUtils.equals(project.getVersion(), parent.getVersion())) {
                if (PomHelper.setProjectParentVersion(pom, project.getVersion())) {
                    getLog().info("Updated parent " + toString(parent) + " to version " + project.getVersion());
                }
            }
        }
    }
}
