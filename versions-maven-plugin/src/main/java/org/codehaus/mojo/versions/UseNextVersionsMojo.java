package org.codehaus.mojo.versions;

/*
 * Copyright MojoHaus and Contributors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import javax.inject.Inject;
import javax.xml.stream.XMLStreamException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

import org.apache.maven.artifact.handler.manager.ArtifactHandlerManager;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.wagon.Wagon;
import org.codehaus.mojo.versions.api.PomHelper;
import org.codehaus.mojo.versions.api.VersionRetrievalException;
import org.codehaus.mojo.versions.api.recording.ChangeRecorder;
import org.codehaus.mojo.versions.api.recording.DependencyChangeRecord;
import org.codehaus.mojo.versions.ordering.InvalidSegmentException;
import org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader;
import org.eclipse.aether.RepositorySystem;

import static java.util.Collections.singletonList;
import static org.codehaus.mojo.versions.api.recording.DependencyChangeRecord.ChangeKind.DEPENDENCY;
import static org.codehaus.mojo.versions.api.recording.DependencyChangeRecord.ChangeKind.DEPENDENCY_MANAGEMENT;
import static org.codehaus.mojo.versions.api.recording.DependencyChangeRecord.ChangeKind.PARENT;

/**
 * Replaces any version with the latest version.
 *
 * @author Stephen Connolly
 * @since 1.0-alpha-3
 */
@Mojo(name = "use-next-versions", threadSafe = true)
public class UseNextVersionsMojo extends UseLatestVersionsMojoBase {

    /**
     * <p>Whether to downgrade a snapshot dependency if <code>allowSnapshots</code> is <code>false</code>
     * and there exists a non-snapshot version within the range fulfilling the criteria.</p>
     * <p>Only valid if <code>allowSnapshots</code> is <code>false</code>.</p>
     *
     * @since 2.15.0
     */
    @Parameter(property = "allowDowngrade", defaultValue = "false")
    protected boolean allowDowngrade;

    // ------------------------------ METHODS --------------------------

    @Inject
    public UseNextVersionsMojo(
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
     * @see org.codehaus.mojo.versions.AbstractVersionsUpdaterMojo#update(org.codehaus.mojo.versions.rewriting.ModifiedPomXMLEventReader)
     */
    protected void update(ModifiedPomXMLEventReader pom)
            throws MojoExecutionException, MojoFailureException, XMLStreamException, VersionRetrievalException {
        try {
            if (isProcessingDependencyManagement()) {
                DependencyManagement dependencyManagement =
                        PomHelper.getRawModel(getProject()).getDependencyManagement();
                if (dependencyManagement != null) {
                    useNextVersions(pom, dependencyManagement.getDependencies(), DEPENDENCY_MANAGEMENT);
                }
            }
            if (getProject().getDependencies() != null && isProcessingDependencies()) {
                useNextVersions(pom, getProject().getDependencies(), DEPENDENCY);
            }
            if (getProject().getParent() != null && isProcessingParent()) {
                useNextVersions(pom, singletonList(getParentDependency()), PARENT);
            }
        } catch (IOException e) {
            throw new MojoExecutionException(e.getMessage(), e);
        }
    }

    private void useNextVersions(
            ModifiedPomXMLEventReader pom,
            Collection<Dependency> dependencies,
            DependencyChangeRecord.ChangeKind changeKind)
            throws XMLStreamException, MojoExecutionException, VersionRetrievalException {
        useLatestVersions(
                pom,
                dependencies,
                (dep, versions) -> {
                    try {
                        return Arrays.stream(versions.getNewerVersions(
                                        dep.getVersion(), Optional.empty(), allowSnapshots, allowDowngrade))
                                .findFirst();
                    } catch (InvalidSegmentException e) {
                        throw new RuntimeException(e);
                    }
                },
                changeKind);
    }
}
