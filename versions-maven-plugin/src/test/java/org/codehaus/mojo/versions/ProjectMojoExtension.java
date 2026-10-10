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

import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.maven.api.plugin.testing.MojoExtension;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.codehaus.plexus.PlexusContainer;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * {@code MojoExtension} gives the mojo an empty project, so the POM's dependencies, parent and properties are
 * missing, including properties that set mojo parameters. This loads the project built from a copy of the POM's
 * directory into it before the mojo is configured; goals that rewrite the POM change the copy.
 */
class ProjectMojoExtension extends MojoExtension {

    private static final ExtensionContext.Namespace OWN = ExtensionContext.Namespace.create(ProjectMojoExtension.class);

    @Override
    protected Mojo lookupMojo(
            ExtensionContext extensionContext,
            String[] coord,
            Xpp3Dom pluginConfiguration,
            PluginDescriptor descriptor,
            Path pomPath)
            throws Exception {
        if (pomPath == null) {
            return super.lookupMojo(extensionContext, coord, pluginConfiguration, descriptor, null);
        }
        Path workDir = Files.createTempDirectory("project-mojo-test");
        extensionContext.getStore(OWN).put(workDir, (AutoCloseable) () -> TestUtils.tearDownTempDir(workDir));
        TestUtils.copyDir(pomPath.getParent(), workDir);
        Path pom = workDir.resolve(pomPath.getFileName().toString());

        PlexusContainer container = getContainer(extensionContext);
        ProjectBuildingRequest request = new DefaultMavenExecutionRequest().getProjectBuildingRequest();
        request.setRepositorySession(new DefaultRepositorySystemSession());
        MavenProject built = container
                .lookup(ProjectBuilder.class)
                .build(pom.toFile(), request)
                .getProject();

        MavenProject project = container.lookup(MavenProject.class);
        project.setModel(built.getModel());
        project.setOriginalModel(built.getOriginalModel());
        project.setFile(built.getFile());
        project.setParent(built.getParent());
        project.setParentFile(built.getParentFile());
        project.setArtifact(built.getArtifact());

        return super.lookupMojo(extensionContext, coord, pluginConfiguration, descriptor, pom);
    }
}
