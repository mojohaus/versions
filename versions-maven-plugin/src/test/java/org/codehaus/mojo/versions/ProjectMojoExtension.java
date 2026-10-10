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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.maven.api.plugin.testing.MojoExtension;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.settings.Settings;
import org.codehaus.mojo.versions.utils.TestUtils;
import org.codehaus.plexus.util.xml.XmlStreamReader;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;

import static org.mockito.Mockito.lenient;

/**
 * {@code MojoExtension} gives the mojo an empty project, so the POM's dependencies, parent and properties are
 * missing, including properties that set mojo parameters. This loads the project built from a copy of the POM's
 * directory into it before the mojo is configured; goals that rewrite the POM change the copy.
 */
class ProjectMojoExtension extends MojoExtension {

    private static final ExtensionContext.Namespace HARNESS = ExtensionContext.Namespace.create("MojoExtension");

    private static final ExtensionContext.Namespace OWN = ExtensionContext.Namespace.create(ProjectMojoExtension.class);

    @Override
    public boolean supportsParameter(ParameterContext parameterContext, ExtensionContext extensionContext) {
        return parameterContext.getParameter().getType() == ProjectMojos.class
                || super.supportsParameter(parameterContext, extensionContext);
    }

    @Override
    public Object resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext) {
        if (parameterContext.getParameter().getType() != ProjectMojos.class) {
            return super.resolveParameter(parameterContext, extensionContext);
        }
        return new ProjectMojos() {
            @Override
            public <T extends Mojo> T lookup(String goal, Path pom) throws Exception {
                Path pomFile = Files.isDirectory(pom) ? pom.resolve("pom.xml") : pom;
                MavenProject built = build(extensionContext, pomFile);
                return lookupBuilt(extensionContext, goal, pomFile, built, null, null);
            }

            @Override
            public <T extends Mojo> T lookupInReactor(String goal, Path baseDir, String... modules) throws Exception {
                List<MavenProject> reactor = new ArrayList<>();
                for (String module : modules) {
                    reactor.add(build(extensionContext, baseDir.resolve(module).resolve("pom.xml")));
                }
                List<MavenProject> all = new ArrayList<>();
                if (Files.exists(baseDir.resolve("pom.xml"))) {
                    all.add(build(extensionContext, baseDir.resolve("pom.xml")));
                }
                Path pomFile = reactor.get(0).getFile().toPath();
                return lookupBuilt(extensionContext, goal, pomFile, reactor.get(0), reactor, all);
            }
        };
    }

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
        MavenProject project = load(extensionContext, build(extensionContext, pom));
        return super.lookupMojo(
                extensionContext, coord, withInherited(pluginConfiguration, project, coord), descriptor, pom);
    }

    /**
     * Configures the mojo of {@code goal} from {@code pom}, working on {@code current}; with a reactor, the
     * session's projects are {@code reactor} and all its projects are {@code roots} followed by {@code reactor}.
     */
    @SuppressWarnings("unchecked")
    private <T extends Mojo> T lookupBuilt(
            ExtensionContext context,
            String goal,
            Path pom,
            MavenProject current,
            List<MavenProject> reactor,
            List<MavenProject> roots)
            throws Exception {
        MavenProject project = load(context, current);
        if (reactor != null) {
            List<MavenProject> projects = new ArrayList<>(reactor);
            projects.set(0, project);
            List<MavenProject> all = new ArrayList<>(roots);
            all.addAll(projects);
            MavenSession session = getContainer(context).lookup(MavenSession.class);
            lenient().doReturn(projects).when(session).getProjects();
            lenient().doReturn(all).when(session).getAllProjects();
        }
        PluginDescriptor descriptor = context.getStore(HARNESS).get(PluginDescriptor.class, PluginDescriptor.class);
        String[] coord = mojoCoordinates(goal, descriptor);
        Xpp3Dom pomDom = Xpp3DomBuilder.build(new XmlStreamReader(pom.toFile()));
        Xpp3Dom configuration = withInherited(extractPluginConfiguration(coord[1], pomDom), project, coord);
        return (T) super.lookupMojo(context, coord, configuration, descriptor, pom);
    }

    /**
     * The configuration from the POM, over the plugin's configuration in the built project, which adds what the POM
     * inherits from its parent or plugin management, as a build would.
     */
    private static Xpp3Dom withInherited(Xpp3Dom fromPom, MavenProject project, String[] coord) {
        Plugin plugin = project.getPlugin(coord[0] + ":" + coord[1]);
        if (plugin == null || plugin.getConfiguration() == null) {
            return fromPom;
        }
        return Xpp3Dom.mergeXpp3Dom(fromPom, new Xpp3Dom((Xpp3Dom) plugin.getConfiguration()));
    }

    private MavenProject build(ExtensionContext context, Path pom) throws Exception {
        ProjectBuildingRequest request = new DefaultMavenExecutionRequest().getProjectBuildingRequest();
        request.setRepositorySession(new DefaultRepositorySystemSession());
        return getContainer(context)
                .lookup(ProjectBuilder.class)
                .build(pom.toFile(), request)
                .getProject();
    }

    /** Loads {@code built} into the project the harness gives the mojo, and returns that project. */
    private MavenProject load(ExtensionContext context, MavenProject built) throws Exception {
        MavenProject project = getContainer(context).lookup(MavenProject.class);
        project.setModel(built.getModel());
        project.setOriginalModel(built.getOriginalModel());
        project.setFile(built.getFile());
        project.setParent(built.getParent());
        project.setParentFile(built.getParentFile());
        project.setArtifact(built.getArtifact());
        MavenSession session = getContainer(context).lookup(MavenSession.class);
        if (session.getSettings() == null) {
            lenient().doReturn(new Settings()).when(session).getSettings();
        }
        // a build of this project alone; lookupInReactor replaces both lists
        List<MavenProject> projects = Collections.singletonList(project);
        lenient().doReturn(projects).when(session).getProjects();
        lenient().doReturn(projects).when(session).getAllProjects();
        return project;
    }
}
