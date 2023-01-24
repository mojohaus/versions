package org.codehaus.mojo.versions.utils;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.doxia.tools.SiteTool;
import org.apache.maven.doxia.tools.SiteToolException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.project.MavenProject;
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.plexus.i18n.I18N;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.version.Version;

import static java.util.Collections.emptyList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Various mock creating utilities
 */
public class MockUtils {
    private static final Map<String, String[]> DEFAULT_VERSION_MAP = new HashMap<String, String[]>() {
        {
            put("artifactA", new String[] {"1.0.0", "2.0.0"});
            put("artifactB", new String[] {"1.0.0", "1.1.0"});
            put("artifactC", new String[] {"1.0.0"});
        }
    };

    /**
     * Creates a mocked  {@linkplain org.eclipse.aether.RepositorySystem}, providing the default version set
     * @return mocked {@linkplain org.eclipse.aether.RepositorySystem}
     */
    public static org.eclipse.aether.RepositorySystem mockAetherRepositorySystem() {
        return mockAetherRepositorySystem(DEFAULT_VERSION_MAP);
    }

    /**
     * Creates a mocked  {@linkplain org.eclipse.aether.RepositorySystem}, providing the version map given in
     * the argument.
     * @param versionMap requested version map
     * @return mocked {@linkplain org.eclipse.aether.RepositorySystem}
     */
    public static org.eclipse.aether.RepositorySystem mockAetherRepositorySystem(Map<String, String[]> versionMap) {
        org.eclipse.aether.RepositorySystem repositorySystem = mock(org.eclipse.aether.RepositorySystem.class);
        try {
            when(repositorySystem.resolveVersionRange(any(), any(VersionRangeRequest.class)))
                    .then(invocation -> {
                        VersionRangeRequest request = invocation.getArgument(1);
                        return versionMap.entrySet().stream()
                                .filter(e ->
                                        e.getKey().equals(request.getArtifact().getArtifactId()))
                                .findAny()
                                .map(e -> Arrays.stream(e.getValue())
                                        .map(VersionStub::new)
                                        .collect(() -> new ArrayList<Version>(), ArrayList::add, ArrayList::addAll))
                                .map(versions -> new VersionRangeResult(request).setVersions(versions))
                                .orElse(null); // should tell us if we haven't populated all cases in the test
                    });
            when(repositorySystem.resolveArtifact(any(RepositorySystemSession.class), any(ArtifactRequest.class)))
                    .then(invocation -> {
                        ArtifactRequest request = invocation.getArgument(1);
                        org.eclipse.aether.artifact.Artifact copiedArtifact =
                                new org.eclipse.aether.artifact.DefaultArtifact(
                                        request.getArtifact().getGroupId(),
                                        request.getArtifact().getArtifactId(),
                                        request.getArtifact().getClassifier(),
                                        request.getArtifact().getExtension(),
                                        request.getArtifact().getVersion());
                        copiedArtifact.setFile(mock(File.class));
                        return new ArtifactResult(request).setArtifact(copiedArtifact);
                    });
        } catch (VersionRangeResolutionException | ArtifactResolutionException e) {
            throw new RuntimeException(e);
        }

        return repositorySystem;
    }

    public static I18N mockI18N() {
        I18N i18n = mock(I18N.class);
        when(i18n.getString(anyString(), any(), anyString())).thenAnswer(invocation -> invocation.getArgument(2));
        return i18n;
    }

    public static SiteTool mockSiteTool() {
        Artifact skinArtifact = mock(Artifact.class);
        when(skinArtifact.getId()).thenReturn("");
        SiteTool siteTool = mock(SiteTool.class);
        try {
            when(siteTool.getSkinArtifactFromRepository(any(), any(), any())).thenReturn(skinArtifact);
        } catch (SiteToolException e) {
            throw new RuntimeException(e);
        }
        return siteTool;
    }

    public static RepositorySystem mockRepositorySystem() {
        RepositorySystem repositorySystem = mock(RepositorySystem.class);
        when(repositorySystem.createDependencyArtifact(any(Dependency.class))).thenAnswer(invocation -> {
            Dependency dependency = invocation.getArgument(0);
            return new DefaultArtifact(
                    dependency.getGroupId(),
                    dependency.getArtifactId(),
                    dependency.getVersion(),
                    dependency.getScope(),
                    dependency.getType(),
                    dependency.getClassifier(),
                    new DefaultArtifactHandlerStub("default"));
        });
        return repositorySystem;
    }

    /**
     * Creates a very simple mock of {@link MavenSession}
     * by providing only a non-{@code null} implementation of its {@link MavenSession#getRepositorySession()} method.
     * @return mocked {@link MavenSession}
     */
    public static MavenSession mockMavenSession() {
        MavenProject project = mock(MavenProject.class);
        when(project.getRemotePluginRepositories()).thenReturn(emptyList());
        when(project.getRemoteProjectRepositories()).thenReturn(emptyList());
        return mockMavenSession(project);
    }

    /**
     * Creates a very simple mock of {@link MavenSession}
     * by providing only a non-{@code null} implementation of its {@link MavenSession#getRepositorySession()} method.
     * @param project {@link MavenProject} to link to
     * @return mocked {@link MavenSession}
     */
    public static MavenSession mockMavenSession(MavenProject project) {
        MavenSession session = mock(MavenSession.class);
        when(session.getRepositorySession()).thenReturn(mock(RepositorySystemSession.class));
        when(session.getCurrentProject()).thenReturn(project);
        return session;
    }
}
