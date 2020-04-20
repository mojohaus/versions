package org.codehaus.mojo.versions.api;

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

import com.google.common.collect.Lists;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.metadata.ArtifactMetadataRetrievalException;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.repository.ArtifactRepositoryPolicy;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.plugin.logging.Log;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.ArtifactProperties;
import org.eclipse.aether.artifact.ArtifactType;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.artifact.DefaultArtifactType;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.repository.RepositoryPolicy;
import org.eclipse.aether.resolution.VersionRangeRequest;
import org.eclipse.aether.resolution.VersionRangeResolutionException;
import org.eclipse.aether.resolution.VersionRangeResult;
import org.eclipse.aether.version.Version;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Created on 20. 04. 20
 *
 * @author e.roznik
 */
public class VersionsProvider {
    private final Log log;
    private final RepositorySystemSession repositorySystemSession;
    private final RepositorySystem repositorySystem;
    private final ArtifactMetadataSource artifactMetadataSource;
    private final boolean useRepositorySystem;

    public VersionsProvider(Log log, RepositorySystemSession repositorySystemSession,
                            RepositorySystem repositorySystem, ArtifactMetadataSource artifactMetadataSource,
                            boolean useRepositorySystem) {
        this.log = log;
        this.repositorySystemSession = repositorySystemSession;
        this.repositorySystem = repositorySystem;
        this.artifactMetadataSource = artifactMetadataSource;
        this.useRepositorySystem = useRepositorySystem;
    }

    public List<ArtifactVersion> fetchArtifactVersions(Artifact artifact, ArtifactRepository localRepository, List remoteRepositories) throws ArtifactMetadataRetrievalException {
        if (useRepositorySystem) {
            return fetchRepositorySystem(artifact, localRepository, remoteRepositories);
        }

        return artifactMetadataSource.retrieveAvailableVersions(artifact, localRepository, remoteRepositories);
    }

    private List<ArtifactVersion> fetchRepositorySystem(Artifact artifact, ArtifactRepository localRepository, List remoteRepositories) throws ArtifactMetadataRetrievalException {
        org.eclipse.aether.artifact.Artifact aetherArtifact =
                toArtifact(artifact).setVersion("[,)");

        List repositoriesToUse = new ArrayList(remoteRepositories);
        repositoriesToUse.add(localRepository);

        VersionRangeRequest request = new VersionRangeRequest(
                aetherArtifact,
                toRepos(repositoriesToUse),
                null);

        VersionRangeResult versionsRangeResult;
        try {
            versionsRangeResult = repositorySystem.resolveVersionRange(repositorySystemSession, request);
        } catch (VersionRangeResolutionException e) {
            throw new ArtifactMetadataRetrievalException("Version resolution failed", e);
        }

        List<Version> versions = versionsRangeResult.getVersions();

        if (log.isDebugEnabled()) {
            String desc = versions == null ? "null" : Arrays.toString(versions.toArray());
            log.debug(String.format("Available versions for %s => %s", aetherArtifact.getArtifactId(), desc));
        }

        if (versions == null) {
            log.warn("Versions result was null for " + aetherArtifact.getArtifactId());
            return Collections.emptyList();
        }

        List<ArtifactVersion> result = Lists.<ArtifactVersion>newArrayListWithExpectedSize(versions.size());
        for (Version version : versions) {
            result.add(new DefaultArtifactVersion(version.toString()));
        }
        return result;
    }

    private static org.eclipse.aether.artifact.Artifact toArtifact(Artifact artifact) {
        if (artifact == null) {
            return null;
        }

        String version = artifact.getVersion();
        if (version == null && artifact.getVersionRange() != null) {
            version = artifact.getVersionRange().toString();
        }

        Map<String, String> props = null;
        if (Artifact.SCOPE_SYSTEM.equals(artifact.getScope())) {
            String localPath = (artifact.getFile() != null) ? artifact.getFile().getPath() : "";
            props = Collections.singletonMap(ArtifactProperties.LOCAL_PATH, localPath);
        }

        org.eclipse.aether.artifact.Artifact result =
                new DefaultArtifact(artifact.getGroupId(), artifact.getArtifactId(), artifact.getClassifier(),
                        artifact.getArtifactHandler().getExtension(), version, props,
                        newArtifactType(artifact.getType(), artifact.getArtifactHandler()));
        result = result.setFile(artifact.getFile());

        return result;
    }

    public static ArtifactType newArtifactType(String id, ArtifactHandler handler) {
        return new DefaultArtifactType(id, handler.getExtension(), handler.getClassifier(), handler.getLanguage(),
                handler.isAddedToClasspath(), handler.isIncludesDependencies());
    }

    public static List<RemoteRepository> toRepos(List<ArtifactRepository> repos) {
        if (repos == null) {
            return null;
        }

        List<RemoteRepository> results = new ArrayList<>(repos.size());
        for (ArtifactRepository repo : repos) {
            results.add(toRepo(repo));
        }
        return results;
    }

    public static RemoteRepository toRepo(ArtifactRepository repo) {
        RemoteRepository result = null;
        if (repo != null) {
            RemoteRepository.Builder builder =
                    new RemoteRepository.Builder(repo.getId(), getLayout(repo), repo.getUrl());
            builder.setSnapshotPolicy(toPolicy(repo.getSnapshots()));
            builder.setReleasePolicy(toPolicy(repo.getReleases()));
            result = builder.build();
        }
        return result;
    }

    public static String getLayout(ArtifactRepository repo) {
        String className = repo.getLayout().getClass().getSimpleName();
        if (className.endsWith("RepositoryLayout")) {
            String layout = className.substring(0, className.length() - "RepositoryLayout".length());
            if (layout.length() > 0) {
                layout = Character.toLowerCase(layout.charAt(0)) + layout.substring(1);
                return layout;
            }
        }
        return "";
    }

    private static RepositoryPolicy toPolicy(ArtifactRepositoryPolicy policy) {
        RepositoryPolicy result = null;
        if (policy != null) {
            result = new RepositoryPolicy(policy.isEnabled(), policy.getUpdatePolicy(), policy.getChecksumPolicy());
        }
        return result;
    }
}
